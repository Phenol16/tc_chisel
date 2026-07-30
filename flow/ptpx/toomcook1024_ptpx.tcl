proc require_env {name} {
  if {![info exists ::env($name)] || $::env($name) eq ""} {
    puts stderr "Missing environment variable: $name"
    exit 2
  }
  return $::env($name)
}

set DESIGN_NAME          [require_env DESIGN_NAME]
set NETLIST_FILE         [require_env NETLIST_FILE]
set SDC_FILE             [require_env SDC_FILE]
set SAIF_FILE            [require_env SAIF_FILE]
set SAIF_STRIP_PATH      [require_env SAIF_STRIP_PATH]
set TARGET_LIBRARY_FILES [require_env TARGET_LIBRARY_FILES]
set CLOCK_PERIOD_NS      [require_env CLOCK_PERIOD_NS]
set REPORTS_DIR          [require_env REPORTS_DIR]
set LINK_LIBRARY_FILES   [expr {
  [info exists ::env(LINK_LIBRARY_FILES)] ? $::env(LINK_LIBRARY_FILES) : ""
}]
set SEARCH_PATH          [expr {
  [info exists ::env(SEARCH_PATH)] ? $::env(SEARCH_PATH) : ""
}]
set PTPX_MAX_CORES       [expr {
  [info exists ::env(PTPX_MAX_CORES)] ? $::env(PTPX_MAX_CORES) : 4
}]

file mkdir $REPORTS_DIR
set_host_options -max_cores $PTPX_MAX_CORES
set power_enable_analysis true
set power_enable_multi_rail_analysis true
set power_analysis_mode averaged
set report_default_significant_digits 6
set sh_source_uses_search_path true

if {$SEARCH_PATH ne ""} {
  set search_path [concat $search_path [split $SEARCH_PATH ":"]]
}
set target_library [split $TARGET_LIBRARY_FILES " "]
set extra_link_library [expr {
  $LINK_LIBRARY_FILES eq "" ? [list] : [split $LINK_LIBRARY_FILES " "]
}]
set link_path [concat * $target_library $extra_link_library]

read_verilog $NETLIST_FILE
current_design $DESIGN_NAME
if {![link]} {
  puts stderr "PTPX_ERROR: failed to link $DESIGN_NAME"
  exit 3
}

read_sdc $SDC_FILE
set old_clocks [get_clocks -quiet *]
if {[sizeof_collection $old_clocks] > 0} {
  remove_clock $old_clocks
}
create_clock -name clock_main -period $CLOCK_PERIOD_NS [get_ports clock]

update_timing -full
check_timing -verbose > $REPORTS_DIR/${DESIGN_NAME}.check_timing.rpt
report_global_timing > $REPORTS_DIR/${DESIGN_NAME}.global_timing.rpt
report_clock -skew -attribute > $REPORTS_DIR/${DESIGN_NAME}.clock.rpt
report_analysis_coverage > $REPORTS_DIR/${DESIGN_NAME}.analysis_coverage.rpt
report_timing -slack_lesser_than 0.0 -pba_mode exhaustive \
  -delay min_max -nosplit -input -net \
  > $REPORTS_DIR/${DESIGN_NAME}.timing.pba.rpt
report_constraints -verbose > $REPORTS_DIR/${DESIGN_NAME}.constraints.rpt

read_saif -input $SAIF_FILE -strip_path $SAIF_STRIP_PATH
report_switching_activity \
  > $REPORTS_DIR/${DESIGN_NAME}.switching.rpt
report_switching_activity -list_not_annotated \
  > $REPORTS_DIR/${DESIGN_NAME}.switching.not_annotated.rpt

check_power > $REPORTS_DIR/${DESIGN_NAME}.check_power.rpt
update_power
report_power -verbose > $REPORTS_DIR/${DESIGN_NAME}.power.rpt
report_power -hierarchy > $REPORTS_DIR/${DESIGN_NAME}.hier.power.rpt
report_power -hierarchy -levels 1 \
  > $REPORTS_DIR/${DESIGN_NAME}.hier.power.level1.rpt
report_power -cell_power > $REPORTS_DIR/${DESIGN_NAME}.cell_power.rpt
report_clock_gate_savings \
  > $REPORTS_DIR/${DESIGN_NAME}.clock_gating.rpt
report_power -threshold_voltage_group \
  > $REPORTS_DIR/${DESIGN_NAME}.power.per_lib_leakage.rpt
report_threshold_voltage_group \
  > $REPORTS_DIR/${DESIGN_NAME}.power.per_voltage_threshold.rpt

set marker [open "ptpx.done" "w"]
puts $marker "PASS"
close $marker
exit
