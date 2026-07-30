#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_ptpx.sh \
    --netlist FILE --sdc FILE --saif FILE \
    --sram-root DIR --clock-period-ns NS [options]

Required:
  --netlist FILE          Same mapped netlist used by VCS
  --sdc FILE              Mapped SDC from the same DC run
  --saif FILE             activity.saif from a PASS VCS run
  --sram-root DIR         Root containing RSPHVT*/CCS/*.db
  --clock-period-ns NS    Must equal the DC and VCS clock period

Options:
  --target-db FILE        Standard-cell CCS DB; repeat as needed
  --std-cell-ccs-root DIR Auto-find the 22nm HVT/SVT/LVT tt0p9v25c DBs
  --mem-corner NAME       Default: tt_typical_0p90v_0p90v_25c
  --run-root DIR          Output root; default: ./runs
  --tag TAG               Run tag; default: timestamp_tt0p9v25c
  --pt-cores N            PrimeTime cores; default: 4
  -h, --help              Show help
EOF
}

die() {
  echo "[toomcook-ptpx] ERROR: $*" >&2
  exit 1
}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TCL_TEMPLATE="$SCRIPT_DIR/toomcook1024_ptpx.tcl"

NETLIST_FILE=""
SDC_FILE=""
SAIF_FILE=""
SRAM_ROOT=""
CLOCK_PERIOD_NS=""
STD_CELL_CCS_ROOT="${STD_CELL22_CCS_ROOT:-/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/timing_power_noise/CCS}"
TARGET_DBS=()
MEM_CORNER=tt_typical_0p90v_0p90v_25c
RUN_ROOT="$SCRIPT_DIR/runs"
RUN_TAG="$(date +'%Y%m%d_%H%M%S')_tt0p9v25c"
PT_CORES=4

while (($# > 0)); do
  case "$1" in
    --netlist) NETLIST_FILE=$2; shift 2 ;;
    --sdc) SDC_FILE=$2; shift 2 ;;
    --saif) SAIF_FILE=$2; shift 2 ;;
    --sram-root) SRAM_ROOT=$2; shift 2 ;;
    --clock-period-ns) CLOCK_PERIOD_NS=$2; shift 2 ;;
    --target-db) TARGET_DBS+=("$2"); shift 2 ;;
    --std-cell-ccs-root) STD_CELL_CCS_ROOT=$2; shift 2 ;;
    --mem-corner) MEM_CORNER=$2; shift 2 ;;
    --run-root) RUN_ROOT=$2; shift 2 ;;
    --tag) RUN_TAG=$2; shift 2 ;;
    --pt-cores) PT_CORES=$2; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    --*) die "unknown option: $1" ;;
    *) die "unexpected positional argument: $1" ;;
  esac
done

[[ -s "$NETLIST_FILE" ]] || die "mapped netlist missing or empty: $NETLIST_FILE"
[[ -s "$SDC_FILE" ]] || die "mapped SDC missing or empty: $SDC_FILE"
[[ -s "$SAIF_FILE" ]] || die "SAIF missing or empty: $SAIF_FILE"
[[ -d "$SRAM_ROOT" ]] || die "SRAM root not found: $SRAM_ROOT"
[[ -n "$CLOCK_PERIOD_NS" ]] || die "--clock-period-ns is required"
[[ "$PT_CORES" =~ ^[1-9][0-9]*$ ]] || die "--pt-cores must be a positive integer"
[[ -s "$TCL_TEMPLATE" ]] || die "PTPX Tcl template missing: $TCL_TEMPLATE"
command -v pt_shell >/dev/null 2>&1 || die "pt_shell not found in PATH"

grep -Fq "(INSTANCE Tb" "$SAIF_FILE" || die "SAIF has no Tb scope"
grep -Fq "(INSTANCE u_ToomCook1024" "$SAIF_FILE" || \
  die "SAIF has no Tb/u_ToomCook1024 scope"

if ((${#TARGET_DBS[@]} == 0)); then
  HVT_DIR="$STD_CELL_CCS_ROOT/tcbn22ullbwp7t40p140hvt_110b"
  SVT_DIR="$STD_CELL_CCS_ROOT/tcbn22ullbwp7t40p140_110b"
  LVT_DIR="$STD_CELL_CCS_ROOT/tcbn22ullbwp7t40p140lvt_110b"
  TARGET_DBS=(
    "$HVT_DIR/tcbn22ullbwp7t40p140hvttt0p9v25c_ccs.db"
    "$SVT_DIR/tcbn22ullbwp7t40p140tt0p9v25c_ccs.db"
    "$LVT_DIR/tcbn22ullbwp7t40p140lvttt0p9v25c_ccs.db"
  )
fi
for db in "${TARGET_DBS[@]}"; do
  [[ -s "$db" ]] || die "standard-cell CCS DB missing or empty: $db"
done

mapfile -t USED_MACROS < <(
  (grep -oE 'RSPHVT[0-9]+X[0-9]+' "$NETLIST_FILE" || true) | sort -u
)
MACRO_DBS=()
for macro in "${USED_MACROS[@]}"; do
  macro_db="$SRAM_ROOT/$macro/CCS/${macro}_${MEM_CORNER}_ccs_tn.db"
  [[ -s "$macro_db" ]] || die "macro CCS DB missing or empty: $macro_db"
  MACRO_DBS+=("$macro_db")
done

RUN_DIR="$RUN_ROOT/$RUN_TAG"
[[ ! -e "$RUN_DIR" ]] || die "run directory already exists: $RUN_DIR"
mkdir -p "$RUN_DIR/rpt"

TARGET_LIBRARY_FILES=$(printf '%s ' "${TARGET_DBS[@]}")
TARGET_LIBRARY_FILES=${TARGET_LIBRARY_FILES% }
LINK_LIBRARY_FILES=""
if ((${#MACRO_DBS[@]} > 0)); then
  LINK_LIBRARY_FILES=$(printf '%s ' "${MACRO_DBS[@]}")
  LINK_LIBRARY_FILES=${LINK_LIBRARY_FILES% }
fi
SEARCH_PATH=$(dirname "${TARGET_DBS[0]}")
for db in "${TARGET_DBS[@]:1}"; do
  SEARCH_PATH+=":$(dirname "$db")"
done
for db in "${MACRO_DBS[@]}"; do
  SEARCH_PATH+=":$(dirname "$db")"
done

export DESIGN_NAME=ToomCook1024
export NETLIST_FILE
export SDC_FILE
export SAIF_FILE
export SAIF_STRIP_PATH=Tb/u_ToomCook1024
export TARGET_LIBRARY_FILES
export LINK_LIBRARY_FILES
export SEARCH_PATH
export CLOCK_PERIOD_NS
export REPORTS_DIR="$RUN_DIR/rpt"
export PTPX_MAX_CORES="$PT_CORES"

{
  echo "design=$DESIGN_NAME"
  echo "netlist=$NETLIST_FILE"
  echo "sdc=$SDC_FILE"
  echo "saif=$SAIF_FILE"
  echo "saif_strip_path=$SAIF_STRIP_PATH"
  echo "clock_period_ns=$CLOCK_PERIOD_NS"
  echo "target_library_files=$TARGET_LIBRARY_FILES"
  echo "link_library_files=$LINK_LIBRARY_FILES"
  echo "search_path=$SEARCH_PATH"
  echo "ptpx_max_cores=$PTPX_MAX_CORES"
} > "$RUN_DIR/ptpx_manifest.txt"

(
  cd "$RUN_DIR"
  set -o pipefail
  pt_shell -f "$TCL_TEMPLATE" 2>&1 | tee ptpx.log
)

[[ -s "$RUN_DIR/ptpx.done" ]] || die "PTPX done marker missing"
POWER_REPORT="$RUN_DIR/rpt/ToomCook1024.power.rpt"
[[ -s "$POWER_REPORT" ]] || die "power report missing or empty: $POWER_REPORT"
grep -q "Total Power" "$POWER_REPORT" || die "power report has no Total Power"

grep -E \
  'Number of annotated nets|Number of fully annotated leaf cells|Total Power' \
  "$RUN_DIR/ptpx.log" "$POWER_REPORT" \
  > "$RUN_DIR/annotation_and_power_summary.txt" || true

echo "[toomcook-ptpx] PASS"
echo "[toomcook-ptpx] run_dir=$RUN_DIR"
echo "[toomcook-ptpx] power=$POWER_REPORT"
echo "[toomcook-ptpx] hierarchy=$RUN_DIR/rpt/ToomCook1024.hier.power.rpt"
