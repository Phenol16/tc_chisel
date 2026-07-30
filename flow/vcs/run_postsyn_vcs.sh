#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_postsyn_vcs.sh \
    --netlist FILE --sram-root DIR \
    --clock-period-ns NS [--std-cell-model FILE ...] [options]

Required:
  --netlist FILE          DC mapped gate-level Verilog
  --sram-root DIR         Root containing RSPHVT*/VERILOG/*.v
  --clock-period-ns NS    Must equal the DC/PTPX clock period

Options:
  --sdf FILE              Enable max-delay SDF annotation; default is zero-delay gate simulation
  --std-cell-model FILE   Standard-cell simulation model; repeat as needed
  --std-cell-root DIR     Auto-find the 22nm HVT/SVT/LVT models under DIR
  --run-root DIR          Output root; default: ./runs
  --tag TAG               Run tag; default: timestamp
  --tasks N               Pipeline tasks; default: 8
  --seed N                Random-vector seed; default: 1
  --enable-timing-checks  Enable library timing checks; valid only with --sdf
  --keep-full-fsdb        Keep both full and activity-window FSDB files
  --discard-full-fsdb     Remove full tb.fsdb after SAIF conversion (default)
  -h, --help              Show help
EOF
}

die() {
  echo "[toomcook-vcs] ERROR: $*" >&2
  exit 1
}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
FLOW_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
TB_GENERATOR="$FLOW_ROOT/scripts/gen_toomcook1024_tb.py"

NETLIST_FILE=""
SDF_FILE=""
SRAM_ROOT=""
CLOCK_PERIOD_NS=""
STD_CELL_ROOT="${STD_CELL22_ROOT:-/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/verilog}"
STD_CELL_MODELS=()
RUN_ROOT="$SCRIPT_DIR/runs"
RUN_TAG=$(date +'%Y%m%d_%H%M%S')
TASKS=8
SEED=1
TIMING_CHECKS=0
KEEP_FULL_FSDB=0

while (($# > 0)); do
  case "$1" in
    --netlist) NETLIST_FILE=$2; shift 2 ;;
    --sdf) SDF_FILE=$2; shift 2 ;;
    --sram-root) SRAM_ROOT=$2; shift 2 ;;
    --clock-period-ns) CLOCK_PERIOD_NS=$2; shift 2 ;;
    --std-cell-model) STD_CELL_MODELS+=("$2"); shift 2 ;;
    --std-cell-root) STD_CELL_ROOT=$2; shift 2 ;;
    --run-root) RUN_ROOT=$2; shift 2 ;;
    --tag) RUN_TAG=$2; shift 2 ;;
    --tasks) TASKS=$2; shift 2 ;;
    --seed) SEED=$2; shift 2 ;;
    --enable-timing-checks) TIMING_CHECKS=1; shift ;;
    --keep-full-fsdb) KEEP_FULL_FSDB=1; shift ;;
    --discard-full-fsdb) KEEP_FULL_FSDB=0; shift ;;
    -h|--help) usage; exit 0 ;;
    --*) die "unknown option: $1" ;;
    *) die "unexpected positional argument: $1" ;;
  esac
done

[[ -s "$NETLIST_FILE" ]] || die "mapped netlist missing or empty: $NETLIST_FILE"
if [[ -n "$SDF_FILE" ]]; then
  [[ -s "$SDF_FILE" ]] || die "mapped SDF missing or empty: $SDF_FILE"
elif ((TIMING_CHECKS == 1)); then
  die "--enable-timing-checks requires --sdf"
fi
[[ -d "$SRAM_ROOT" ]] || die "SRAM root not found: $SRAM_ROOT"
[[ -n "$CLOCK_PERIOD_NS" ]] || die "--clock-period-ns is required"
[[ "$TASKS" =~ ^[0-9]+$ ]] || die "--tasks must be an integer >= 3"
((TASKS >= 3)) || die "--tasks must be an integer >= 3"
[[ "$SEED" =~ ^[0-9]+$ ]] || die "--seed must be a non-negative integer"
[[ -s "$TB_GENERATOR" ]] || die "TB generator missing: $TB_GENERATOR"

for tool in python3 vcs fsdbextract fsdb2saif; do
  command -v "$tool" >/dev/null 2>&1 || die "required tool not found in PATH: $tool"
done

if ((${#STD_CELL_MODELS[@]} == 0)); then
  [[ -d "$STD_CELL_ROOT" ]] || die \
    "standard-cell model root not found: $STD_CELL_ROOT; use --std-cell-model"
  for model_name in \
    tcbn22ullbwp7t40p140hvt.v \
    tcbn22ullbwp7t40p140.v \
    tcbn22ullbwp7t40p140lvt.v
  do
    model_path=$(find "$STD_CELL_ROOT" -type f -name "$model_name" | sort | sed -n '1p')
    [[ -n "$model_path" ]] || die "cannot find $model_name under $STD_CELL_ROOT"
    STD_CELL_MODELS+=("$model_path")
  done
fi
for model_path in "${STD_CELL_MODELS[@]}"; do
  [[ -s "$model_path" ]] || die "standard-cell model missing or empty: $model_path"
done

RUN_DIR="$RUN_ROOT/$RUN_TAG"
[[ ! -e "$RUN_DIR" ]] || die "run directory already exists: $RUN_DIR"
mkdir -p "$RUN_DIR"

python3 "$TB_GENERATOR" \
  --out-dir "$RUN_DIR" \
  --tasks "$TASKS" \
  --seed "$SEED" \
  --clock-period-ns "$CLOCK_PERIOD_NS" \
  --rtl "$NETLIST_FILE" \
  --vector-dir-runtime vectors

mapfile -t USED_MACROS < <(
  (grep -oE 'RSPHVT[0-9]+X[0-9]+' "$NETLIST_FILE" || true) | sort -u
)
SRAM_MODELS=()
for macro in "${USED_MACROS[@]}"; do
  mapfile -t found_models < <(
    find "$SRAM_ROOT/$macro/VERILOG" -type f -name '*.v' \
      -exec grep -lE "module[[:space:]]+${macro}([[:space:]]|#|;|\\()" {} + \
      2>/dev/null | sort -u
  )
  ((${#found_models[@]} > 0)) || die \
    "cannot find functional Verilog model for $macro under $SRAM_ROOT/$macro/VERILOG"
  SRAM_MODELS+=("${found_models[0]}")
done

{
  echo "$RUN_DIR/tb_ToomCook1024.sv"
  echo "$NETLIST_FILE"
  printf '%s\n' "${STD_CELL_MODELS[@]}"
  if ((${#SRAM_MODELS[@]} > 0)); then
    printf '%s\n' "${SRAM_MODELS[@]}"
  fi
} > "$RUN_DIR/vfile.f"

VCS_FLAGS=(
  -full64
  -sverilog
  -timescale=1ns/1ps
  +lint=TFIPC-L
  -debug_access+all
  -debug_acc+dmptf
  -debug_region+cell+encrypt
  -kdb
  -top Tb
  +define+DUMP_FSDB
  +define+TSMC_INITIALIZE_MEM+no_warning
  +vcs+initreg+random
  -error=noMPD
  -f "$RUN_DIR/vfile.f"
  -Mdir="$RUN_DIR/csrc"
  -o "$RUN_DIR/simv"
)

if [[ -n "$SDF_FILE" ]]; then
  SIMULATION_MODE=sdf_max
  VCS_FLAGS+=(
    +maxdelays
    +sdfverbose
    -negdelay
    -sdfretain
    +define+NTC+RECREM
    -sdf "max:Tb/u_ToomCook1024:$SDF_FILE"
  )
  if ((TIMING_CHECKS == 0)); then
    VCS_FLAGS+=(+notimingcheck)
  else
    VCS_FLAGS+=(+neg_tchk)
  fi
else
  # Match the established encrypu power methodology: deterministic
  # zero-delay gate simulation for switching activity. Timing is analyzed
  # separately by PrimeTime using the mapped SDC and timing libraries.
  SIMULATION_MODE=zero_delay
  VCS_FLAGS+=(
    +no_notifier
    +nospecify
    +delay_mode_zero
    +notimingcheck
    +define+UNIT_DELAY
  )
fi

(
  cd "$RUN_DIR"
  vcs "${VCS_FLAGS[@]}" 2>&1 | tee vcs_compile.log
  ./simv +vcs+initreg+0 2>&1 | tee vcs_run.log
)

grep -Fq "[TB] ToomCook1024 PASS" "$RUN_DIR/vcs_run.log" || \
  die "functional PASS marker missing; refusing to create SAIF"
[[ -s "$RUN_DIR/tb.fsdb" ]] || die "full FSDB missing: $RUN_DIR/tb.fsdb"
[[ -s "$RUN_DIR/activity_window.txt" ]] || die "activity window missing"

BTNS=$(awk -F= '/^BTNS=/{print $2}' "$RUN_DIR/activity_window.txt")
ETNS=$(awk -F= '/^ETNS=/{print $2}' "$RUN_DIR/activity_window.txt")
[[ -n "$BTNS" && -n "$ETNS" ]] || die "invalid activity_window.txt"

(
  cd "$RUN_DIR"
  fsdbextract tb.fsdb -bt "${BTNS}ns" -et "${ETNS}ns" -o activity.fsdb
  fsdb2saif activity.fsdb -o activity.saif
)

[[ -s "$RUN_DIR/activity.fsdb" ]] || die "activity FSDB missing"
[[ -s "$RUN_DIR/activity.saif" ]] || die "SAIF missing"
grep -Fq "(INSTANCE Tb" "$RUN_DIR/activity.saif" || die "SAIF has no Tb scope"
grep -Fq "(INSTANCE u_ToomCook1024" "$RUN_DIR/activity.saif" || \
  die "SAIF has no Tb/u_ToomCook1024 scope"

if ((KEEP_FULL_FSDB == 0)); then
  rm -f "$RUN_DIR/tb.fsdb"
fi

{
  echo "status=PASS"
  echo "design=ToomCook1024"
  echo "tb_module=Tb"
  echo "dut_instance=u_ToomCook1024"
  echo "saif_strip_path=Tb/u_ToomCook1024"
  echo "netlist=$NETLIST_FILE"
  echo "simulation_mode=$SIMULATION_MODE"
  echo "sdf=${SDF_FILE:-none}"
  echo "saif=$RUN_DIR/activity.saif"
  echo "activity_fsdb=$RUN_DIR/activity.fsdb"
  echo "clock_period_ns=$CLOCK_PERIOD_NS"
  echo "tasks=$TASKS"
  echo "seed=$SEED"
  echo "timing_checks=$TIMING_CHECKS"
  printf 'std_cell_model=%s\n' "${STD_CELL_MODELS[@]}"
  printf 'sram_model=%s\n' "${SRAM_MODELS[@]}"
} > "$RUN_DIR/run_manifest.txt"

echo "[toomcook-vcs] PASS"
echo "[toomcook-vcs] simulation_mode=$SIMULATION_MODE"
echo "[toomcook-vcs] run_dir=$RUN_DIR"
if ((KEEP_FULL_FSDB == 1)); then
  echo "[toomcook-vcs] full_fsdb=$RUN_DIR/tb.fsdb"
fi
echo "[toomcook-vcs] verdi_fsdb=$RUN_DIR/activity.fsdb"
echo "[toomcook-vcs] activity_saif=$RUN_DIR/activity.saif"
echo "[toomcook-vcs] PTPX strip_path=Tb/u_ToomCook1024"
