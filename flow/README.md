# ToomCook1024 gate-level VCS and PrimeTime PX flow

This flow follows the proven `encrypu` sequence:

1. Generate deterministic random vectors and a self-checking SystemVerilog TB.
2. Run a deterministic zero-delay simulation of the DC mapped netlist in VCS.
3. Require `[TB] ToomCook1024 PASS`.
4. Extract the measured active interval from FSDB and convert it to SAIF.
5. Read the same mapped netlist, mapped SDC, and SAIF in PrimeTime PX.

The behavioral A/B/C memories are outside `u_ToomCook1024`. PTPX uses
`read_saif ... -strip_path Tb/u_ToomCook1024`, so external input/output memory
power is excluded. The three internal RSPHVT macro types remain inside the DUT
and are included through their CCS DBs.

## VCS on the post-synthesis server

Use the same clock period that was used by DC:

```bash
cd /path/to/TOOMCOOKChisel

flow/vcs/run_postsyn_vcs.sh \
  --netlist /path/to/ToomCook1024.mapped.v \
  --sram-root /path/to/SramLib22 \
  --clock-period-ns 2.0 \
  --tasks 8 \
  --seed 1
```

This default matches the established `encrypu` power methodology:
`+nospecify +delay_mode_zero +notimingcheck`. It does not need an SDF. The
mapped netlist supplies gate structure, VCS supplies switching activity, and
PrimeTime uses the mapped SDC and CCS timing/power libraries.

If an SDF is available and a delay-annotated gate simulation is desired, add:

```bash
  --sdf /path/to/ToomCook1024.mapped.sdf
```

This selects max-delay annotation. Add `--enable-timing-checks` only when
timing-check notifier behavior is also required.

The script auto-finds the three 22nm standard-cell simulation models under
`/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/verilog`. Use repeated
`--std-cell-model FILE` arguments if the server has a different layout.

Important outputs are:

```text
vcs/runs/<tag>/vcs_run.log
vcs/runs/<tag>/activity.fsdb
vcs/runs/<tag>/activity.saif
vcs/runs/<tag>/run_manifest.txt
```

`tb.fsdb` is removed after successful activity extraction by default to avoid
duplicating a large waveform. Add `--keep-full-fsdb` if reset/fill activity
outside the PTPX window must also be debugged.

Open the activity-window waveform with:

```bash
verdi -sv -f flow/vcs/runs/<tag>/vfile.f \
  -ssf flow/vcs/runs/<tag>/activity.fsdb
```

## PrimeTime PX

Only use a SAIF from a VCS run whose `run_manifest.txt` contains
`status=PASS`.

```bash
flow/ptpx/run_ptpx.sh \
  --netlist /path/to/ToomCook1024.mapped.v \
  --sdc /path/to/ToomCook1024.mapped.sdc \
  --saif /path/to/activity.saif \
  --sram-root /path/to/SramLib22 \
  --clock-period-ns 2.0 \
  --pt-cores 4
```

The default power corner is:

```text
standard cells: tt0p9v25c CCS
memory macros:  tt_typical_0p90v_0p90v_25c CCS
```

`CLOCK_PERIOD_NS` must match across DC, VCS, and PTPX. Even in zero-delay VCS,
the period determines the event rate and PTPX dynamic-power normalization.
