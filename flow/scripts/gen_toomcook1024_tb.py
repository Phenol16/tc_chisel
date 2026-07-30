#!/usr/bin/env python3

import argparse
import random
import re
from pathlib import Path


N = 1024
A_WIDTH = 24
B_WIDTH = 8
C_WIDTH = 24
BANKS = 4
DEPTH = 16
LANES = 16


def write_hex(path: Path, values, width: int) -> None:
    digits = (width + 3) // 4
    mask = (1 << width) - 1
    with path.open("w", encoding="ascii") as handle:
        for value in values:
            handle.write(f"{value & mask:0{digits}x}\n")


def negacyclic(a, b):
    out = [0] * N
    mask = (1 << C_WIDTH) - 1
    for i, av in enumerate(a):
        for j, bv in enumerate(b):
            product = av * bv
            index = i + j
            if index < N:
                out[index] += product
            else:
                out[index - N] -= product
    return [value & mask for value in out]


def expected_ports():
    ports = {
        "clock",
        "reset",
        "io_start",
        "io_busy",
        "io_done",
        "io_resultReady",
    }
    for bank in range(BANKS):
        ports.add(f"io_aMem_{bank}_en")
        ports.add(f"io_aMem_{bank}_addr")
        ports.add(f"io_bMem_{bank}_en")
        ports.add(f"io_bMem_{bank}_addr")
        ports.add(f"io_cMem_{bank}_we")
        ports.add(f"io_cMem_{bank}_addr")
        for lane in range(LANES):
            ports.add(f"io_aMem_{bank}_dout_{lane}")
            ports.add(f"io_bMem_{bank}_dout_{lane}")
            ports.add(f"io_cMem_{bank}_din_{lane}")
    return ports


def validate_rtl_ports(rtl_path: Path) -> None:
    text = rtl_path.read_text(encoding="utf-8")
    match = re.search(
        r"\bmodule\s+ToomCook1024\s*\((.*?)\)\s*;",
        text,
        flags=re.DOTALL,
    )
    if not match:
        raise SystemExit(f"cannot find module ToomCook1024 in {rtl_path}")
    header = match.group(1)
    missing = sorted(port for port in expected_ports() if not re.search(
        rf"\b{re.escape(port)}\b", header
    ))
    if missing:
        raise SystemExit(
            "ToomCook1024 interface does not match the power TB; missing ports: "
            + ", ".join(missing)
        )


def emit_connections():
    lines = [
        "  ToomCook1024 u_ToomCook1024 (",
        "    .clock(clock),",
        "    .reset(reset),",
        "    .io_start(io_start),",
        "    .io_busy(io_busy),",
        "    .io_done(io_done),",
    ]
    for bank in range(BANKS):
        lines.append(f"    .io_aMem_{bank}_en(a_en[{bank}]),")
        lines.append(f"    .io_aMem_{bank}_addr(a_addr[{bank}]),")
        for lane in range(LANES):
            lines.append(
                f"    .io_aMem_{bank}_dout_{lane}(a_dout[{bank}][{lane}]),"
            )
    for bank in range(BANKS):
        lines.append(f"    .io_bMem_{bank}_en(b_en[{bank}]),")
        lines.append(f"    .io_bMem_{bank}_addr(b_addr[{bank}]),")
        for lane in range(LANES):
            lines.append(
                f"    .io_bMem_{bank}_dout_{lane}(b_dout[{bank}][{lane}]),"
            )
    lines.append("    .io_resultReady(io_resultReady),")
    for bank in range(BANKS):
        lines.append(f"    .io_cMem_{bank}_we(c_we[{bank}]),")
        lines.append(f"    .io_cMem_{bank}_addr(c_addr[{bank}]),")
        for lane in range(LANES):
            comma = "," if not (bank == BANKS - 1 and lane == LANES - 1) else ""
            lines.append(
                f"    .io_cMem_{bank}_din_{lane}(c_din[{bank}][{lane}]){comma}"
            )
    lines.append("  );")
    return "\n".join(lines)


def emit_testbench(tasks: int, clock_period_ns: float, vector_dir: str) -> str:
    half_period = clock_period_ns / 2.0
    timeout_cycles = 1400 + tasks * 500
    connections = emit_connections()
    return f"""`timescale 1ns/1ps

module Tb;
  localparam integer N = {N};
  localparam integer TASKS = {tasks};
  localparam integer BANKS = {BANKS};
  localparam integer DEPTH = {DEPTH};
  localparam integer LANES = {LANES};
  localparam integer TIMEOUT_CYCLES = {timeout_cycles};
  localparam integer EXPECTED_BANK_WRITES = 65;
  localparam integer MAX_ERROR_PRINTS = 20;

  reg clock;
  reg reset;
  reg io_start;
  reg io_resultReady;
  wire io_busy;
  wire io_done;

  wire a_en [0:BANKS-1];
  wire [3:0] a_addr [0:BANKS-1];
  reg [23:0] a_dout [0:BANKS-1][0:LANES-1];
  wire b_en [0:BANKS-1];
  wire [3:0] b_addr [0:BANKS-1];
  reg [7:0] b_dout [0:BANKS-1][0:LANES-1];
  wire c_we [0:BANKS-1];
  wire [3:0] c_addr [0:BANKS-1];
  wire [23:0] c_din [0:BANKS-1][0:LANES-1];

  reg [23:0] a_mem [0:BANKS-1][0:DEPTH-1][0:LANES-1];
  reg [7:0] b_mem [0:BANKS-1][0:DEPTH-1][0:LANES-1];
  reg [23:0] c_mem [0:BANKS-1][0:DEPTH-1][0:LANES-1];

  reg [23:0] a_file [0:TASKS*N-1];
  reg [7:0] b_file [0:TASKS*N-1];
  reg [23:0] c_expect_file [0:TASKS*N-1];

  integer completed_tasks;
  integer error_count;
  integer c_bank_writes;
  integer cycle_count;
  integer task_start_cycle [0:TASKS-1];
  integer task_done_cycle [0:TASKS-1];
  integer activity_window_fd;
  real activity_begin_ns;
  real activity_end_ns;
  string vector_dir;
  string vector_path;

  initial clock = 1'b0;
  always #{half_period:.9f} clock = ~clock;

{connections}

  // External input SRAM model: one-cycle synchronous read.
  // These memories are testbench infrastructure and are outside the DUT SAIF path.
  always @(posedge clock) begin : external_input_sram_read
    integer bank_idx;
    integer lane_idx;
    if (reset) begin
      for (bank_idx = 0; bank_idx < BANKS; bank_idx = bank_idx + 1) begin
        for (lane_idx = 0; lane_idx < LANES; lane_idx = lane_idx + 1) begin
          a_dout[bank_idx][lane_idx] <= 24'b0;
          b_dout[bank_idx][lane_idx] <= 8'b0;
        end
      end
    end else begin
      for (bank_idx = 0; bank_idx < BANKS; bank_idx = bank_idx + 1) begin
        if (a_en[bank_idx]) begin
          for (lane_idx = 0; lane_idx < LANES; lane_idx = lane_idx + 1) begin
            a_dout[bank_idx][lane_idx] <=
              a_mem[bank_idx][a_addr[bank_idx]][lane_idx];
          end
        end
        if (b_en[bank_idx]) begin
          for (lane_idx = 0; lane_idx < LANES; lane_idx = lane_idx + 1) begin
            b_dout[bank_idx][lane_idx] <=
              b_mem[bank_idx][b_addr[bank_idx]][lane_idx];
          end
        end
      end
    end
  end

  // External output SRAM model. A write is sampled on the rising edge.
  always @(posedge clock) begin : external_output_sram_write
    integer bank_idx;
    integer lane_idx;
    if (!reset) begin
      for (bank_idx = 0; bank_idx < BANKS; bank_idx = bank_idx + 1) begin
        if (c_we[bank_idx]) begin
          c_bank_writes = c_bank_writes + 1;
          for (lane_idx = 0; lane_idx < LANES; lane_idx = lane_idx + 1) begin
            c_mem[bank_idx][c_addr[bank_idx]][lane_idx] <=
              c_din[bank_idx][lane_idx];
          end
        end
      end
    end
  end

  always @(posedge clock) begin
    cycle_count <= cycle_count + 1;
  end

  task load_task;
    input integer task_idx;
    integer coeff_idx;
    integer lane_idx;
    integer bank_idx;
    integer addr_idx;
    integer rem_idx;
    integer aa_idx;
    integer bb_idx;
    begin
      for (coeff_idx = 0; coeff_idx < N; coeff_idx = coeff_idx + 1) begin
        lane_idx = coeff_idx / 64;
        rem_idx = coeff_idx % 64;
        aa_idx = rem_idx / 16;
        bb_idx = (rem_idx % 16) / 4;
        bank_idx = rem_idx % 4;
        addr_idx = aa_idx * 4 + bb_idx;
        a_mem[bank_idx][addr_idx][lane_idx] =
          a_file[task_idx*N + coeff_idx];
        b_mem[bank_idx][addr_idx][lane_idx] =
          b_file[task_idx*N + coeff_idx];
      end
    end
  endtask

  task clear_output_memory;
    integer bank_idx;
    integer addr_idx;
    integer lane_idx;
    begin
      for (bank_idx = 0; bank_idx < BANKS; bank_idx = bank_idx + 1) begin
        for (addr_idx = 0; addr_idx < DEPTH; addr_idx = addr_idx + 1) begin
          for (lane_idx = 0; lane_idx < LANES; lane_idx = lane_idx + 1) begin
            c_mem[bank_idx][addr_idx][lane_idx] = 24'b0;
          end
        end
      end
    end
  endtask

  task wait_input_free;
    integer waited;
    begin : wait_busy_loop
      for (waited = 0; waited < 1200; waited = waited + 1) begin
        @(negedge clock);
        if (io_busy === 1'b0) begin
          disable wait_busy_loop;
        end
      end
      if (io_busy !== 1'b0) begin
        $fatal(1, "[TB] input busy timeout or X after %0d cycles", waited);
      end
    end
  endtask

  task launch_task;
    input integer task_idx;
    begin
      wait_input_free();
      load_task(task_idx);
      io_start = 1'b1;
      task_start_cycle[task_idx] = cycle_count;
      $display("[TB] task=%0d start cycle=%0d", task_idx, cycle_count);
      @(posedge clock);
      @(negedge clock);
      io_start = 1'b0;
    end
  endtask

  task check_result;
    input integer task_idx;
    integer coeff_idx;
    integer lane_idx;
    integer bank_idx;
    integer addr_idx;
    reg [23:0] got;
    reg [23:0] expected;
    begin
      for (coeff_idx = 0; coeff_idx < N; coeff_idx = coeff_idx + 1) begin
        addr_idx = coeff_idx / 64;
        bank_idx = (coeff_idx % 64) / 16;
        lane_idx = coeff_idx % 16;
        got = c_mem[bank_idx][addr_idx][lane_idx];
        expected = c_expect_file[task_idx*N + coeff_idx];
        if (got !== expected) begin
          if (error_count < MAX_ERROR_PRINTS) begin
            $display(
              "[TB][ERR] task=%0d c[%0d] bank=%0d addr=%0d lane=%0d expected=%06x got=%06x",
              task_idx, coeff_idx, bank_idx, addr_idx, lane_idx,
              expected, got
            );
          end
          error_count = error_count + 1;
        end
      end
      if (c_bank_writes != EXPECTED_BANK_WRITES) begin
        $display(
          "[TB][ERR] task=%0d expected %0d C-bank writes, observed %0d",
          task_idx, EXPECTED_BANK_WRITES, c_bank_writes
        );
        error_count = error_count + 1;
      end
      task_done_cycle[task_idx] = cycle_count;
      $display(
        "[TB] task=%0d done cycle=%0d latency=%0d bank_writes=%0d",
        task_idx, cycle_count,
        cycle_count - task_start_cycle[task_idx], c_bank_writes
      );
      c_bank_writes = 0;
    end
  endtask

  // done is checked on the falling edge, after the final output-SRAM write.
  always @(negedge clock) begin
    if (!reset && io_done === 1'b1) begin
      if (completed_tasks >= TASKS) begin
        $fatal(1, "[TB] unexpected extra io_done pulse");
      end
      check_result(completed_tasks);
      completed_tasks = completed_tasks + 1;
    end
  end

`ifdef DUMP_FSDB
  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0, Tb);
    $fsdbDumpMDA();
  end
`endif

  initial begin : test_sequence
    integer task_idx;
    integer waited;

    clock = 1'b0;
    reset = 1'b1;
    io_start = 1'b0;
    io_resultReady = 1'b1;
    completed_tasks = 0;
    error_count = 0;
    c_bank_writes = 0;
    cycle_count = 0;
    vector_dir = "{vector_dir}";

    vector_path = $sformatf("%s/a_all.hex", vector_dir);
    $readmemh(vector_path, a_file);
    vector_path = $sformatf("%s/b_all.hex", vector_dir);
    $readmemh(vector_path, b_file);
    vector_path = $sformatf("%s/c_expected_all.hex", vector_dir);
    $readmemh(vector_path, c_expect_file);
    clear_output_memory();

    // Keep synchronous reset asserted long enough for gate-level init.
    repeat (5) @(posedge clock);
    @(negedge clock);
    reset = 1'b0;
    repeat (2) @(posedge clock);
    @(negedge clock);

    activity_begin_ns = $realtime;
    for (task_idx = 0; task_idx < TASKS; task_idx = task_idx + 1) begin
      launch_task(task_idx);
    end

    begin : completion_wait
      for (waited = 0; waited < TIMEOUT_CYCLES; waited = waited + 1) begin
        @(negedge clock);
        if (completed_tasks == TASKS) begin
          disable completion_wait;
        end
      end
      if (completed_tasks != TASKS) begin
        $fatal(
          1,
          "[TB] completion timeout: completed=%0d expected=%0d busy=%b done=%b",
          completed_tasks, TASKS, io_busy, io_done
        );
      end
    end

    repeat (2) @(posedge clock);
    activity_end_ns = $realtime;
    activity_window_fd = $fopen("activity_window.txt", "w");
    if (activity_window_fd == 0) begin
      $fatal(1, "[TB] cannot create activity_window.txt");
    end
    $fwrite(activity_window_fd, "BTNS=%0.6f\\n", activity_begin_ns);
    $fwrite(activity_window_fd, "ETNS=%0.6f\\n", activity_end_ns);
    $fclose(activity_window_fd);

    if (error_count != 0) begin
      $fatal(1, "[TB] ToomCook1024 FAILED with %0d errors", error_count);
    end
    for (task_idx = 1; task_idx < TASKS; task_idx = task_idx + 1) begin
      $display(
        "[TB] done interval task%0d->task%0d = %0d cycles",
        task_idx - 1, task_idx,
        task_done_cycle[task_idx] - task_done_cycle[task_idx - 1]
      );
    end
    $display("[TB] ToomCook1024 PASS tasks=%0d seed vectors complete", TASKS);
    $finish;
  end

endmodule
"""


def parse_args():
    parser = argparse.ArgumentParser(
        description="Generate a VCS/SDF/PTPX-ready ToomCook1024 testbench."
    )
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--tasks", type=int, default=8)
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--clock-period-ns", type=float, required=True)
    parser.add_argument("--rtl", type=Path)
    parser.add_argument("--vector-dir-runtime", default="vectors")
    return parser.parse_args()


def main():
    args = parse_args()
    if args.tasks < 3:
        raise SystemExit("--tasks must be at least 3 to exercise pipeline overlap")
    if args.clock_period_ns <= 0:
        raise SystemExit("--clock-period-ns must be positive")
    if args.rtl is not None:
        validate_rtl_ports(args.rtl)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    vectors_dir = args.out_dir / "vectors"
    vectors_dir.mkdir(parents=True, exist_ok=True)

    rng = random.Random(args.seed)
    all_a = []
    all_b = []
    all_c = []
    for _ in range(args.tasks):
        a = [rng.getrandbits(A_WIDTH) for _ in range(N)]
        b = [rng.getrandbits(B_WIDTH) for _ in range(N)]
        c = negacyclic(a, b)
        all_a.extend(a)
        all_b.extend(b)
        all_c.extend(c)

    write_hex(vectors_dir / "a_all.hex", all_a, A_WIDTH)
    write_hex(vectors_dir / "b_all.hex", all_b, B_WIDTH)
    write_hex(vectors_dir / "c_expected_all.hex", all_c, C_WIDTH)

    tb_path = args.out_dir / "tb_ToomCook1024.sv"
    tb_path.write_text(
        emit_testbench(
            args.tasks,
            args.clock_period_ns,
            args.vector_dir_runtime,
        ),
        encoding="ascii",
    )

    manifest = args.out_dir / "tb_manifest.txt"
    manifest.write_text(
        "\n".join(
            [
                "design=ToomCook1024",
                "tb_module=Tb",
                "dut_instance=u_ToomCook1024",
                "saif_strip_path=Tb/u_ToomCook1024",
                f"tasks={args.tasks}",
                f"seed={args.seed}",
                f"clock_period_ns={args.clock_period_ns}",
                f"tb={tb_path}",
                f"vectors={vectors_dir}",
            ]
        )
        + "\n",
        encoding="ascii",
    )
    print(f"Generated {tb_path}")
    print(f"Generated vectors under {vectors_dir}")
    print(f"SAIF strip path: Tb/u_ToomCook1024")


if __name__ == "__main__":
    main()
