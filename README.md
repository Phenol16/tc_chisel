# ToomCook1024 High backup

This repository is a source backup of the current `TOOMCOOKChisel` High-layer design as of 2026-07-30.

Included:

- `src/main/scala/High/`: High-layer scheduling, evaluation, interpolation, SRAM wrappers, and top-level design
- `src/main/scala/Core/`: the unchanged Core implementation directly instantiated by High
- `src/test/scala/High/`: High-layer SRAM and pipeline tests
- `flow/`: gate-level VCS/SDF/FSDB/SAIF/PTPX helper flow
- minimal sbt build metadata

Excluded intentionally: build outputs, synthesis reports, generated netlists, archives, local tools, and unrelated legacy implementations.

Validation note: static flow checks were previously completed, but dynamic VCS/PTPX execution was not confirmed in the local environment.
