# Comprehensive Dataset Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a paste/edit table Dataset Builder that supports per-elementary-design dosing and sampling schedules, including schedules like `psm_eval.csv`.

**Architecture:** Add pure R parsing and generation helpers around a schedule table while preserving the existing simple builder API. Keep Shiny as a thin layer that collects the pasted schedule table, calls the pure helpers, displays errors/warnings, and downloads CSV output.

**Tech Stack:** R, Shiny, DT, dplyr, tibble, readr, testthat.

---

## File Structure

- Modify `R/nonmem_dataset_builder.R`
  - Add dose-event parsing, schedule-table parsing, per-design dataset generation, summaries, and validation warnings.
  - Keep `build_nonmem_elementary_dataset()` working for the existing simple path.

- Modify `app/R/mod_dataset_builder.R`
  - Replace the global form with a paste/edit schedule table workflow.
  - Add preset buttons, validation, summary, preview, CSV download, and schedule-spec download.

- Modify `tests/testthat/test-nonmem_dataset_builder.R`
  - Add TDD coverage for dose events, schedule tables, psm_eval reproduction, validation, and Shiny server behavior.

## Task 1: Core Schedule Tests

- [ ] Write failing tests for `parse_dose_events()`, `parse_design_schedule_table()`, and `build_nonmem_dataset_from_schedule_table()`.
- [ ] Run the focused test file and verify the new tests fail because the functions do not exist.
- [ ] Implement the pure helpers in `R/nonmem_dataset_builder.R`.
- [ ] Run the focused test file and verify the new tests pass.
- [ ] Commit as `Add schedule-table dataset builder core`.

## Task 2: Validation And Summary

- [ ] Write failing tests for blocking schedule-table errors and nonblocking warnings.
- [ ] Implement validation and summary helpers.
- [ ] Run the focused test file and verify it passes.
- [ ] Commit as `Add dataset builder schedule validation`.

## Task 3: Shiny Paste/Edit Workflow

- [ ] Write failing Shiny module tests for preset loading, schedule-table generation, stale-preview detection, invalid input handling, and download eligibility.
- [ ] Update `app/R/mod_dataset_builder.R` to use the paste/edit schedule table workflow.
- [ ] Run the focused test file and verify it passes.
- [ ] Commit as `Add paste-table dataset builder UI`.

## Task 4: Verification And Review

- [ ] Run `Rscript tests/run_tests.R`.
- [ ] Run the Shiny app source smoke test.
- [ ] Dispatch a reviewer subagent with the diff and requirements.
- [ ] Fix any blocking or important review findings.
- [ ] Commit fixes if needed.

## Acceptance Checklist

- [ ] A pasted schedule table can reproduce the core structure of `docs/results/psm_eval/psm_eval.csv`.
- [ ] Each elementary design can have different dose events and sampling times.
- [ ] Dose event tokens can override rate and CMT per event.
- [ ] Observation `DOSE` carries the current/latest dose amount.
- [ ] Metadata columns such as `ARM` are populated only when present in the schedule table.
- [ ] Existing simple builder tests still pass.
- [ ] Full test runner exits 0.

