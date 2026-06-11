# NONMEM Elementary Dataset Builder Design

Date: 2026-06-11

## Purpose

Add a new Shiny tab that helps pharmacometricians create a theoretical NONMEM-compatible CSV for `$DESIGN` evaluation. The first draft focuses only on elementary designs: each `ID` is a design prototype, not a real subject.

The goal is to make it easy to turn a clinical trial sampling schedule into a clean input dataset that respects strict CSV conventions:

- English uppercase column names.
- Comma separators.
- Decimal points with `.`.
- Missing values written as `.` when missing values are needed.

## Scope

In scope:

- A new `Design` tab, tentatively named `Dataset Builder`.
- Elementary design generation only.
- Evaluation datasets only.
- Fixed dosing and fixed sampling times.
- One or more prototype arms/designs.
- Live preview of generated rows.
- CSV download.
- Validation feedback for schema and row conventions.

Out of scope for this first draft:

- Real subject expansion.
- Subject-level covariates such as weight or CLCR.
- Time optimization columns: `TSTRAT`, `TMIN`, `TMAX`.
- Dose optimization columns: `DSTRAT`, `DMIN`, `DMAX`.
- Control stream generation.
- Importing a protocol document or parsing free-form schedule text beyond simple numeric lists.

## CSV Contract

The generated CSV uses this baseline column order:

```text
ID,TIME,DOSE,AMT,RATE,DV,MDV,EVID,CMT,ARM
```

Column meaning:

- `ID`: elementary design prototype identifier.
- `TIME`: event time in hours.
- `DOSE`: nominal dose attached to the prototype or row.
- `AMT`: NONMEM dose amount; positive on dose rows and `0` on observation rows.
- `RATE`: infusion rate or `0` for bolus/observation rows. For the first draft, the UI exposes a simple route/rate mode and defaults to `0` unless the user enters a rate.
- `DV`: `0` for dose rows, `1` placeholder for observation rows.
- `MDV`: `1` for dose rows, `0` for observation rows.
- `EVID`: `1` for dose rows, `0` for observation rows.
- `CMT`: compartment number chosen in the UI.
- `ARM`: numeric prototype arm label.

Dose row rules:

- `EVID = 1`
- `MDV = 1`
- `DV = 0`
- `AMT > 0`
- `DOSE > 0`
- `CMT` equals the selected dose compartment.

Observation row rules:

- `EVID = 0`
- `MDV = 0`
- `DV = 1`
- `AMT = 0`
- `RATE = 0`
- `CMT` equals the selected observation compartment.

Rows are sorted by `ID`, then `TIME`, with dose rows before observation rows when they share the same time.

## User Experience

The tab follows the existing V5 Shiny patterns: `page_shell()`, `page_header()`, `page_section()`, `control_panel()`, `table_panel()`, and `status_panel()`.

The first screen contains:

- Prototype settings:
  - number of prototypes or arms
  - arm labels or numeric arm ids
  - dose amount
  - dose interval in hours
  - number of administrations
  - dose compartment
  - observation compartment
  - optional rate
- Sampling schedule:
  - a text area accepting comma, semicolon, whitespace, or newline separated numeric times
  - example placeholder such as `1, 24, 168, 671.9`
- Actions:
  - generate or refresh preview
  - download CSV
- Outputs:
  - validation banner
  - generated dataset preview through DT
  - compact summary: prototype count, dose row count, observation row count, total rows

The UI should not require a loaded NONMEM run. It is a standalone design preparation tool.

## Architecture

Add pure R helpers in a new file:

```text
R/nonmem_dataset_builder.R
```

Proposed functions:

- `parse_sampling_times(text)`: returns a sorted numeric vector from a simple schedule string.
- `build_nonmem_elementary_dataset(...)`: creates the dataset tibble from prototype, dosing, and observation settings.
- `validate_nonmem_dataset(dat)`: returns structured validation results for column names, uppercase schema, dose rows, observation rows, numeric fields, and missing value representation.
- `write_nonmem_csv(dat, file)`: writes the dataset with comma separators, dot decimals, and `na = "."`.

Add a Shiny module:

```text
app/R/mod_dataset_builder.R
```

Proposed functions:

- `mod_dataset_builder_ui(id)`
- `mod_dataset_builder_server(id, reset_trigger = NULL)`

Wire it into:

- `app/app.R`: add `tabPanel("Dataset Builder", value = "dataset_builder", mod_dataset_builder_ui("dataset_builder"))` under `navbarMenu("Design", ...)`.
- `app/app.R`: call `mod_dataset_builder_server("dataset_builder", reset_trigger = reset_trigger)` near the other design module servers.

No changes are needed in `mod_upload.R` or `mod_sse_upload.R`; this feature generates an input dataset and does not parse NONMEM outputs or PsN SSE files.

## Testing

Add focused tests in:

```text
tests/testthat/test-nonmem_dataset_builder.R
```

Test cases:

- sampling schedule parsing accepts comma, semicolon, whitespace, and newline separated values.
- generated dataset has exact uppercase columns in the expected order.
- dose rows follow `EVID/MDV/DV/AMT/CMT` conventions.
- observation rows follow `EVID/MDV/DV/AMT/RATE/CMT` conventions.
- rows sort dose before observations at identical times.
- generated CSV round-trips through `write_nonmem_csv()` and `readr::read_csv(na = ".")`.
- validation reports problems for lowercase column names, missing required columns, invalid dose rows, and invalid observation rows.

## Acceptance Criteria

- Users can create an elementary design CSV without loading any NONMEM output.
- The generated CSV uses comma separators, dot decimals, and `.` missing values through `readr::write_csv(na = ".")`.
- The preview and downloaded file contain only uppercase English column names.
- No optimization columns are generated in this first draft.
- No subject covariate workflow is exposed in this first draft.
- The new logic is covered by unit tests and the app can source successfully.
