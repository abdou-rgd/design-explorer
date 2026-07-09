# Comprehensive NONMEM Dataset Builder Design

Date: 2026-07-09

## Purpose

Upgrade the Dataset Builder from a single global schedule generator into a practical elementary-design dataset builder. The builder must support paste/edit tables where each elementary design can have its own dosing events, sampling schedule, compartments, rates, and metadata columns.

The reference behavior is `docs/results/psm_eval/psm_eval.csv`: two elementary designs with different dosing schedules, different sampling schedules, different dose compartments, and an `ARM` metadata column.

## User Input Model

The primary input is a pasted CSV table. This keeps the UI compact while allowing complete per-design control.

Required columns:

```text
DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,OBS_CMT
```

Optional columns:

```text
ID,N_SUBJECTS,DOSE_CMT,RATE,DOSE_UNIT
```

Column meanings:

- `DESIGN`: elementary design label. It is exported as metadata when present.
- `ARM`: arm or group label. It is exported as metadata when present.
- `ID`: optional numeric output ID. When absent, IDs are assigned in table order.
- `N_SUBJECTS`: optional expansion count. Default is `1`. Each expansion receives a new ID with the same schedules.
- `DOSE_EVENTS`: semicolon-separated event tokens. Each token is `time:amt`, `time:amt:rate`, or `time:amt:rate:cmt`.
- `SAMPLING_TIMES`: comma, semicolon, whitespace, or newline-separated observation times.
- `DOSE_CMT`: default dose compartment for dose tokens without an explicit CMT.
- `OBS_CMT`: observation compartment.
- `RATE`: default rate for dose tokens without an explicit rate.
- `DOSE_UNIT`: optional metadata.

Example:

```csv
DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT,RATE
1,0,"0:1800:1800:2;672:1200:1200:2","1,671.9",2,2,0
2,1,"0:1800:1800:1;336:1800:1800:1","335.9,671.9",1,2,0
```

## Output Contract

The generated dataset keeps the NONMEM core columns first:

```text
ID,TIME,DOSE,AMT,RATE,DV,MDV,EVID,CMT
```

Metadata columns follow the core columns in the order they are requested or present in the input, such as:

```text
DESIGN,ARM,DOSE_UNIT
```

Rules:

- Dose rows use `EVID=1`, `MDV=1`, `DV=0`, `AMT>0`, `DOSE=AMT`, and the dose event CMT.
- Observation rows use `EVID=0`, `MDV=0`, `DV=1`, `AMT=0`, `RATE=0`, and `OBS_CMT`.
- Observation-row `DOSE` carries the most recent dose amount at or before the observation time. If an observation occurs before any dose, it uses the first dose amount.
- Rows sort by `ID`, `TIME`, then dose rows before observations at identical times.
- CSV export uses comma separators, decimal points, uppercase columns, and `.` for missing values.

## Validation

Blocking errors:

- Missing required columns.
- Invalid numeric fields.
- Empty dosing or sampling schedule.
- Negative times.
- Nonpositive dose amounts.
- Invalid CMT, `ID`, or `N_SUBJECTS`.
- Malformed dose event token.

Warnings:

- Sampling exactly at a dose time.
- Sampling before the first dose.
- No observation after the first dose.
- Duplicate generated rows with the same `ID`, `TIME`, `EVID`, and `CMT`.

Warnings do not block preview or download. Blocking errors do.

## UI

The Shiny tab remains standalone and does not require uploaded NONMEM output.

Sections:

- Schedule table: a paste/edit text area containing the CSV schedule table.
- Presets: load a simple example or load the `psm_eval`-style example.
- Validation: status panel showing blocking errors and warnings separately.
- Summary: design count, ID count, dose rows, observation rows, total rows, and time range.
- Preview: generated dataset with CSV download.
- Schedule-spec download: lets users save the pasted builder input.

## Testing

Tests must cover:

- Dose-event parsing.
- Schedule-table parsing.
- Reproduction of the structure and key values in `psm_eval.csv`.
- Backward compatibility for the existing simple builder path.
- Validation errors and warnings.
- Shiny server generation state for valid and invalid paste tables.

