# Shiny wiring single-source-of-truth audit

Date: 2026-04-30

## State owners and consumers

- NONMEM primary upload is owned by `mod_upload_server()` in `app/R/mod_upload.R:26`. It stores detected file paths and raw `.ext`/`.ctl` lines in `app/R/mod_upload.R:29-36`, resets them in `app/R/mod_upload.R:38-45`, and exposes parsed `.ext`, `.shk`, `.coi`, `.clt`, `.tab`, `.ctl/.mod/.con`, and `.cpu` reactives in `app/R/mod_upload.R:76-84`.
- Built-in example selection is owned by `mod_examples_server()` in `app/R/mod_examples.R:251`. The selected example state lives in `app/R/mod_examples.R:256-267`, paths/labels/summary are filled in `app/R/mod_examples.R:283-319`, and reset cleanup is in `app/R/mod_examples.R:322-335`.
- App-level primary run state is consumed through merged reactives in `app/app.R:281-305`. User upload now has priority per slot via `upload_has()` in `app/app.R:282-285`; example values are fallback only when the matching upload path is absent.
- `.ctl`-derived app state is centralized through `merged_ctl_lines` and `merged_true_vals` in `app/app.R:303-310`. CMT labels, design name, and GROUPSIZE are derived from uploaded `.ctl` lines in `app/app.R:261-278`; example GROUPSIZE is derived in `app/app.R:228-233`. True values are resolved by `resolve_true_values()` in `app/R/helpers_ui.R:71-80`.
- Global TABLE NO is owned by `selected_table_no` in `app/app.R:175-176`. The app-level selector is synchronized from merged `.ext` data in `app/app.R:319-343`, then injected as `tbl_no` from `app/app.R:391-393` into downstream modules.
- SSE upload is owned by `mod_sse_upload_server()` in `app/R/mod_sse_upload.R:84`. Design A/B raw results are stored in `app/R/mod_sse_upload.R:87-89`, parsed with `read_sse_raw_all()` in `app/R/mod_sse_upload.R:101-148`, reset in `app/R/mod_sse_upload.R:91-99`, and returned as shared reactives in `app/R/mod_sse_upload.R:222-228`.
- SSE consumers receive injected reactives from `app/app.R:457-485`: validation receives `sse_a_data`, `sse_b_data`, `.ctl` lines, true values, FIM inputs, and `tbl_no`; analysis receives shared SSE A/B plus true values; comparison receives shared SSE A/B plus shared `.ctl`/true-value state.
- Compare/multi-run state is owned separately by `mod_compare_server()` in `app/R/mod_compare.R:17`. It is secondary-run state, not competing primary state. Dynamic run data is stored in `app/R/mod_compare.R:21-25`, parsed in `app/R/mod_compare.R:100-150`, reset for UI/data cleanup in `app/R/mod_compare.R:81-88`, and aggregated into `all_runs` in `app/app.R:351-388`.
- Reset is owned by `reset_trigger` in `app/app.R:169-170`. App-level derived state cleanup is in `app/app.R:396-400`; module reset hooks are wired for upload/examples/compare in `app/app.R:178-181`, SSE upload in `app/app.R:457-458`, power in `app/R/mod_power.R:41-44`, and home's reset button in `app/R/mod_home.R:98-100`.

## Duplicated or competing logic found

- The main duplication was not parser ownership but priority arbitration: before this audit, merged primary reactives preferred examples over uploads despite the upload-priority comment in `app/app.R:251`. That could keep stale example-derived data visible if upload and example state overlapped.
- NONMEM parsing is still intentionally present in three places: primary upload (`mod_upload`), built-in examples loaded by the app, and secondary comparison runs (`mod_compare`). Consumer tabs should not parse primary NONMEM files themselves.
- SSE raw-results parsing is centralized in `mod_sse_upload`. SSE validation, analysis, and comparison consume injected shared reactives and should not expose their own raw-results upload controls.
- `.ctl` true-value parsing is centralized through `resolve_true_values()`. SSE consumers should call that helper with injected `shared_true_vals` and `shared_ctl_lines`, not read an alternate control stream.

## Fixes applied

- Changed merged primary reactives in `app/app.R:281-305` so uploaded slots win over example slots. This covers `.ext`, `.shk`, `.coi`, `.clt`, `.tab`, `.ctl/.mod/.con`, `.cpu`, summary, `.ctl` lines, and `.ext` lines.
- Passed `reset_trigger` into `mod_compare_server()` at `app/app.R:180` and added compare-run cleanup in `app/R/mod_compare.R:81-88`.
- The compare reset intentionally preserves the monotone run counter and observer registry. Reusing dynamic input IDs after reset would leave old observers alive and can double-handle future uploads, so reset clears current UI/data state while keeping future IDs unique.

## Edge cases covered by tests

- Primary NONMEM parser calls remain limited to upload, examples, and compare paths.
- SSE raw-results upload/parsing remains limited to `mod_sse_upload`.
- SSE consumers use injected `.ctl`/true-value state.
- TABLE NO selection remains app-owned and injected.
- Upload-over-example priority is pinned by source-level contract tests.
- Universal reset reaches upload, examples, SSE upload, power, home, and compare state owners.
