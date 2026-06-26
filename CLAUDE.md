# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

CSIemail is a **Golem-structured R Shiny package** for ACS Cyprus. It splits financial reports by store, emails per-store attachments, and produces per-city cashier reconciliation workbooks. Despite being a Shiny app, it is built and loaded as an R **package** (`CSIemail`), so app code lives in `R/` and is loaded with `pkgload::load_all()` / `devtools`, not sourced ad hoc.

## Running & Developing

Always load the package first — functions are package-internal (`@noRd`) and many are referenced via `CSIemail:::name`.

```r
# Development (from project root)
golem::document_and_reload()   # or pkgload::load_all()
run_app()                       # uses GOLEM_CONFIG_ACTIVE, defaults to "default"

# dev/run_dev.R does the detach + document_and_reload + run_app cycle
```

Config environment is selected via the `GOLEM_CONFIG_ACTIVE` env var (falls back to `R_CONFIG_ACTIVE`, then `"default"`). Read values with `get_golem_config("key")` — never hardcode paths/credentials; they all live in [inst/golem-config.yml](inst/golem-config.yml) (`default`, `production`, `dev`).

```r
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
run_app()
```

Dependencies are managed with `renv` — run `renv::restore()` to set up the library.

### Production deployment (Windows Server)
Windows Task Scheduler runs [launch_on_browser.R](launch_on_browser.R) via `Rscript.exe` (see [ShinyApp.bat](ShinyApp.bat)). That script detects the server IP via `ipconfig`, serves on **port 3846** bound to that IP, and sinks stderr to `error_log.Rout`. Note its `folder_address` is hardcoded to the production server path.

### Tests
There is **no `testthat` suite**. `Scripts/test-*.R` are ad-hoc exploratory scripts (excluded from the build via `.Rbuildignore`), not an automated test harness. Don't assume `devtools::test()` does anything meaningful.

## Architecture

The app is four dashboard tabs wired in [R/app_server.R](R/app_server.R) and [R/app_ui.R](R/app_ui.R), with a single SQLite connection (`dbase_csi`) opened at startup and passed into modules:

1. **Load file** (`mod_tab_load`) — upload an ACS CSI or Ticket Hour `.xlsx`, validate, split by store. Output flows into shared `rv` reactiveValues (`csi`, `stores`, `csi_type`, `csi_date`).
2. **Send emails** (`mod_tab_send_email`) — generate per-store Excel attachments and SMTP-send them. Consumes the `rv` values.
3. **Cashier Monitoring** (`mod_cashier_monitoring`) — composes three loader sub-modules (`mod_load_cashier_per_store`, `mod_load_moneygram_statement`, `mod_load_viva_per_store`), reconciles them into per-city Excel workbooks, and emails city cashier groups.
4. **Database** (`mod_tab_dbase`, sidebar label "Database", `tabName = "dbase"`) — a `tabsetPanel` of CRUD sub-tabs over reference tables: Store Emails, Cashier Groups Emails, Cashier Groups, and **Moneygram Stores** (`mod_moneygram_stores`, the `Agent ID → Acs store code` mapping). All sub-modules share the same `(id, conn)` signature and `rv$db_trigger` refresh pattern; see "Editable tables" below.

### File naming conventions (Golem)
- `mod_*.R` — Shiny modules, each exporting `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`. Modules are composed by nesting `ns()`-namespaced UIs and passing the DB connection / reactives down.
- `fct_*.R` — pure helper/processing functions (file reading, data cleaning, DB ops, email, Excel generation).
- `app_config.R` (`get_golem_config`, `app_sys`), `run_app.R`, `zzz.R` (`.onLoad`), `golem_utils_*.R` are Golem boilerplate.

### Key data-flow specifics
- **Reading inputs**: `fct_read_helpers.R` has one reader per file type (`read_acs_csi`, `read_ticket_hour`, `read_monitoring_statement`, `read_moneygram_statement`, `read_viva_statement`). Excel/CSV reads go through `safe_readXL` / `safe_readCSV` — `purrr::safely()` wrappers created in `.onLoad` ([R/zzz.R](R/zzz.R)); they return `list(result, error)`, so callers check `is.null(temp$result)`. Cashier-per-store and Moneygram files contain **Greek column headers and store markers** matched via `\uXXXX` unicode escapes (see `names_cashier_per_store`, `store_unicode`) — don't replace these with literal Greek text.
- **Excel output**: report layout is hand-built cell-by-cell with `openxlsx` against loaded template workbooks. The heavy logic is `prepare_store_monitoring()` in [R/fct_prepare_store_monitoring.R](R/fct_prepare_store_monitoring.R) — it writes per-store cash/visa/moneygram/viva blocks with hardcoded column indices (`col_*`) and reconciliation formulas. Edits to template column order must stay in sync with those constants.
- **Internal package data**: `R/sysdata.rda` holds compiled constants (`col_names_ticket`, `names_cashier_per_store`, `db_tables`, `statement_types`, Excel templates). Regenerate it by editing and running [data-raw/create_global_values.R](data-raw/create_global_values.R) (`usethis::use_data(..., internal = TRUE)`) — do **not** hand-edit `sysdata.rda`.

### Editable tables (CRUD pattern)
The Database tab's sub-modules ([R/mod_cashier_groups.R](R/mod_cashier_groups.R) is the cleanest template) all follow one pattern: a `DT::datatable` with single-row selection + the shared `buttons_edit(ns)` Add/Edit/Delete buttons ([R/fct_ui.R](R/fct_ui.R)); data read via `conn %>% tbl(<table>) %>% collect()` gated on `rv$db_trigger`; Add/Edit through a `modalDialog` helper in [R/mod_tab_dbase_fct_helpers.R](R/mod_tab_dbase_fct_helpers.R); writes via `append_data()` (insert) / `DBI::dbExecute()` parameterized UPDATE / `delete_data()`; then bump `rv$db_trigger` to refresh. Toasts use `shinyFeedback::showToast`.

The email/cashier tables key on a generated `uuid`/`uid` column. **`moneygram_stores` is different** ([R/mod_moneygram_stores.R](R/mod_moneygram_stores.R)): no surrogate key — `Agent ID` (REAL) is the natural key, and column names contain spaces so raw SQL must backtick them (`` `Agent ID` ``, `` `Agent name` ``, `` `Acs store code` ``). It is referenced by literal table name (no `db_tables` entry). Agent ID is locked on edit, written as numeric to keep the column REAL (so the startup `as.character()` join in `app_server.R` keeps matching), and the module blocks duplicate Agent IDs and duplicate store codes before saving. `data.frame(..., check.names = FALSE)` is required when building rows because of the spaced column names.

### Persistence & auth
- SQLite DBs live in `DB/` (gitignored): `csi_db.db` (email/cashier tables, path from `db_path`) and `credentials.sqlite` (`shinymanager` users, path from `db_users`). Table names are centralized in `db_tables`.
- DB writes use short-lived connections (`append_to_db`, `check_statement_in_db` in [R/fct_dbase_operations.R](R/fct_dbase_operations.R)); the long-lived `dbase_csi` is for reads and is disconnected in `onStop`.
- Auth via `shinymanager::secure_app` / `secure_server` (passphrase from config); SMTP via `blastula` with credentials resolved through `keyring` (`smtp_creds` config key).

## Gotchas
- [R/app_server.R](R/app_server.R) currently contains a stray `browser()` call (~line 83) inside the moneygram_stores `observe` — a debugging leftover that will halt execution interactively. Remove before relying on that observer.
- `inst/global.R` is dead code ("NOT USED YET") — the live DB connection/onStop logic is in `app_server.R`.
