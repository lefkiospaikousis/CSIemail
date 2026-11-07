# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Full maintainer documentation lives in [docs/handover/](docs/handover/README.md): architecture, module and function reference, DB tables, setup, deployment, and the known-issues list in [07-maintenance-guide.md](docs/handover/07-maintenance-guide.md). Keep it in sync when behaviour changes.

## Overview

CSIemail is a **Golem-structured R Shiny package** for ACS Cyprus. It splits financial reports by store, emails per-store attachments, and produces per-city cashier reconciliation workbooks. Despite being a Shiny app, it is built and loaded as an R **package** (`CSIemail`), so app code lives in `R/` and is loaded with `pkgload::load_all()` / `devtools`, not sourced ad hoc.

## Running & Developing

Always load the package first. Functions are package-internal (`@noRd`) and many are referenced via `CSIemail:::name`.

```r
# Development (from project root)
golem::document_and_reload()   # or pkgload::load_all()
run_app()                       # uses GOLEM_CONFIG_ACTIVE, defaults to "default"

# dev/run_dev.R does the options(golem.app.prod = FALSE) + detach + document_and_reload + run_app cycle
```

Behaviour is controlled by **one switch, the config profile**: `active_config()` ([R/app_config.R](R/app_config.R)) returns `GOLEM_CONFIG_ACTIVE`, else `R_CONFIG_ACTIVE`, else `"default"`. It's used by `get_golem_config()` and by `app_server` for the shinymanager DB and passphrase. Read values with `get_golem_config("key")`. Never hardcode paths or credentials. Profiles: `default`, `production`, `dev`.

Where emails go comes from two config keys, resolved in [R/fct_email_helpers.R](R/fct_email_helpers.R):
- `smtp_creds_source`: `file` means a blastula creds file named by `smtp_creds`, `keyring` means a keyring key
- `cashier_emails_table`: `city_emails` (real cashiers) or `city_emails2` (test list)

`golem.app.prod` / `golem::app_dev()` is only a **fallback** when a machine's config lacks these keys (a warning is logged). Don't reintroduce `app_dev()` checks for behaviour. The sidebar (`mode_badge()` in [R/fct_ui.R](R/fct_ui.R)) and a log line at session start show the active profile, SMTP source and cashier table.

```r
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")   # keyring + real cashier recipients
run_app()
```

**`inst/golem-config.yml` is git-ignored.** Each machine keeps its own copy. The committed template is [inst/golem-config.example.yml](inst/golem-config.example.yml), with placeholders for the passphrase and sender. The production server's copy is maintained by hand by the owner. When you add a config key:
- add it to the template and to the local file
- tell the user it must be added to the server copy by hand
- make the code tolerate it being missing (return `NULL` → fallback + warning, like `smtp_creds_source()`)

Never write real secrets into the template. The sidebar version comes from `DESCRIPTION` (`utils::packageVersion`), not from `golem_version`.

Dependencies are managed with `renv` (`renv.lock` records R 4.2.3). Run `renv::restore()` to set up the library. Some packages are used but missing from `DESCRIPTION` Imports (`zip`, `cli`, `here`, `dbplyr`). When adding a dependency, run `usethis::use_package()` and then `renv::snapshot()`.

### Production deployment (Windows Server)
The server keeps its own git-ignored `inst/golem-config.yml`. If it's a git checkout, the first pull after the file was un-tracked deletes it, so it must be backed up and restored around that pull.

[ShinyApp.bat](ShinyApp.bat) runs [launch_on_browser.R](launch_on_browser.R) via `Rscript.exe`. Whether Task Scheduler or an NSSM service supervises it is unconfirmed. The script detects the server IP via `ipconfig`, `runApp()`s the hard-coded production folder (which executes `app.R`) on **port 3846** bound to that IP, and sinks R messages to `error_log.Rout` (overwritten on restart). `folder_address` and the log path are hardcoded to the production server. The `.bat` points at R-4.1.0, which doesn't match `renv.lock`.

### Tests
A `testthat` (edition 3) suite lives in [tests/testthat/](tests/testthat/). Run it with `devtools::test()` or `testthat::test_local()`. There's no CI yet. Coverage:
- the four CRUD modules (Edit→Cancel→Add, normal Edit/Add, duplicate Agent ID)
- the config-driven email mode
- `find_unmatched()`
- `mod_cashier_monitoring` refresh and unmatched reporting

Conventions:
- **Never use the real `DB/` files.** `local_fixture_db()` in [tests/testthat/helper-fixtures.R](tests/testthat/helper-fixtures.R) builds a synthetic SQLite DB with the same schema and cleans up after itself.
- Use `read_tbl()` rather than `DBI::dbReadTable()`, because of the spaced Moneygram column names.
- Mock `get_golem_config()` / `cashier_emails_table()` with `local_mocked_bindings()`, so tests don't depend on the machine's git-ignored config.
- Module logic goes through `shiny::testServer()`. Don't advance timers in `mod_cashier_monitoring` tests: that would write report files.
- Tests needing the git-ignored `SampleData/` must `skip_if_not()` when it's absent.

`Scripts/test-*.R` are ad-hoc exploratory scripts (git-ignored), not part of the suite.

## Architecture

The app is four dashboard tabs wired in [R/app_server.R](R/app_server.R) and [R/app_ui.R](R/app_ui.R). `app_server` opens one SQLite connection (`dbase_csi`) **per session**, passes it into the modules, and disconnects it via `shiny::onStop` when the session ends. There is no pool.

1. **Load file** (`mod_tab_load`): upload an ACS CSI or Ticket Hour `.xlsx`, validate, split by store. Output flows into the shared `rv` reactiveValues in `app_server` (`csi`, `stores`, `csi_type`, `csi_date`).
2. **Send emails** (`mod_tab_send_email`): generate per-store Excel attachments (in `tempdir()`) and SMTP-send them. Consumes the `rv` values, and re-reads the `emails` table when the app-level `rv$db_trigger` changes.
3. **Cashier Monitoring** (`mod_cashier_monitoring`): composes three loader sub-modules (`mod_load_cashier_per_store`, `mod_load_moneygram_statement`, `mod_load_viva_per_store`), reconciles them into per-city Excel workbooks (a `reactiveTimer` state machine, one city per tick), and emails city cashier groups.
   - Its recipient and `cashier_groups` reads are driven by local counters (`emails_refresh`, `groups_refresh`). These are bumped on Send, Generate and the "Re-check against the Database" link, so Database-tab edits are picked up without a reload.
   - `find_unmatched()` ([R/fct_unmatched.R](R/fct_unmatched.R)) lists rows that won't appear in any report (stores missing from `cashier_groups`, unmapped Moneygram agents). The list appears in a "Not matched" tab and in the Generate modal.
4. **Database** (`mod_tab_dbase`, sidebar label "Database", `tabName = "dbase"`): a `tabsetPanel` of CRUD sub-tabs over the reference tables: Store Emails (implemented in `mod_tab_dbase` itself), Cashier Groups Emails, Cashier Groups, and **Moneygram Stores** (`mod_moneygram_stores`, the `Agent ID → Acs store code` mapping). All sub-modules share the same `(id, conn)` signature and `rv$db_trigger` refresh pattern; see "Editable tables" below. Only `mod_tab_dbase`'s own trigger is propagated to `app_server`.

### File naming conventions (Golem)
- `mod_*.R`: Shiny modules, each exporting `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`. Modules are composed by nesting `ns()`-namespaced UIs and passing the DB connection / reactives down. Children hand data back by returning a `reactiveValues` that the parent watches with `observeEvent`. `mod_downloadTable` is the exception: it uses the legacy `callModule()` signature.
- `fct_*.R`: helper/processing functions (file reading, data cleaning, DB ops, email, Excel generation). Some helpers live in `mod_*_fct_helpers.R`.
- `app_config.R` (`get_golem_config`, `app_sys`), `run_app.R`, `zzz.R` (`.onLoad`), `golem_utils_*.R` are Golem boilerplate. `golem_utils_server.R` defines `rv <- shiny::reactiveValues`. A module that forgets to define a local `rv` silently gets this function.

### Key data-flow specifics
- **Reading inputs**: `fct_read_helpers.R` has one reader per file type (`read_acs_csi`, `read_ticket_hour`, `read_monitoring_statement`, `read_moneygram_statement`, `read_viva_statement`). Most Excel/CSV reads go through `safe_readXL` / `safe_readCSV`: `purrr::safely()` wrappers created in `.onLoad` ([R/zzz.R](R/zzz.R)). They return `list(result, error)`, so callers check `is.null(temp$result)`. `read_moneygram_statement` calls `readxl` directly. Cashier-per-store and Moneygram files contain **Greek column headers and store markers** matched via `\uXXXX` unicode escapes (see `names_cashier_per_store`, `store_unicode`). Don't replace these with literal Greek text. The one existing literal, `Τιμολόγιο` in `process_csi`, is an encoding risk.
- **Report date**: the cashier monitoring date comes from the VIVA file (it must contain exactly one `Day`). The Moneygram file-name date is ignored.
- **Excel output**: report layout is hand-built cell by cell with `openxlsx` against template workbooks loaded from the config paths (`acs_template`, `th_template`, `cashier_template` in `data-raw/`). The heavy logic is `prepare_store_monitoring()` in [R/fct_prepare_store_monitoring.R](R/fct_prepare_store_monitoring.R). It writes per-store cash/visa/moneygram/viva blocks with hardcoded column indices (`col_*`) and formulas that rely on **named ranges in `data-raw/template_report.xlsx`** (`tamiaki`, `cash_received`, `visa_received`, `cheques_received`, `moneygram_owed`, `moneygram_received`, …). Template columns, named ranges and the constants must stay in sync. The caller passes `city = city`, which only reaches the `city_name` parameter through partial argument matching.
- **Internal package data**: `R/sysdata.rda` holds compiled constants (`col_names_ticket`, `vars_sum_ticketHour`, `names_cashier_per_store`, `db_tables`, `statement_types`, plus unused `xl_template_*` workbooks). Regenerate it by editing and running [data-raw/create_global_values.R](data-raw/create_global_values.R) (`usethis::use_data(..., internal = TRUE)`). Do **not** hand-edit `sysdata.rda`. That script currently fails: it references `xl_template_store_monitoring` without defining it.

### Editable tables (CRUD pattern)
The Database tab's sub-modules ([R/mod_cashier_groups.R](R/mod_cashier_groups.R) is the cleanest template) all follow one pattern:
- a `DT::datatable` with single-row selection, plus the shared `buttons_edit(ns)` Add/Edit/Delete buttons ([R/fct_ui.R](R/fct_ui.R))
- data read via `conn %>% tbl(<table>) %>% collect()`, gated on `rv$db_trigger`
- Add and Edit through a `modalDialog` helper in [R/mod_tab_dbase_fct_helpers.R](R/mod_tab_dbase_fct_helpers.R)
- writes on the shared `conn` via `append_data()` (insert), a parameterized `DBI::dbExecute()` UPDATE, or `delete_data()`
- then a bump of `rv$db_trigger` to refresh
- toasts via `shinyFeedback::showToast`

**Insert vs update** is decided by `rv$*_to_edit` (`store_to_edit` / `city_to_edit`): NULL means INSERT, otherwise UPDATE that row. Edit sets it. The `btn_add` observer **must reset it to NULL**, because Cancel is a plain `modalButton` the server never sees. Without the reset, Edit → Cancel → Add overwrites a row (fixed 2026-11-07). `form_data()` reads `rv$*_to_edit` without `isolate()`, so the key can't be served stale. Keep both details in any new CRUD module.

The email/cashier tables key on a generated `uuid`/`uid` column, and UPDATE/DELETE match on it. Rows inserted outside the app may have a NULL key: in the local DB, 36 of 37 `cashier_groups` rows do, and Edit/Delete silently match nothing for them. **`moneygram_stores` is different** ([R/mod_moneygram_stores.R](R/mod_moneygram_stores.R)):
- There's no surrogate key. `Agent ID` (REAL) is the natural key.
- Column names contain spaces, so raw SQL must backtick them (`` `Agent ID` ``, `` `Agent name` ``, `` `Acs store code` ``).
- It's referenced by literal table name (no `db_tables` entry). The cashier recipient table name comes from `cashier_emails_table()`.
- Agent ID is locked on edit, and written as numeric to keep the column REAL, so the `as.character()` join in `get_moneygram_stores()` ([R/fct_dbase_operations.R](R/fct_dbase_operations.R)) keeps matching. That function re-reads the mapping each time a Moneygram statement loads.
- The module blocks duplicate Agent IDs and duplicate store codes before saving.
- `data.frame(..., check.names = FALSE)` is required when building rows because of the spaced column names. `DBI::dbReadTable()` also mangles these names (`Agent.ID`), so use `dbGetQuery("SELECT * …")` or `tbl()` instead.

### Persistence & auth
- SQLite DBs live in `DB/` (git-ignored, not in the repo): `csi_db.db` (email/cashier tables, path from `db_path`) and `credentials.sqlite` (`shinymanager` users, path from `db_users`). Most table names are centralized in `db_tables`. Paths are relative to the working directory. A wrong path makes RSQLite silently create an empty DB, which then fails with "no such table".
- All live reads and CRUD writes use the per-session `dbase_csi` connection. `append_to_db` and `check_statement_in_db` in [R/fct_dbase_operations.R](R/fct_dbase_operations.R) open short-lived connections, but they're only called from the dead `add_to_db` observer in `mod_cashier_monitoring`.
- Auth is via `shinymanager::secure_app(enable_admin = TRUE)` / `secure_server` (passphrase from config). There are no app-level roles. SMTP is via `blastula`, with credentials from `mail_credentials()`: a creds file or a `keyring` key, chosen by the `smtp_creds_source` config key (id in `smtp_creds`).

## Gotchas
- `inst/global.R` is dead code ("NOT USED YET"). The live DB connection/onStop logic is in `app_server.R`.
- Other dead code: `mod_user_management` (never mounted, broken), `fct_data_validation.R`, `anonymise*`, the `add_to_db` observer in `mod_cashier_monitoring`, and the unrendered `output$statement` in the loader modules. Full list in [docs/handover/07-maintenance-guide.md](docs/handover/07-maintenance-guide.md#dead-code-safe-to-remove-once-confirmed).
- `www/getIP.js` is bundled automatically and makes every browser call `http://ipinfo.io`. The result is unused.
- `save_csi_to_disk()` calls `Sys.setlocale()` with Windows locale names, which affects the whole R process.
- Cashier reports still exclude stores missing from `cashier_groups` and Moneygram agents missing from `moneygram_stores`. They're now listed by `find_unmatched()`, so keep that function in step if you change how statements are joined. The Moneygram mapping is applied when the file is loaded, so after a mapping fix the file must be loaded again.
- There are no `browser()` calls in `R/`. Don't commit any.
- Log files (`*.log`, `*.Rout`, e.g. `error_log.Rout`, `service.*.log`) are git-ignored. Never commit them, and don't use `git update-index --skip-worktree` to hide tracked files: three logs had that flag, which hid them from `git status` while they stayed tracked.
