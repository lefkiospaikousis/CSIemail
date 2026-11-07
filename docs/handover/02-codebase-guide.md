# 02 – Codebase guide

## Repository layout

| Path | In git? | In package build? | Purpose |
|---|---|---|---|
| `R/` | ✅ | ✅ | All app code (modules, fct, golem boilerplate) and `sysdata.rda` |
| `inst/golem-config.yml` | ❌ (`.gitignore`, one copy per machine) | ✅ | Config profiles: `default`, `production`, `dev`. **Required**: the package won't load without it |
| `inst/golem-config.example.yml` | ✅ | ✅ | Template for the above, with placeholders for the passphrase and sender |
| `inst/app/www/` | ✅ | ✅ | CSS, JS and images, served under `www/` |
| `inst/global.R` | ✅ | ✅ | **Dead code**: says "NOT USED YET" ([inst/global.R:3](../../inst/global.R#L3)) |
| `man/` | ✅ | ✅ | roxygen output for the few documented functions |
| `data-raw/` | ✅ | ❌ (`.Rbuildignore`) | Excel templates **read at runtime** (paths in config), the scripts that build `sysdata.rda` and seed the `emails` table, and the source spreadsheets |
| `dev/` | ✅ | ❌ | golem dev scripts. Only `run_dev.R` is actually used |
| `app.R` | ✅ | ❌ | golem entry point for RStudio "Run App": sets prod mode and the `production` profile, then calls `run_app()`. The production service uses the root `run_app.R` instead |
| `run_app.R` (root) | ✅ | ❌ (`.Rbuildignore`) | Production launcher, started by the NSSM service as `Rscript.exe run_app.R` ([06-deployment.md](06-deployment.md)). Not the same file as `R/run_app.R` |
| `logs/` | ❌ (`.gitignore`) | ❌ | `logs/app.log`, written by the launcher |
| `ShinyApp.bat`, `launch_on_browser.R` | ✅ | ✅/— | **Deprecated, not used** since 2026-11-08. The old `.bat` launcher, kept with a deprecation header for reference and rollbacks |
| `renv.lock`, `renv/`, `.Rprofile` | ✅ (`renv/activate.R`, `settings.json`, `.gitignore`; the library itself is not) | ❌ | Dependency management |
| `.Renviron` | ✅ | — | Sets `RENV_PATHS_LIBRARY` only |
| `DB/` | ❌ (`.gitignore`) | ❌ | SQLite databases and CSV copies |
| `SampleData/` | ❌ | ❌ | Sample input files, plus `CashierReports/`, the output folder for the default and dev profiles |
| `Scripts/` | ❌ | ❌ | Ad-hoc exploration scripts and the user-DB creation script |
| `error_log.Rout`, `service.*.log`, `launch_on_browser.log` | ❌ (`*.log` and `*.Rout` are git-ignored; untracked on 2026-11-08) | — | Logs of the old `.bat` launcher. No longer written; safe to delete |
| `mail_improvast` | ❌ | — | blastula SMTP credentials file for dev. **Contains a secret** |

## `R/`, file by file

### Boot and wiring

| File | Purpose |
|---|---|
| [run_app.R](../../R/run_app.R) | Exported `run_app()`. Wraps `app_ui` in `shinymanager::secure_app` (login page with logo, admin mode on) and calls `golem::with_golem_options` |
| [app_ui.R](../../R/app_ui.R) | `app_ui()`: hidden `remote_addr` input, then a `dashboardPage` with 4 `menuItem`s and `tabItems`. `golem_add_external_resources()` adds the favicon, the `www` bundle, `useShinyjs`, `use_waiter`, `useShinyFeedback` |
| [app_server.R](../../R/app_server.R) | `app_server()`: `onStop` disconnect, top-level `rv`, shinymanager `secure_server`, opens the SQLite connection, unused IP reactives, mounts the 4 tab modules |
| [app_config.R](../../R/app_config.R) | `app_sys()` (wraps `system.file`), `active_config()` (profile name), `get_golem_config()` |
| [zzz.R](../../R/zzz.R) | `.onLoad` builds `safe_readXL` / `safe_readCSV`. `load_hidden_packages()` exists only so renv records `dbplyr` |
| [_disable_autoload.R](../../R/_disable_autoload.R) | Turns off Shiny's `R/` autoloading |
| [golem_utils_ui.R](../../R/golem_utils_ui.R), [golem_utils_server.R](../../R/golem_utils_server.R) | golem boilerplate. The parts actually used are `col_12()`, `col_10()` and the `rv` alias |
| `sysdata.rda` | Internal constants. Regenerate from [data-raw/create_global_values.R](../../data-raw/create_global_values.R) and **never hand-edit** it (see [07](07-maintenance-guide.md#regenerate-rsysdatarda)) |

### Modules

| File | Module | Tab |
|---|---|---|
| [mod_tab_load.R](../../R/mod_tab_load.R) | `mod_tab_load` | 1. Load file |
| [mod_tab_send_email.R](../../R/mod_tab_send_email.R) | `mod_tab_send_email` | 2. Send emails |
| [mod_tab_send_emails_fct_helpers.R](../../R/mod_tab_send_emails_fct_helpers.R) | *(helpers)* | Builds per-store workbooks and the confirmation modal |
| [mod_cashier_monitoring.R](../../R/mod_cashier_monitoring.R) | `mod_cashier_monitoring` | Cashier Monitoring |
| [mod_load_cashier_per_store.R](../../R/mod_load_cashier_per_store.R) | `mod_load_cashier_per_store` | child of Monitoring |
| [mod_load_moneygram_statement.R](../../R/mod_load_moneygram_statement.R) | `mod_load_moneygram_statement` | child of Monitoring |
| [mod_load_viva_per_store.R](../../R/mod_load_viva_per_store.R) | `mod_load_viva_per_store` | child of Monitoring |
| [mod_tab_dbase.R](../../R/mod_tab_dbase.R) | `mod_tab_dbase` | Database (and its Store Emails sub-tab) |
| [mod_cachier_groups_emails.R](../../R/mod_cachier_groups_emails.R) | `mod_cachier_groups_emails` | Database › Cashier Groups Emails |
| [mod_cashier_groups.R](../../R/mod_cashier_groups.R) | `mod_cashier_groups` | Database › Cashier Groups |
| [mod_moneygram_stores.R](../../R/mod_moneygram_stores.R) | `mod_moneygram_stores` | Database › Moneygram Stores |
| [mod_tab_dbase_fct_helpers.R](../../R/mod_tab_dbase_fct_helpers.R) | *(helpers)* | Entry or delete modals for every CRUD table, plus `append_data()` and `delete_data()` |
| [mod_downloadTable.R](../../R/mod_downloadTable.R) | `mod_downloadTable` | Reusable "download as .xlsx" link |
| [mod_user_management.R](../../R/mod_user_management.R) | `mod_user_management` | **Unused and broken**: it uses an undefined `conn` and calls `entry_form()` with a `user` argument it doesn't accept |

### Business logic (`fct_*`)

| File | Domain |
|---|---|
| [fct_read_helpers.R](../../R/fct_read_helpers.R) | Reading and cleaning every upload type, plus totals and date helpers |
| [fct_prepare_store_monitoring.R](../../R/fct_prepare_store_monitoring.R) | Writes the per-city reconciliation workbook |
| [fct_email_helpers.R](../../R/fct_email_helpers.R) | Where emails go, driven by config: `mail_credentials()`, `smtp_creds_source()`, `cashier_emails_table()`, `app_mode_info()` |
| [fct_unmatched.R](../../R/fct_unmatched.R) | `find_unmatched()` and `unmatched_warning_ui()`: rows of the loaded statements that won't appear in any report |
| [fct_email_cashier.R](../../R/fct_email_cashier.R) | `email_cashier()`: sends one city report |
| [fct_dbase_operations.R](../../R/fct_dbase_operations.R) | `get_moneygram_stores()` (live), plus `append_to_db()` and `check_statement_in_db()` (only called from dead code) |
| [fct_notifications.R](../../R/fct_notifications.R) | Toast, feedback and modal helpers |
| [fct_global.R](../../R/fct_global.R) | `moneygram_date()`, icon helpers |
| [fct_ui.R](../../R/fct_ui.R) | `buttons_edit(ns)`: the Add/Edit/Delete button row. `mode_badge()`: the sidebar block showing the profile and where emails go |
| [fct_data_validation.R](../../R/fct_data_validation.R) | **Dead**: `check_filetype()` / `check_var_names()` refer to objects that don't exist (`accepted_files`, `column_names`) |

---

## Modules in detail

### `mod_tab_load`: load and split a CSI file
[R/mod_tab_load.R](../../R/mod_tab_load.R) · mounted as `tab_load_ui_1`

| | |
|---|---|
| **Purpose** | Upload an ACS CSI or Ticket Hour file, validate it, clean it, and preview it per store |
| **UI** | `radioButtons csi_type` ("ACS CSI" / "Ticket Hour Sales"), `fileInput file_csi`, then a box (hidden until a file loads) with `selectInput store`, `DTOutput store_csi`, and the download link `down_csi` |
| **Server args** | `id` only |
| **Returns** | `reactiveValues` with `csi` (clean tibble), `stores` (unique `store_code`), `csi_date`, `csi_type` ([L49-54](../../R/mod_tab_load.R#L49-L54), returned at [L207](../../R/mod_tab_load.R#L207)) |
| **Side effects** | None. Read-only |
| **Depends on** | `read_acs_csi`, `read_ticket_hour`, `process_csi`, `process_th`, `get_csi_date`, `add_totals`, `error_csi_file`, `col_names_ticket`, `vars_sum_ticketHour` |
| **Key logic** | the `csi` reactive ([L62-139](../../R/mod_tab_load.R#L62-L139)): extension check → read → date → process. `store_csi` ([L161-189](../../R/mod_tab_load.R#L161-L189)) adds a Total row and, for TH, relabels columns using `col_names_ticket` |

### `mod_tab_send_email`: email each store its CSI
[R/mod_tab_send_email.R](../../R/mod_tab_send_email.R) · mounted as `tab_email_ui_1`

| | |
|---|---|
| **Purpose** | Join the loaded CSI to the store email list, let the user pick stores, build one Excel per store, and send them |
| **UI** | (hidden until data exists) `actionButton send_emails`, `htmlOutput csi_type_UI` (dates, type, store count), `reactableOutput stores` (multi-select, with a send-status column) |
| **Server args** | `conn`, `trigger` (reactive), and positional `csi_type`, `csi`, `csi_date` (reactives) |
| **Returns** | nothing |
| **Side effects** | **reads** `emails`. **Writes** temporary `.xlsx` files to `tempdir()` and deletes them after sending. **Sends** SMTP email |
| **Depends on** | `save_csi_to_disk`, `as_excel_wb`, `verify_send`, `mail_credentials`, `add_totals`, `message_failure`, `praise::praise()` (in the email footer) |
| **Key logic** | `csi_by_store` ([L91-124](../../R/mod_tab_send_email.R#L91-L124)) nests the data by store and joins emails. `input$send_emails` opens the confirmation modal ([L209-246](../../R/mod_tab_send_email.R#L209-L246)). `input$confirm_send` saves the files, composes the email and calls `purrr::safely(smtp_send)` for each address ([L250-419](../../R/mod_tab_send_email.R#L250-L419)) |

### `mod_cashier_monitoring`: build and email per-city reconciliation reports
[R/mod_cashier_monitoring.R](../../R/mod_cashier_monitoring.R) · mounted as `cashier_monitoring_1`

| | |
|---|---|
| **Purpose** | Collect the three statements, generate one workbook per city, then offer a ZIP download and per-city emails |
| **UI** | left column: the 3 loader modules. Right: a tabset with 3 `DTOutput`s (`statement_cashier_per_store`, `statement_moneygram`, `statement_viva_per_store`). Below: `statement_date_report` and `actionButton generate_reports` (disabled by default). The modal (built in the server at [L630-L663](../../R/mod_cashier_monitoring.R#L630-L663)) has the progress area, `downloadButton download_all_reports`, `checkboxGroupInput cities_to_send`, `actionButton send_emails`, `actionButton close_modal` |
| **Server args** | `dbase_csi` (connection) |
| **Returns** | `rv`, which here is the package-level `reactiveValues` **function**, not data. Nothing uses the return value |
| **Side effects** | **reads** `cashier_groups` and the table named by config key `cashier_emails_table` (`city_emails`, or the test list `city_emails2`). Both are re-read on Generate, Send and Re-check. **Writes** `TAMEIAKH_<city>-<date>.xlsx` to `path_store_reports`. **Sends** SMTP email. **Serves** a ZIP download |
| **Unmatched rows** | `unmatched()` = `find_unmatched()` over the three statements and `cashier_groups`. It's shown in the "Not matched" tab (`output$unmatched`, with the `recheck_unmatched` link), as a notification when rows appear, and as `unmatched_warning_ui()` at the top of the Generate modal ([R/mod_cashier_monitoring.R:150-195](../../R/mod_cashier_monitoring.R#L150-L195), [L633](../../R/mod_cashier_monitoring.R#L633)) |
| **Depends on** | `prepare_store_monitoring`, `email_cashier`, `mail_credentials`, `names_cashier_per_store`, icon helpers, `zip::zip` |
| **Key logic** | enable rule ([L206-L215](../../R/mod_cashier_monitoring.R#L206-L215)), the timer-driven generation state machine ([L361-L491](../../R/mod_cashier_monitoring.R#L361-L491)), per-city email sending ([L494-L580](../../R/mod_cashier_monitoring.R#L494-L580)), ZIP ([L583-L608](../../R/mod_cashier_monitoring.R#L583-L608)) |
| **Dead code** | the `input$add_to_db` observer ([L690-L764](../../R/mod_cashier_monitoring.R#L690-L764)). No UI input called `add_to_db` exists, and it refers to an `rv` that isn't defined in this scope |

### Loader sub-modules (`mod_load_*`)
All three follow the same shape:
- a `fileInput file_statement` with an extension check
- a `tryCatch` around the reader
- `shinyFeedback` success or failure messages
- a hidden download link (`down_statement`)
- a returned `rv` with `statement`, `statement_type` and `statement_date`

Each one also defines `output$statement`, which **no UI element renders**. The parent displays the data instead.

| Module | Args | Accepts | Reader | Extra processing | `statement_date` |
|---|---|---|---|---|---|
| [mod_load_cashier_per_store](../../R/mod_load_cashier_per_store.R) | `id` | xls, xlsx | `read_monitoring_statement` | none | never set |
| [mod_load_moneygram_statement](../../R/mod_load_moneygram_statement.R) | `id, conn` | xls, xlsx | `read_moneygram_statement` | `left_join(get_moneygram_stores(conn), by = "agent_id")`, moves `store` first ([L125-132](../../R/mod_load_moneygram_statement.R#L125-L132)) | from the file **name** via `moneygram_date()`; the parent ignores it |
| [mod_load_viva_per_store](../../R/mod_load_viva_per_store.R) | `id` | csv | `read_viva_statement` | rejects the file unless there's exactly one distinct `Day`, keeps `Transaction Type == "SA"`, strips a trailing `-1` from the store code, renames it to `store` ([L108-142](../../R/mod_load_viva_per_store.R#L108-L142)) | the single `Day`. **This is the report date** |

### `mod_tab_dbase` and its CRUD sub-modules
All four CRUD screens share one pattern:
- a `DT::datatable` with single selection and `filter = "top"`
- the `buttons_edit(ns)` button row
- a modal from `mod_tab_dbase_fct_helpers.R` for Add and Edit
- `append_data()` for inserts, `DBI::dbExecute()` for a parameterised UPDATE, `delete_data()` for deletes
- after any write, a bump of the local `rv$db_trigger`, which re-reads the table

| Module | Table | Key | Modal(s) | Notes |
|---|---|---|---|---|
| [mod_tab_dbase](../../R/mod_tab_dbase.R) (Store Emails) | `emails` | `uid` | `entry_form`, `verify_delete` | Returns `rv` (`db_trigger`, `store_to_edit`) to `app_server`. Also hosts the tabset |
| [mod_cachier_groups_emails](../../R/mod_cachier_groups_emails.R) | `city_emails` | `uuid` | `entry_form_city`, `verify_delete_city` | Always edits `city_emails`, never `city_emails2` |
| [mod_cashier_groups](../../R/mod_cashier_groups.R) | `cashier_groups` | `uuid` | `entry_form_cashier_group`, `verify_delete_store` | |
| [mod_moneygram_stores](../../R/mod_moneygram_stores.R) | `moneygram_stores` | natural key `Agent ID` (REAL) | `entry_form_moneygram`, `verify_delete_moneygram` | The only module with real validation: Agent ID numeric and unique, store code required and unique (case-insensitive). The Agent ID is locked when editing. Column names contain spaces, so SQL must backtick them |

How a submit decides between insert and update: `rv$*_to_edit` NULL → INSERT, otherwise UPDATE that row. Edit sets it. The Add button and a successful edit clear it. (Before 2026-11-07 Add didn't clear it, so Edit → Cancel → Add overwrote a row; that's B1 in [07](07-maintenance-guide.md#correctness-bugs).) Rows with a NULL key can't be edited or deleted (B1b).

### `mod_downloadTable`
[R/mod_downloadTable.R](../../R/mod_downloadTable.R). The UI is a `downloadLink` with class `downExcel`. The server is old-style `function(input, output, session, table_name, the_table)`, called with `callModule()`. `the_table` must be a reactive, **passed without `()`**. It writes `<table_name>.xlsx` with `writexl`.

---

## Important functions by domain

### 1. Reading and cleaning uploads ([R/fct_read_helpers.R](../../R/fct_read_helpers.R))

| Function | Signature → returns | What it does | Called from | Gotchas |
|---|---|---|---|---|
| `read_acs_csi` ([L42](../../R/fct_read_helpers.R#L42)) | `(path)` → tibble or `NULL` | `safe_readXL`, renames `...1`→`date` and `...2`→`store_code`, converts `AWB` to character | `mod_tab_load` | `stopifnot` on the extension. Depends on readxl's `...1` naming for unnamed columns |
| `process_csi` ([L220](../../R/fct_read_helpers.R#L220)) | `(csi)` → tibble | Takes the text after `": "` in `store_code`, trims it, fills it down, drops the first 2 rows and the first column, drops rows with no invoice number (the source's subtotal rows), converts `AWB:last_col()` to double | `mod_tab_load` | The invoice column is matched by the **literal Greek name** `Τιμολόγιο` at [L243](../../R/fct_read_helpers.R#L243), not by a `\u` escape. That's an encoding risk on non-UTF-8 sessions |
| `read_ticket_hour` ([L4](../../R/fct_read_helpers.R#L4)) | `(path)` → tibble or `NULL` | Reads with no headers, keeps the first 15 columns, names them from `col_names_ticket`, drops 7 columns. Returns `NULL` if `os`, `debit` and `credit` are all empty (read as logical) | `mod_tab_load` | Column **position** matters, not headers |
| `process_th` ([L267](../../R/fct_read_helpers.R#L267)) | `(csi)` → tibble | `date` → `ymd` → `"%d/%m/%Y"` text | `mod_tab_load` | |
| `get_csi_date` ([L295](../../R/fct_read_helpers.R#L295)) | `(csi, csi_type)` → chr / named vector | ACS: the text after `": "` in the first `date` cell. TH: `c(from = min, to = max)` | `mod_tab_load`, which calls it **before** processing | Send-email parses ACS dates with `dmy` and TH dates with `ymd` |
| `add_totals` ([L198](../../R/fct_read_helpers.R#L198)) | `(dta, ...)` → tibble | `janitor::adorn_totals(where = "row", name = "Total")` over the given columns | load and send-email | ACS: every column except `AWB`. TH: `os`, `debit`, `credit` |
| `read_monitoring_statement` ([L66](../../R/fct_read_helpers.R#L66)) | `(path)` → tibble or `NULL` | Cashier per Store. Renames column 1 to `courier` and requires the Greek total-cash and total-card headers. Rows starting `Κατάστημα: ` ("Store: ") give the `store` value, which is filled down; those rows and the NA-courier total rows are then dropped | `mod_load_cashier_per_store` | Uses `\u` escapes (`store_unicode`, `names_cashier_per_store`). Typo `collpase` in the error message |
| `read_moneygram_statement` ([L126](../../R/fct_read_helpers.R#L126)) | `(path)` → tibble(`agent_id`, `agent_name`, `total`) | Keeps the rows labelled "Agent ID", "Agent Name" and "TOTAL, EUR" and takes them in groups of 3. The total comes from column 8. `(x)` is treated as negative | `mod_load_moneygram_statement` | Uses `readxl` directly (not `safe_readXL`), and doesn't check the extension. Assumes a strict ID/Name/Total triplet order |
| `read_viva_statement` ([L161](../../R/fct_read_helpers.R#L161)) | `(path)` → tibble or `NULL` | `safe_readCSV`. Requires `Transaction Type`, `ACS Account Code/Store`, `Amount` and `Day`, and returns only those | `mod_load_viva_per_store` | |
| `moneygram_date` ([R/fct_global.R:10](../../R/fct_global.R#L10)) | `(path)` → Date | The first 8-digit run in the file name, parsed `dmy` (e.g. `…24092025.xls`) | `mod_load_moneygram_statement` | The result isn't used downstream |
| `anonymise` / `anonymise2` ([L316](../../R/fct_read_helpers.R#L316)) | | Masks 50% of characters | nowhere (commented out) | dead |

### 2. Per-store CSI export and email ([R/mod_tab_send_emails_fct_helpers.R](../../R/mod_tab_send_emails_fct_helpers.R))

| Function | Signature → returns | What it does | Gotchas |
|---|---|---|---|
| `save_csi_to_disk` ([L1](../../R/mod_tab_send_emails_fct_helpers.R#L1)) | `(dta, csi_type, csi_date)` → invisible | Needs `store_code`, `store_name`, `data` and `filename` columns. Builds a workbook for each row and saves it | Switches the process locale to `"greek"`, then back to `"English_United Kingdom"`. Those are **Windows-only** locale names |
| `as_excel_wb` ([L32](../../R/mod_tab_send_emails_fct_helpers.R#L32)) | `(dta, store_code, store_name, csi_type, csi_date)` → openxlsx Workbook | Loads the `acs_template` or `th_template` from config. Writes the data table at row 8, the store name and code at E5:E6, a timestamp in EET at B3, the date(s) at B5:B6, and a bold total line 3 rows below the table | ACS total = the last cell of the last column. TH total = the `os` total. More than 2 dates → error |
| `verify_send` ([L115](../../R/mod_tab_send_emails_fct_helpers.R#L115)) | `(session, dta)` → modalDialog | "You are about to send N email(s) to M store(s)", with an `OK Send!` button → `confirm_send` | |

### 3. Cashier reconciliation workbook ([R/fct_prepare_store_monitoring.R](../../R/fct_prepare_store_monitoring.R))

`prepare_store_monitoring(wb, dta_cashier, dta_moneygram, dta_viva, city_name = 'Tamiaki')` ([L8](../../R/fct_prepare_store_monitoring.R#L8)) changes `wb` **in place** (openxlsx workbooks are reference objects) and returns nothing useful. The caller loads a fresh template for each city, because reusing one carries state over between cities ([R/mod_cashier_monitoring.R:407-410](../../R/mod_cashier_monitoring.R#L407-L410)).

- **Inputs:**
  - `dta_cashier(store, courier, total_cash, total_card)`
  - `dta_moneygram(store, total, …)`
  - `dta_viva(store, total)`
- **Caller:** `mod_cashier_monitoring` passes `city = city` ([L412-L418](../../R/mod_cashier_monitoring.R#L412-L418)). That only works through **partial argument matching** to `city_name`, so don't add another parameter that starts with `city`.
- **Layout:** described in [04-functionality.md](04-functionality.md#the-generated-workbook-tameiakh_cityxlsx). Column constants are at [L52-67](../../R/fct_prepare_store_monitoring.R#L52-L67), and the formulas use the template's named ranges.

`find_unmatched(cashier = NULL, moneygram = NULL, viva = NULL, groups)` ([R/fct_unmatched.R:16](../../R/fct_unmatched.R#L16)) is a pure function, with no Shiny inside, so it's easy to test. It returns a tibble with `statement`, `store`, `detail` and `amount` for everything the report generation would drop:
- stores of each statement that aren't in `groups$store`, with the amount summed per store (cashier total cash, Moneygram total, VIVA `Amount`)
- Moneygram rows whose agent has no store mapping (`store` is NA)

`NULL` statements are skipped. `unmatched_warning_ui()` ([L73](../../R/fct_unmatched.R#L73)) turns the result into the alert box shown in the Generate modal.

### 4. Email

| Function | What it does | Gotchas |
|---|---|---|
| `mail_credentials()` ([R/fct_email_helpers.R:15](../../R/fct_email_helpers.R#L15)) | `smtp_creds_source()` == `"file"` → `blastula::creds_file(<smtp_creds>)`, a file in the working directory. `"keyring"` → `blastula::creds_key(<smtp_creds>)`, the OS keyring | The From address is the credential's `user`, so the config key `sender` is never used |
| `smtp_creds_source()` / `cashier_emails_table()` ([L32](../../R/fct_email_helpers.R#L32), [L54](../../R/fct_email_helpers.R#L54)) | Read the config keys `smtp_creds_source` / `cashier_emails_table` | If a key is missing (old config), they fall back to the old `golem::app_dev()` logic and log a warning (`warn_missing_config_key()`) |
| `app_mode_info()` ([L70](../../R/fct_email_helpers.R#L70)) | `list(profile, smtp_source, cashier_table, test_recipients)` | Used by `mode_badge()` and the startup log line in `app_server` |
| `email_cashier(city, recipients, path_attachement, mail_credentials)` ([R/fct_email_cashier.R:8](../../R/fct_email_cashier.R#L8)) | One email to every recipient of a city. Subject `Accounts report for <city>`, with the attachment | Recipients are trimmed. Errors propagate to the caller's `tryCatch` |
| inline in `mod_tab_send_email` ([L272-317](../../R/mod_tab_send_email.R#L272-L317)) | Store emails. Subject `<CSI type without "ACS "> - <store> - Date:<dd-mm-yyyy>`, with a `praise::praise()` footer | |

### 5. Database helpers

| Function | Where | What it does |
|---|---|---|
| `append_data(conn, table, value)` | [R/mod_tab_dbase_fct_helpers.R:310](../../R/mod_tab_dbase_fct_helpers.R#L310) | Checks the table exists, then `DBI::dbAppendTable`. **Exported** |
| `delete_data(conn, table, col, ids)` | [R/mod_tab_dbase_fct_helpers.R:323](../../R/mod_tab_dbase_fct_helpers.R#L323) | `DELETE FROM <table> WHERE <col> = :x`, binding `ids`. The table and column names are pasted into the SQL, so only pass literals |
| `get_moneygram_stores(conn)` | [R/fct_dbase_operations.R:47](../../R/fct_dbase_operations.R#L47) | Reads `moneygram_stores`, casts `Agent ID` to character, returns `agent_id`, `store` |
| `append_to_db(data, table_name)` | [R/fct_dbase_operations.R:7](../../R/fct_dbase_operations.R#L7) | Opens its own connection, appends, returns TRUE/FALSE. **Only used by dead code** |
| `check_statement_in_db(date, type)` | [R/fct_dbase_operations.R:61](../../R/fct_dbase_operations.R#L61) | `SELECT COUNT(*)` built with `glue`, with no parameters. **Only used by dead code** |

### 6. UI and notification helpers

| Function | Where | Use |
|---|---|---|
| `buttons_edit(ns)` | [R/fct_ui.R:8](../../R/fct_ui.R#L8) | `btn_add`, `btn_edit`, `btn_delete` |
| `message_failure` / `message_success` | [R/fct_notifications.R:8](../../R/fct_notifications.R#L8) | Sticky toast at the top centre |
| `error_csi_file(csi_type)` | [R/fct_notifications.R:30](../../R/fct_notifications.R#L30) | Red feedback under `file_csi` |
| `error_statement_type(statement_type, session)` | [R/fct_notifications.R:38](../../R/fct_notifications.R#L38) | Red feedback under `file_statement` |
| `modal_error_editing_dbase(entity)` | [R/fct_notifications.R:52](../../R/fct_notifications.R#L52) | "Unable to add/edit … to the database" modal. It already calls `showModal()` itself |
| `icon_success`, `icon_failure`, `icon_success_round`, `icon_warning` | [R/fct_global.R:18-23](../../R/fct_global.R#L18-L23) | Icons in the progress messages |

### 7. Config and boot

| Function | Where | Notes |
|---|---|---|
| `active_config()` | [R/app_config.R:22](../../R/app_config.R#L22) | Profile name: `GOLEM_CONFIG_ACTIVE`, then `R_CONFIG_ACTIVE`, then `"default"`. Used by `get_golem_config()` and `app_server` |
| `get_golem_config(value, config = active_config())` | [R/app_config.R:41](../../R/app_config.R#L41) | Returns `NULL` for a missing key |
| `app_sys(...)` | [R/app_config.R:11](../../R/app_config.R#L11) | `system.file(..., package = "CSIemail")` |
| `.onLoad` | [R/zzz.R:14](../../R/zzz.R#L14) | builds the `safely` readers |
