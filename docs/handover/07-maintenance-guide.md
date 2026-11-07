# 07 – Maintenance guide

## Recipes

### Add a new module

```r
golem::add_module(name = "my_feature")   # creates R/mod_my_feature.R with _ui/_server stubs
```

1. Follow the house style: `mod_my_feature_server <- function(id, conn) moduleServer(id, …)`. Pass the DB connection in; don't open a new one.
2. Mount it.
   - **As a new tab:** add a `menuItem(…, tabName = "x")` to the sidebar and a `tabItem(tabName = "x", mod_my_feature_ui("my_feature_1"))` to the body, both in [R/app_ui.R](../../R/app_ui.R), and call `mod_my_feature_server("my_feature_1", conn = dbase_csi)` in [R/app_server.R](../../R/app_server.R).
   - **As a Database sub-tab:** add a `tabPanel` in [R/mod_tab_dbase.R:16-30](../../R/mod_tab_dbase.R#L16-L30) and call its server next to the others ([L46-48](../../R/mod_tab_dbase.R#L46-L48)).
3. To pass data out, return a `reactiveValues` (`rv(...)`) and watch it from the parent with `observeEvent(child$field, …)`. That's the existing pattern.
4. Run `golem::document_and_reload()` (or `source("dev/run_dev.R")`).

For a new CRUD table, copy [R/mod_cashier_groups.R](../../R/mod_cashier_groups.R), the cleanest example, and add a modal helper to [R/mod_tab_dbase_fct_helpers.R](../../R/mod_tab_dbase_fct_helpers.R). Keep the two details that stop Add from overwriting a row: the `btn_add` observer resets `rv$*_to_edit <- NULL`, and `form_data()` reads `rv$*_to_edit` **without** `isolate()`. Make sure every row has its key (see B1b).

### Add a new business-logic function

```r
golem::add_fct("my_domain")         # R/fct_my_domain.R
# or golem::add_fct("helper", module = "my_feature")  → R/mod_my_feature_fct_helper.R
```

Keep fct functions **free of Shiny where possible**, taking plain data in and returning data out, so they can be tested later. Tag them `@noRd` unless they need exporting. Call other packages as `pkg::fun()`, or add an `@importFrom` and run `devtools::document()`.

### Add a dependency

```r
usethis::use_package("newpkg")   # adds to DESCRIPTION Imports
renv::install("newpkg")          # if not already installed
renv::snapshot()                 # records it in renv.lock
```

Commit `DESCRIPTION` and `renv.lock` together. Several packages are already used but **missing from DESCRIPTION** (`zip`, `cli`, `here`, `dbplyr`). Adding them is a cheap fix.

### Update a package

```r
renv::update("openxlsx")   # or renv::install("openxlsx@4.2.x")
# smoke-test (05-setup-and-running.md#tests), especially the Excel output
renv::snapshot()
```

Packages to treat with care:
- **`openxlsx`**: the reports depend on its styling and protection behaviour.
- **`shinymanager`**: the credentials DB format.
- **`blastula`**: credential file and key formats.
- **`readxl`**: unnamed-column naming (`...1`, `...2`), which `read_acs_csi` depends on.

Update one at a time.

### Add a config key

`inst/golem-config.yml` is git-ignored and exists separately on every machine, so a new key has to go in **three places**:

1. The template [inst/golem-config.example.yml](../../inst/golem-config.example.yml), which is committed. Add it under `default:`, and override it in `production:` or `dev:` only where the value differs. Use placeholders for anything secret.
2. Your own `inst/golem-config.yml`.
3. The production server's `inst/golem-config.yml`, by hand. Mention it in the release notes.

Then:
- Read it with `get_golem_config("my_key")`. Don't pass `config =`, so `active_config()` applies.
- If the code must keep running on machines whose config predates the key, handle `NULL` with a fallback and a warning, as `smtp_creds_source()` does ([R/fct_email_helpers.R:32](../../R/fct_email_helpers.R#L32)).
- Prefer the keyring for credentials (`keyring::key_get()`, `blastula::creds_key()`).

### Change a DB query or schema safely

1. **Work on a copy** of `DB/csi_db.db`. The production DB isn't in git and there's no migration tooling.
2. Find every user of the table. The map is in [03-data-and-databases.md](03-data-and-databases.md#dbcsi_dbdb-table-reference). Most code reads through `db_tables[["…"]]`, but `moneygram_stores` and the literal `"emails"` in `mod_tab_dbase` are hard-coded. The cashier recipient table name comes from the config key `cashier_emails_table`.
3. UPDATE statements bind `params = as.list(form_data())`. If you rename a column, rename both the data-frame column and the `$placeholder`.
4. Spaces in `moneygram_stores` column names: backtick them in SQL, and build data frames with `check.names = FALSE`. Keep `Agent ID` **numeric**. `get_moneygram_stores()` casts it to character to match the statement's `agent_id`.
5. Schema changes go in a small, idempotent R script under `data-raw/` (or a new `migrations/` folder) that you run once on the server after backing up. Write down that you ran it.

### Regenerate `R/sysdata.rda`

[data-raw/create_global_values.R](../../data-raw/create_global_values.R) is the source. **As it stands it fails**: `xl_template_store_monitoring` is passed to `use_data()` but never defined.

1. Remove the three unused `xl_template_*` objects from the `use_data()` call (nothing reads them), or define the missing one.
2. Make your change, then run the script from the repo root.
3. Run `pkgload::load_all()` and check that `CSIemail:::db_tables` etc. look right.

Keep Greek strings as `\u` escapes. The file must stay ASCII-safe.

### Change an Excel template

- **CSI and TH attachments** (`data-raw/ACS CSI template.xlsx`, `TH CSI template.xlsx`): `as_excel_wb()` writes at fixed cells (B3, B5:B6, E5:E6, the table at row 8). If you move things in the template, update [R/mod_tab_send_emails_fct_helpers.R:58-100](../../R/mod_tab_send_emails_fct_helpers.R#L58-L100).
- **Cashier report** (`data-raw/template_report.xlsx`): the column constants at [R/fct_prepare_store_monitoring.R:52-67](../../R/fct_prepare_store_monitoring.R#L52-L67), the hard-coded rows (N9:O18, O27:O28) and the **named ranges** in the template all have to agree. The table in [04](04-functionality.md#the-generated-workbook-tameiakh_cityxlsx) lists them.
  - The sheet must still be called `Sheet1` in the template. The code renames it.
  - The sheet is protected. Edit `template_report_unprotected.xlsx`, then re-protect it and save it as `template_report.xlsx` (⚠️ password, see open questions).

### Bump the version

Change `Version:` in `DESCRIPTION`. The sidebar reads it with `utils::packageVersion("CSIemail")` ([R/app_ui.R:34](../../R/app_ui.R#L34)). `golem_version` in the git-ignored config is no longer displayed, so it doesn't need bumping.

## Debugging tips

- **Inspect a module's state.** Add `browser()` temporarily inside an observer, or put `print(reactiveValuesToList(rv))` in a `renderPrint`. A leftover `output$outs <- renderPrint(reactiveValuesToList(rv))` already exists in [R/app_server.R:138-141](../../R/app_server.R#L138-L141); add a `verbatimTextOutput("outs")` to the UI to see it. **Never commit `browser()`.**
- **Test a reader outside Shiny:**
  ```r
  pkgload::load_all()
  x <- CSIemail:::read_moneygram_statement("SampleData/_Activity Moneygram 24092025.xls")
  ```
  Load the package first, otherwise `safe_readXL` is still the dummy.
- **Reproduce a report** without the UI by calling `prepare_store_monitoring(openxlsx::loadWorkbook(get_golem_config("cashier_template")), cashier, moneygram, viva, city_name = "X")` with data frames built from the readers, then `openxlsx::saveWorkbook()`.
- **"This is not a valid  file" (double space):** expected. The type name is missing because the loaders read the non-existent `input$statement_type`. It's still a genuine read failure. Check the file structure against [03](03-data-and-databases.md#uploaded-file-formats-what-the-readers-expect).
- **"Error when preparing <city>":**
  - Usually VIVA wasn't loaded: `statements$viva_per_store` is NULL, and `select()` on NULL fails.
  - Otherwise a store in `cashier_groups` has odd data. The message carries the R error.
- **A store or Moneygram total is missing from a report:** open the **Not matched** tab in Cashier Monitoring. It lists:
  - stores missing from `cashier_groups`
  - Moneygram agents missing from `moneygram_stores`. Fix the mapping, then load the Moneygram file again.
  - If the store isn't listed there, check that the VIVA store code matches after `-1` is stripped.
- **Which mode am I in?** Look at the sidebar (`Config` / `SMTP credentials` / `Cashier emails`), or the log line at session start. A `Config key … is missing` warning means that machine's `golem-config.yml` predates the `smtp_creds_source` / `cashier_emails_table` keys.
- **Emails fail:**
  - Check `mail_credentials()` interactively with the same `GOLEM_CONFIG_ACTIVE` as the app.
  - In production, the keyring entry must belong to the Windows user running R.
  - `blastula::smtp_send(..., verbose = TRUE)` shows the SMTP conversation.
- **"no such table"** at startup: the app opened the wrong `db_path` (often the wrong working directory) and SQLite created an empty file. Delete the empty file and start from the repo root.
- **Nobody can log in:** `DB/credentials.sqlite` is missing, or the `users_passphrase` doesn't match.
- **Logs in production:** `error_log.Rout` in the app folder. It's overwritten on restart, so copy it before restarting.
- **Encoding:** if Greek headers stop matching, check `Encoding: UTF-8` in DESCRIPTION. Keep `\u` escapes in the code. The one literal `Τιμολόγιο` at [R/fct_read_helpers.R:243](../../R/fct_read_helpers.R#L243) is the most likely to break.

## Known issues, tech debt and risks

Ranked roughly by impact.

### Security and data protection

| # | Issue | Where | Suggested action |
|---|---|---|---|
| S1 | ✅ **Mitigated (2026-11-07).** `inst/golem-config.yml` (holds `users_passphrase`) is now **git-ignored** and copied to each machine by hand. Template: `inst/golem-config.example.yml` | `.gitignore`, [inst/golem-config.example.yml](../../inst/golem-config.example.yml) | Older commits still contain the file, so the value is in git history. Rewrite history or change the passphrase if that matters (depends on Q14) |
| S2 | `DB/csi_db.db` (store and cashier email addresses) is in **14 historic commits**, even though `DB/` is now git-ignored | git history | If the repo is public or shared, consider rewriting history (`git filter-repo`) |
| S3 | Plaintext SMTP credentials file in the repo root | `mail_improvast` (git-ignored) | Fine for dev, but never copy it to the server. Don't sync it to shared OneDrive |
| S4 | Possibly plaintext passwords in local files | `DB/users.db`, `DB/credentials.csv`, `Scripts/01-create_db_users.R` (all git-ignored) | Delete them, or move them to a secure store |
| S5 | Every browser calls `http://ipinfo.io` (plain HTTP, third party) on page load, and the result is unused | [inst/app/www/getIP.js](../../inst/app/www/getIP.js) | Delete the file, along with `IP_info` / `remote_addr` if they're not wanted |
| S6 | The app serves plain HTTP on the LAN, and every logged-in user is effectively an admin of the reference data | deployment, app design | Put HTTPS in front if needed. Decide whether CRUD needs a role check (`res_auth$admin`) |
| S7 | `check_statement_in_db()` builds SQL with `glue` (injection-prone) | [R/fct_dbase_operations.R:67](../../R/fct_dbase_operations.R#L67) | Dead code: remove it, or parameterise it |

### Correctness bugs

| # | Issue | Where | Effect |
|---|---|---|---|
| B1 | ✅ **Fixed (2026-11-07).** Edit → Cancel → Add used to UPDATE the previously selected row instead of inserting a new one, because `*_to_edit` was never cleared | all 4 CRUD modules: the `btn_add` observers now reset `rv$*_to_edit <- NULL` (e.g. [R/mod_tab_dbase.R:88-95](../../R/mod_tab_dbase.R#L88-L95), [R/mod_moneygram_stores.R:63-70](../../R/mod_moneygram_stores.R#L63-L70)), and `form_data()` no longer `isolate()`s the edit target | Checked with `shiny::testServer` against a temporary DB copy: the bug reproduced in all 4 modules before the fix, and Add/Edit/Edit→Cancel→Add all behave correctly after it |
| B1b | **`cashier_groups` rows without a `uuid`.** In the local DB, 36 of 37 rows have `uuid` NULL. Edit and Delete match on `uuid`, so for those rows they **silently do nothing** (`WHERE uuid = NULL` matches nothing) | data in `DB/csi_db.db`; [R/mod_cashier_groups.R:119](../../R/mod_cashier_groups.R#L119), [L194](../../R/mod_cashier_groups.R#L194) | One-off data fix on the server, after a backup: give every NULL `uuid` a fresh UUID. ⚠️ TO CONFIRM whether the production DB has the same gap (Q25) |
| B2 | Generate is enabled without VIVA. Every city then fails, and the file names have no date | [R/mod_cashier_monitoring.R:209](../../R/mod_cashier_monitoring.R#L209) | confusing failure. Add `!is.null(statements$viva_per_store)` to the condition |
| B3 | ✅ **Fixed (2026-11-07).** City and store-group lists in Monitoring used to be read once per session. They're now re-read on **Generate** (cashier groups), **Send Emails** (recipients) and **Re-check against the Database** (in the "Not matched" tab), via the `groups_refresh` / `emails_refresh` counters | [R/mod_cashier_monitoring.R:123-147](../../R/mod_cashier_monitoring.R#L123-L147) | — |
| B4 | Email-format validation is broken: it passes the value instead of the input ID, and doesn't stop saving | [R/mod_tab_dbase.R:178-185](../../R/mod_tab_dbase.R#L178-L185), [R/mod_cachier_groups_emails.R:154-161](../../R/mod_cachier_groups_emails.R#L154-L161) | bad addresses get stored, and sends fail later |
| B5 | `prepare_store_monitoring(city = …)` depends on partial matching to `city_name` | [R/mod_cashier_monitoring.R:417](../../R/mod_cashier_monitoring.R#L417) | breaks if a parameter like `city_x` is added. Rename the argument in the call |
| B6 | ✅ **Mitigated (2026-11-07).** Unmapped Moneygram agents and stores missing from `cashier_groups` are still left out of the reports, but they're **listed**: `find_unmatched()` ([R/fct_unmatched.R:16](../../R/fct_unmatched.R#L16)) feeds a "Not matched" tab, a notification, and a warning block in the Generate modal | [R/mod_cashier_monitoring.R:150-195](../../R/mod_cashier_monitoring.R#L150-L195) | — |
| B7 | `showModal(showModal(...))`, and `modal_error_editing_dbase()` already calls `showModal` itself | [R/mod_tab_dbase.R:169](../../R/mod_tab_dbase.R#L169) | harmless but wrong |
| B8 | Loader messages read the non-existent `input$statement_type` | [R/mod_load_cashier_per_store.R:71](../../R/mod_load_cashier_per_store.R#L71), [R/mod_load_viva_per_store.R:69](../../R/mod_load_viva_per_store.R#L69) | the message text is missing the type |
| B9 | `cli_abort` message uses `collpase` (typo) | [R/fct_read_helpers.R:100](../../R/fct_read_helpers.R#L100), [L181](../../R/fct_read_helpers.R#L181) | garbled error text |
| B10 | `Sys.setlocale()` changes the locale for the whole process while files are written | [R/mod_tab_send_emails_fct_helpers.R:5-6](../../R/mod_tab_send_emails_fct_helpers.R#L5-L6) | could affect concurrent sessions. Windows-only |
| B11 | The template's `viva_received` named range is `#REF!` | `data-raw/template_report.xlsx` | unused today, but a trap |
| B12 | ✅ **Fixed (2026-11-08).** The logo on the login page was missing after every app start, and only appeared once someone had logged in. shinymanager renders the login page without calling `app_ui()`, and `app_ui()` was where golem registered `www/`, so `www/logo.png` was a 404 until the first login in that R process. `run_app()` now registers `www/` itself | [R/run_app.R](../../R/run_app.R), test in [tests/testthat/test-run-app.R](../../tests/testthat/test-run-app.R) | — |

### Operational and structural risks

| # | Issue | Where |
|---|---|---|
| O1 | Hard-coded production paths, with the dev ones commented out | [launch_on_browser.R:6-15](../../launch_on_browser.R#L6-L15) |
| O2 | The R version in `ShinyApp.bat` (4.1.0) doesn't match `renv.lock` (4.2.3) | [ShinyApp.bat](../../ShinyApp.bat) |
| O3 | Packages used but not in `DESCRIPTION` (`zip`, `cli`, `here`, `dbplyr`) | [DESCRIPTION](../../DESCRIPTION) |
| O4 | The DB isn't versioned or backed up by anything in the repo. There are no migrations and no constraints | `DB/` |
| O5 | One R process serves every user, and email sending and report generation block it | architecture |
| O6 | A small `testthat` suite exists (since 2026-11-07), but there's no CI and most of the code is untested: the file readers, `prepare_store_monitoring()` and the store-email flow | [tests/testthat/](../../tests/testthat/) |
| O7 | ✅ **Fixed (2026-11-07).** The SMTP source and cashier recipient table used to depend on `golem.app.prod`, separately from the config profile, and `app_server` ignored `R_CONFIG_ACTIVE`. Now both come from config keys (`smtp_creds_source`, `cashier_emails_table`), the profile lookup is in one place (`active_config()`), and the sidebar and log show the active mode | [R/fct_email_helpers.R](../../R/fct_email_helpers.R), [R/app_config.R:22](../../R/app_config.R#L22), [R/fct_ui.R:30](../../R/fct_ui.R#L30) |
| O8 | Very long functions: `mod_cashier_monitoring_server` (~610 lines) and `prepare_store_monitoring` (~370 lines, hard-coded cells) | |
| O9 | The sysdata build script is broken | [data-raw/create_global_values.R:75](../../data-raw/create_global_values.R#L75) |
| O10 | The seeding script overwrites the `emails` table and adds personal test addresses | [data-raw/create_store_email_table.R](../../data-raw/create_store_email_table.R) |
| O11 | ✅ **Fixed (2026-11-08).** Log files (`error_log.Rout`, `service.*.log`, `launch_on_browser.log`) used to be committed, three of them hidden with the `skip-worktree` flag. They're now untracked, and `*.log` / `*.Rout` are git-ignored | [.gitignore](../../.gitignore) |
| O12 | CLAUDE.md is out of date: it mentions a stray `browser()` in `app_server.R` that no longer exists. There are no `browser()` calls in `R/` | [CLAUDE.md](../../CLAUDE.md) |

### Dead code (safe to remove once confirmed)

| Item | Where |
|---|---|
| `add_to_db` observer and `return(rv)` | [R/mod_cashier_monitoring.R:690-767](../../R/mod_cashier_monitoring.R#L690-L767) |
| `append_to_db`, `check_statement_in_db` (only used by the above) | [R/fct_dbase_operations.R](../../R/fct_dbase_operations.R) |
| Whole module `mod_user_management` (broken) | [R/mod_user_management.R](../../R/mod_user_management.R) |
| `check_filetype`, `check_var_names` (undefined globals) | [R/fct_data_validation.R](../../R/fct_data_validation.R) |
| `anonymise`, `anonymise2` | [R/fct_read_helpers.R:316-332](../../R/fct_read_helpers.R#L316-L332) |
| `labelMandatory`, `appCSS` | [R/mod_tab_dbase_fct_helpers.R:8-15](../../R/mod_tab_dbase_fct_helpers.R#L8-L15) |
| `get_icon` | [R/fct_global.R:5](../../R/fct_global.R#L5) |
| `IP_info`, `ip_user`, `output$ip_user`, `output$outs` | [R/app_server.R:85-97](../../R/app_server.R#L85-L97), [L138-L141](../../R/app_server.R#L138-L141) |
| `output$statement` in all 3 loaders (never rendered) | e.g. [R/mod_load_viva_per_store.R:154](../../R/mod_load_viva_per_store.R#L154) |
| `statement_types`, `xl_template_*` in sysdata | [data-raw/create_global_values.R](../../data-raw/create_global_values.R) |
| `inst/global.R` | [inst/global.R](../../inst/global.R) |
| `tbl_emails_proxy` (created, never used) | [R/mod_tab_dbase.R:50](../../R/mod_tab_dbase.R#L50) |
