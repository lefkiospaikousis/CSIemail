# 05 – Setup and running

## Requirements

| Item | Value | Source |
|---|---|---|
| OS | **Windows**, effectively required. `Sys.setlocale("greek")` and `"English_United Kingdom"` are Windows locale names ([R/mod_tab_send_emails_fct_helpers.R:5-6](../../R/mod_tab_send_emails_fct_helpers.R#L5-L6)), the launcher uses `ipconfig`, and production uses the Windows credential store via `keyring` | code |
| R | **4.2.3**, per `renv.lock` | [renv.lock](../../renv.lock) |
| Package repo | Posit Package Manager, `https://packagemanager.posit.co/cran/latest` (Windows binaries, no compilers needed) | `renv.lock` |
| Packages | 212 locked. `renv` settings: implicit snapshot, ignores `CSIemail` and `covrpage` | [renv/settings.json](../../renv/settings.json) |
| System libraries | None beyond what CRAN binaries bundle on Windows (`RSQLite`, `openxlsx`, `readxl`, `zip` ship as binaries). On Linux you'd need the usual `libxml2` / `libcurl` / `libssl` / `libsodium` headers, plus a keyring backend. That isn't tested | inferred |
| RStudio | optional. `CSIemail.Rproj` is the project file | |

> ⚠️ TO CONFIRM: [ShinyApp.bat](../../ShinyApp.bat) hard-codes `R-4.1.0`, but `renv.lock` was written by R 4.2.3. This dev machine has 4.1.0, 4.2.3 and 4.3.3 installed. Which R version does the production server actually run?

`DESCRIPTION` says `Depends: R (>= 2.10)`, but the code uses the native pipe `|>` and needs **R ≥ 4.1**.

## First-time setup

```r
# 0. Clone and open CSIemail.Rproj (or setwd() to the repo root).
#    The .Rprofile runs source("renv/activate.R").
#    renv/activate.R is in git and bootstraps renv itself on first run.

# 1. Restore the locked library
renv::restore()

# 2. Create your machine's config from the template (golem-config.yml is git-ignored)
file.copy("inst/golem-config.example.yml", "inst/golem-config.yml")
#    ...then edit inst/golem-config.yml and fill in the <...> values

# 3. Check that the package loads
pkgload::load_all()
```

Then sort out the files git doesn't carry:

| What | Where it must be | How to get it |
|---|---|---|
| `inst/golem-config.yml` | `inst/` | copy [inst/golem-config.example.yml](../../inst/golem-config.example.yml) and fill in `users_passphrase` (it must be the passphrase `DB/credentials.sqlite` was encrypted with; get it from the previous maintainer) and `sender`. **The package won't load without this file** |
| `DB/csi_db.db` | `DB/` (config `db_path`) | copy from production or from the previous maintainer. ⚠️ TO CONFIRM where the master copy lives |
| `DB/credentials.sqlite` | `DB/` (config `db_users`) | same as above. You need an account in it to log in |
| `SampleData/` | repo root | copy from the previous maintainer. Used for manual testing |
| `SampleData/CashierReports/` | must **exist** for the `default` and `dev` profiles (`path_store_reports`) | create the folder. `saveWorkbook` fails if it's missing |
| SMTP credentials | see below | only needed if you want to send email |

`.Renviron` sets `RENV_PATHS_LIBRARY` to a machine-specific path, which keeps the renv library out of the synced OneDrive folder. **Change or remove it on a new machine**, otherwise renv may try to use a path that doesn't exist.

### `.Renviron` template

```ini
# Optional: where renv keeps the project library (keep it out of OneDrive/network drives)
RENV_PATHS_LIBRARY=

# Optional: config profile; leave unset for "default". Values: default | production | dev
GOLEM_CONFIG_ACTIVE=

# Fallback, used only when GOLEM_CONFIG_ACTIVE is unset (see active_config())
R_CONFIG_ACTIVE=
```

The app reads no other environment variables. Everything else is in `inst/golem-config.yml` (git-ignored; template [inst/golem-config.example.yml](../../inst/golem-config.example.yml)) or the OS keyring.

### SMTP credentials

`mail_credentials()` ([R/fct_email_helpers.R:15](../../R/fct_email_helpers.R#L15)) reads the config key `smtp_creds_source`:

- **`smtp_creds_source: file`** (`default`, `dev`): a blastula **credentials file** in the working directory, named by `smtp_creds` (currently `mail_improvast`). Create one interactively. It prompts for the password:
  ```r
  blastula::create_smtp_creds_file(file = "mail_improvast", user = "<your test sender>",
                                   provider = "gmail")  # or host/port/use_ssl
  ```
  The file stores the password **in plain text**. Keep it git-ignored; it's already listed in `.gitignore`.
- **`smtp_creds_source: keyring`** (`production`): a **keyring** entry with id `smtp_creds` (currently `acs`). Create it on the server, **as the Windows user the app runs under**:
  ```r
  blastula::create_smtp_creds_key(id = "acs", user = "<sender address>", provider = "office365")
  ```

> ⚠️ TO CONFIRM: the SMTP provider and host for production. The `office365` in the root README is marked "adjust as needed". Also confirm the sender account.

## Choosing a configuration

**One switch, the config profile**, controls behaviour: `GOLEM_CONFIG_ACTIVE`, falling back to `R_CONFIG_ACTIVE`, then `"default"`. The lookup lives in one helper, `active_config()` ([R/app_config.R:22](../../R/app_config.R#L22)), which both `get_golem_config()` and `app_server` (for the login DB) use.

The profile also decides **where emails go**, through two config keys:
- `smtp_creds_source` → the SMTP credential source ([R/fct_email_helpers.R:32](../../R/fct_email_helpers.R#L32))
- `cashier_emails_table` → which table holds the city cashier recipients ([R/fct_email_helpers.R:54](../../R/fct_email_helpers.R#L54))

The R option `golem.app.prod` no longer affects either decision. `app.R` still sets it, but this app doesn't depend on it any more.

| Profile key | `default` | `production` | `dev` |
|---|---|---|---|
| `app_prod` | no | yes | no |
| `smtp_creds` | dev creds file | `acs` (keyring id) | dev creds file |
| `smtp_creds_source` | `file` | `keyring` | `file` |
| `cashier_emails_table` | `city_emails2` (test) | `city_emails` (**real cashiers**) | `city_emails2` (test) |
| `sender` | a personal test address | the ACS accounts address | a personal test address |
| `db_path` | `DB/csi_db.db` | (inherits) | (inherits) |
| `db_users` | `DB/credentials.sqlite` | (inherits) | (inherits) |
| `users_passphrase` | *(secret, see [03](03-data-and-databases.md#credentials-and-where-they-live-no-values))* | (inherits) | (inherits) |
| `acs_template` / `th_template` / `cashier_template` | `data-raw/…xlsx` | (inherits) | (inherits) |
| `path_store_reports` | `SampleData/CashierReports` | `tempdir()` | `SampleData/CashierReports` |
| `golem_wd` | — | — | `here::here()` |

`inst/golem-config.yml` is **git-ignored**, so every machine keeps its own copy. The template, with placeholders for the passphrase and sender, is [inst/golem-config.example.yml](../../inst/golem-config.example.yml). On the production server the file is copied over and maintained by hand.

`app_prod` and `sender` are **never read by the code**. The From address comes from the SMTP credential.

| How you start it | Profile | SMTP source | Cashier recipients |
|---|---|---|---|
| `source("dev/run_dev.R")` | `default` (unless the env var is set) | creds file | `city_emails2` |
| `pkgload::load_all(); run_app()` | env var or `default` | creds file | `city_emails2` |
| `app.R` / production launcher | `production` (set in [app.R:7](../../app.R#L7)) | keyring `acs` | `city_emails` (**real cashiers**) |

**Seeing which mode is active:**
- The sidebar shows `Config: <profile>`, `SMTP credentials: <file|keyring>` and `Cashier emails: <table>`. A red **(TEST)** appears when the table isn't `city_emails` (`mode_badge()`, [R/fct_ui.R:30](../../R/fct_ui.R#L30)).
- The same line is logged at session start (`app_server`, [R/app_server.R:41-44](../../R/app_server.R#L41-L44)), so it ends up in `error_log.Rout` in production.

Tab 2 (store CSI emails) always sends to the addresses in the `emails` table, whatever the profile. Only the cashier reports have a test recipient list.

**Older config files without the two keys:** the code falls back to the previous behaviour, driven by `golem.app.prod` (dev → file + `city_emails2`, prod → keyring + `city_emails`), and logs a warning saying `Config key … is missing from golem-config.yml`. Add the keys to make the warning go away. A wrong value for `smtp_creds_source` stops with an error.

## Running in development

```r
source("dev/run_dev.R")
# = options(golem.app.prod = FALSE); golem::detach_all_attached(); rm(list = ls());
#   golem::document_and_reload(); run_app()
```

To run with the production profile against your local DB, which is useful for reproducing a production bug **but sends real email** (the sidebar won't show TEST):

```r
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
pkgload::load_all(); run_app()
```

## Tests

### Automated tests

A `testthat` (edition 3) suite was added on 2026-11-07, in [tests/testthat/](../../tests/testthat/):

```r
devtools::test()            # or testthat::test_local()
```

| File | Covers |
|---|---|
| [test-crud-modules.R](../../tests/testthat/test-crud-modules.R) | All four Database-tab modules: Edit → Cancel → Add inserts (the B1 regression), normal Edit changes only the selected row, normal Add inserts, Moneygram rejects a duplicate Agent ID |
| [test-email-mode.R](../../tests/testthat/test-email-mode.R) | `active_config()` lookup order. The config keys win over `golem.app.prod`. Fallback and warning when the keys are missing. An invalid value errors. `mail_credentials()` dispatch (blastula mocked). The sidebar TEST flag. The template defines the keys for every profile and holds only placeholder secrets |
| [test-find-unmatched.R](../../tests/testthat/test-find-unmatched.R) | `find_unmatched()` for each statement type, the amount sums, rows with no store, and the modal warning |
| [test-mod-cashier-monitoring.R](../../tests/testthat/test-mod-cashier-monitoring.R) | Re-read of Cashier Groups on Re-check and Generate, recipients on Send, the recipient table coming from `cashier_emails_table()`, unmatched reporting, and loading the three sample files through the UI inputs |

How the suite is set up:
- **It never uses the real `DB/` files.** `local_fixture_db()` ([helper-fixtures.R](../../tests/testthat/helper-fixtures.R)) builds a small synthetic SQLite DB with the same schema and deletes it after each test.
- Config lookups are mocked with `local_mocked_bindings()`, so the suite runs even on a machine without `inst/golem-config.yml`.
- The sample-file test is **skipped** when the git-ignored `SampleData/` is missing.
- No test advances the report timer, so no report files are written.

Still **not** covered: the file readers' edge cases, `prepare_store_monitoring()` / the Excel output, the store CSI email flow (Tab 1–2), and actual SMTP sending. There's no CI, and `Scripts/test-*.R` (git-ignored) are exploratory scripts, not part of the suite.

### Manual smoke test

Run this before every release, using `SampleData/`:

1. Log in.
2. Tab 1: load `csi 23092021.xls` as ACS CSI, then `TH REPORT EXCEL.xls` as Ticket Hour. Preview a store and download it.
3. Tab 2: select **one** test store, i.e. one whose `emails` address you control, and send.
4. Tab 3: load `_CASHIERS_CITY_USERS.xlsx`, `_Activity Moneygram 24092025.xls` and `_VIVA_24-10-2025.csv`. Generate, download the ZIP, and open one workbook in Excel to check the formulas resolve. Load `_VIVA_24-10-2025_2dates.csv` and confirm it's rejected.
5. Tab 4: add, edit and delete a dummy row in each sub-tab. Use a copy of the DB.

> ⚠️ TO CONFIRM: these sample files are from different dates. Is there a matched set (same day) that makes a meaningful reconciliation test?
