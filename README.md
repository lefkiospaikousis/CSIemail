# CSIemail

<!-- badges: start -->
<!-- badges: end -->

**CSIemail** is a Shiny web application, built as an R package with
[golem](https://thinkr-open.github.io/golem/), for ACS Cyprus. It automates the
processing and distribution of financial reports across store locations.

> **New maintainer?** Start with the handover documentation in
> [`docs/handover/`](docs/handover/README.md). It covers the architecture, every
> module, the databases, setup, deployment and the known issues.

## Features

The app has a login page (`shinymanager`) and four sections:

### 1. Load File
- Upload a consolidated Cash Sales Invoice (CSI) or Ticket Hour Sales report (`.xlsx` / `.xls`)
- Validate the file format and content
- Split the data by store automatically
- Preview and download each store's data

### 2. Send Emails
- Select stores from the detected list
- Generate a per-store Excel attachment from a template
- Send the reports over SMTP to each store's email addresses
- See the send status of each store

### 3. Cashier Monitoring
- Load three daily statements:
  - **Cashier per Store**: cash and card totals per courier/cashier (Excel)
  - **Moneygram**: money transfer activity (Excel)
  - **VIVA**: card payment transactions (CSV). This sets the report date
- See which rows won't appear in any report (stores missing from Cashier Groups, unmapped Moneygram agents)
- Generate one protected Excel reconciliation workbook per city
- Download all workbooks as a ZIP, or email each city's cashier group

### 4. Database
CRUD screens for the reference tables the other sections depend on:
- **Store Emails**: store → email addresses
- **Cashier Groups Emails**: city → cashier email addresses
- **Cashier Groups**: city → stores included in that city's report
- **Moneygram Stores**: Moneygram Agent ID → ACS store code

## Technology Stack

| Category        | Packages                                                       |
|-----------------|----------------------------------------------------------------|
| Framework       | `shiny`, `golem`, `shinydashboard`, `shinymanager`             |
| Data Processing | `dplyr`, `tidyr`, `purrr`, `stringr`, `readxl`, `openxlsx`     |
| Database        | `DBI`, `RSQLite`, `dbplyr`                                     |
| Email           | `blastula`, `keyring`                                          |
| UI / UX         | `shinyjs`, `shinyFeedback`, `waiter`, `reactable`, `DT`        |

## Getting Started

### Prerequisites

- **Windows.** The code uses Windows locale names and the Windows credential store.
- **R 4.2.3**, the version recorded in `renv.lock`. At least R 4.1 is needed for the native pipe.
- Dependencies are managed with [`renv`](https://rstudio.github.io/renv/). Restore the project library with:

```r
renv::restore()
```

- The config file is **not in git**. Create it from the template and fill in the `<...>` placeholders. The passphrase must be the one `DB/credentials.sqlite` was encrypted with:

```r
file.copy("inst/golem-config.example.yml", "inst/golem-config.yml")
```

- The SQLite databases are **not in git** (`DB/` is git-ignored). Copy them into `DB/` before starting:
  - `DB/csi_db.db`: reference tables
  - `DB/credentials.sqlite`: login accounts
- For the `default` and `dev` profiles, create the folder `SampleData/CashierReports/`. Generated cashier reports are saved there.

### Running the App (Development)

From the project root:

```r
source("dev/run_dev.R")   # golem.app.prod = FALSE, then document_and_reload() + run_app()
```

Run the automated tests with `devtools::test()`. They use a synthetic database, never the real `DB/` files.
Before a release, also run the manual smoke test in
[`docs/handover/05-setup-and-running.md`](docs/handover/05-setup-and-running.md#tests).

### Configuration

Behaviour is controlled by the **config profile** in `inst/golem-config.yml`, selected with the
`GOLEM_CONFIG_ACTIVE` environment variable (`default`, `production`, `dev`). `R_CONFIG_ACTIVE`
is used if `GOLEM_CONFIG_ACTIVE` is unset. Two keys in each profile decide where emails go:

| Profile      | `smtp_creds_source`          | `cashier_emails_table`            |
|--------------|------------------------------|-----------------------------------|
| `default`    | `file` (blastula creds file) | `city_emails2` (test recipients)  |
| `dev`        | `file`                       | `city_emails2`                    |
| `production` | `keyring`                    | `city_emails` (real city cashiers)|

`dev/run_dev.R` uses the `default` profile, and `app.R` (production) sets `production`. The sidebar
shows the active profile, SMTP source and cashier table, with a red **(TEST)** flag for the test list.

```r
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
pkgload::load_all(); run_app()   # caution: this sends real emails
```

`inst/golem-config.yml` is **git-ignored**. Each machine keeps its own copy, created from
[`inst/golem-config.example.yml`](inst/golem-config.example.yml). The production server's copy is
maintained by hand: don't overwrite it when deploying, and add any new keys to it yourself.

### SMTP Credentials

- **`smtp_creds_source: file`** (development): a blastula credentials file in the project root, named by the
  `smtp_creds` config key. Create it with `blastula::create_smtp_creds_file()`.
  It stores the password in plain text, so it must stay git-ignored.
- **`smtp_creds_source: keyring`** (production): a keyring entry, created on the server **as the Windows user that runs the app**:

```r
blastula::create_smtp_creds_key(
  id       = "acs",               # must match smtp_creds in the production profile
  user     = "<sender address>",
  provider = "office365"          # adjust as needed
)
```

### Production Deployment (Windows Server)

In production, the app runs from source on an ACS Windows Server as a Windows service
managed by [NSSM](https://nssm.cc/). NSSM starts R's `Rscript.exe` with the launcher
[`run_app.R`](run_app.R) in the project root. There's no `.bat` file. The launcher:

- finds its own folder, activates renv, and warns if the R version doesn't match `renv.lock`
- checks that `inst/golem-config.yml` and the `DB/` files exist
- loads the package with the `production` profile (or `GOLEM_CONFIG_ACTIVE`, if set)
- serves on all interfaces, port **3846** (or `APP_PORT`, or the first argument)
- appends to `logs/app.log`, and exits with code 1 on errors so NSSM restarts it

```cmd
"C:\Program Files\R\R-4.2.3\bin\x64\Rscript.exe" run_app.R        # by hand; NSSM runs the same
```

Once running, the app is available on the local network at `http://<server-name>:3846/`.

The service must run as the Windows account that owns the `acs` keyring entry (the SMTP password).
When deploying, keep the server's `DB/` folder and its own `inst/golem-config.yml`, and back up `DB/` first.
See [`docs/handover/06-deployment.md`](docs/handover/06-deployment.md) for:
- the NSSM setup
- the one-time switch from the old `ShinyApp.bat`
- the update, rollback and troubleshooting steps

## Project Structure

```
CSIemail/
├── R/
│   ├── app_ui.R / app_server.R   # App entry points
│   ├── mod_*.R                   # Shiny modules (one per tab / sub-tab)
│   ├── fct_*.R                   # Business logic: readers, Excel builders, email, DB helpers
│   └── sysdata.rda               # Internal constants (built from data-raw/)
├── inst/
│   ├── golem-config.yml          # Environment configuration (git-ignored, one per machine)
│   ├── golem-config.example.yml  # Template for golem-config.yml
│   └── app/www/                  # CSS, JS, images
├── data-raw/                     # Excel templates (used at runtime) and build scripts
├── docs/handover/                # Maintainer documentation
├── DB/                           # SQLite databases (git-ignored)
├── SampleData/                   # Sample input files (git-ignored)
├── Scripts/                      # Ad-hoc scripts (git-ignored)
├── dev/                          # golem dev scripts (run_dev.R)
├── renv/, renv.lock              # Dependency management
├── tests/testthat/               # Automated tests
├── run_app.R                     # Production launcher (started by NSSM); not R/run_app.R
├── ShinyApp.bat, launch_on_browser.R  # DEPRECATED old launcher, kept for reference only
└── app.R                         # golem entry point for RStudio's "Run App"
```

## Author

**Lefkios Paikousis**, lefkiospaikousis@yahoo.com

## Links

- GitHub: <https://github.com/lefkiospaikousis/CSIemail>
- Issues: <https://github.com/lefkiospaikousis/CSIemail/issues>
