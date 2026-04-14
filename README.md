# CSIemail

<!-- badges: start -->
<!-- badges: end -->

**CSIemail** is a Shiny web application built for ACS Cyprus to automate the
processing and distribution of financial reports across store locations.

## Features

The app is organized into four main sections:

### 1. Load File
- Upload consolidated Cash Sales Invoice (CSI) or Ticket Hour Sales reports (`.xlsx` / `.xls`)
- Validate file format and content
- Automatically split data by store
- Preview and download per-store data

## 2. Send Emails
- Select stores from the detected list
- Generate per-store Excel attachments
- Send reports via SMTP to store email contacts
- Track send status with in-app notifications

### 3. Cashier Monitoring
- Load and consolidate multiple statement types:
  - **Cashier per Store** — daily cash/card totals by courier/cashier
  - **Moneygram Statements** — money transfer transactions
  - **VIVA Statements** — card payment transactions (CSV)
- Generate Excel monitoring workbooks with reconciliation summaries
- Email consolidated reports to city-level cashier groups

### 4. Email Database Management
- Maintain store-to-email mappings (CRUD)
- Manage city-level cashier group emails
- Organise cashier group/department structures

## Technology Stack

| Category        | Packages                                                        |
|-----------------|-----------------------------------------------------------------|
| Framework       | `shiny`, `golem`, `shinydashboard`, `shinymanager`             |
| Data Processing | `dplyr`, `tidyr`, `purrr`, `stringr`, `readxl`, `openxlsx`    |
| Database        | `DBI`, `RSQLite`                                               |
| Email           | `blastula`, `keyring`                                          |
| UI / UX         | `shinyjs`, `shinyFeedback`, `waiter`, `reactable`, `DT`        |

## Getting Started

### Prerequisites

- R >= 2.10
- Dependencies are managed with [`renv`](https://rstudio.github.io/renv/). Restore the project library with:

```r
renv::restore()
```

### Running the App (Development)

```r
CSIemail::run_app()
```

### Production Deployment (Windows Server)

In production, the app runs on an ACS Windows Server. It is launched via
**Windows Task Scheduler**, which calls `Rscript.exe` to source
`launch_on_browser.R`.

That script:
- Detects the server's local IP address via `ipconfig`
- Starts the Shiny app on port **3846**, bound to the server IP (no browser launch)
- Redirects error output to `error_log.Rout` for diagnostics

Once running, the app is accessible on the local network at:

```
http://<server-ip>:3846/
```

### Configuration

App behaviour is controlled by `inst/golem-config.yml` using the `config` package.
Set the `R_CONFIG_ACTIVE` environment variable to switch environments:

| Environment  | Description                                          |
|--------------|------------------------------------------------------|
| `default`    | Development — uses test SMTP credentials             |
| `production` | Production — uses ACS official SMTP account          |
| `dev`        | Dev variant with local file paths                    |

```r
Sys.setenv(R_CONFIG_ACTIVE = "production")
CSIemail::run_app()
```

### SMTP Credentials

Email credentials are managed via the `keyring` package. Register credentials
before running in production:

```r
blastula::create_smtp_creds_key(
  id       = "acs",
  user     = "accounts@acscyprus.com",
  provider = "office365"   # adjust as needed
)
```

## Project Structure

```
CSIemail/
├── R/
│   ├── app_ui.R / app_server.R   # App entry points
│   ├── mod_*.R                   # Shiny modules (one per feature tab)
│   └── fct_*.R                   # Helper / processing functions
├── inst/
│   └── golem-config.yml          # Environment configuration
├── data-raw/                     # Excel templates and store config files
├── DB/                           # SQLite databases (emails, users)
├── SampleData/                   # Test/sample input files
├── Scripts/                      # Utility and testing scripts
├── renv/                         # renv project library
├── renv.lock                     # Dependency lockfile
├── launch_on_browser.R           # Production launcher (Windows Task Scheduler)
└── app.R                         # Standard Shiny entry point
```

## Author

**Lefkios Paikousis** — lefkiospaikousis@yahoo.com

## Links

- GitHub: <https://github.com/lefkiospaikousis/CSIemail>
- Issues: <https://github.com/lefkiospaikousis/CSIemail/issues>
