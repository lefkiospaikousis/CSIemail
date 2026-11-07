# CSIemail: handover documentation

This folder is the entry point for whoever maintains **CSIemail** next. You should know R and Shiny already; these docs explain *this* app.

> Conventions used throughout
> - File references look like [R/app_server.R:80](../../R/app_server.R#L80). Line numbers were correct when this was written (branch `dev`, after commit `872cf18`).
> - `> ⚠️ TO CONFIRM:` marks something the code alone can't settle. Every one of them is also listed in [08-open-questions.md](08-open-questions.md).
> - No secret values appear in these docs. Where a credential exists, the docs say *where* it is and *what it's for*.

## What the app does

CSIemail is a Shiny app, built as an R package with [golem](https://thinkr-open.github.io/golem/), for **ACS Cyprus** ([DESCRIPTION](../../DESCRIPTION), [README.md](../../README.md)). It automates three back-office jobs:

1. **Split and email per-store statements.** A user uploads a consolidated *ACS CSI* (Cash Sales Invoice) or *Ticket Hour Sales* Excel file. The app splits it by store, builds one Excel attachment per store from a template, and emails each store its own file over SMTP. Store email addresses come from a SQLite table.
2. **Cashier monitoring.** A user uploads three daily statements: a *Cashier per Store* report (Excel, with Greek headers), a *Moneygram* activity statement (Excel) and a *VIVA* card-payments statement (CSV). The app reconciles them per store, writes one protected Excel workbook per **city** (the "ΤΑΜΕΙΑΚΗ" or *Tamiaki* report) and emails each city's cashier group.
3. **Reference data maintenance.** CRUD screens for the lookup tables the first two jobs depend on: store emails, city cashier emails, city→store groups, and the Moneygram agent→store mapping.

Access is behind a login page (`shinymanager`). The production instance runs on an ACS Windows server on the local network, port 3846. An NSSM Windows service starts it with the launcher [run_app.R](../../run_app.R) ([06-deployment.md](06-deployment.md)).

> ⚠️ TO CONFIRM: who uses the app day to day (accounts staff? how many?), how often each job runs (daily?), and who administers user accounts. None of this can be read from the code.

## 5-minute quickstart (local development)

Prerequisites:
- R. `renv.lock` records 4.2.3; see the version note in [05-setup-and-running.md](05-setup-and-running.md).
- RStudio, or any R console started **in the repo root**.

```r
# 1. Restore packages. The project .Rprofile activates renv automatically.
renv::restore()

# 2. Create your config (git-ignored) and fill in the <...> placeholders
file.copy("inst/golem-config.example.yml", "inst/golem-config.yml")

#    Get the two SQLite files into DB/. They are git-ignored and not in the repo:
#    DB/csi_db.db and DB/credentials.sqlite. See 05-setup-and-running.md.

# 3. Run in development mode (config profile "default": creds file, test cashier list)
source("dev/run_dev.R")
```

Log in with a shinymanager account that exists in `DB/credentials.sqlite`. Sending emails with the default profile also needs a blastula credentials file in the repo root; see [05-setup-and-running.md](05-setup-and-running.md#smtp-credentials). Everything else, including loading files and generating reports, works without it.

Sample input files for every upload type are in `SampleData/`. That folder is git-ignored too, so you'll have to obtain a copy along with the DB.

## Map of the docs

| Doc | Read it when you need to… |
|---|---|
| [01-architecture.md](01-architecture.md) | understand how the pieces fit: golem layout, module tree, data flow, reactivity |
| [02-codebase-guide.md](02-codebase-guide.md) | find what a given file, module or function does |
| [03-data-and-databases.md](03-data-and-databases.md) | touch the SQLite DBs, uploaded file formats, SQL, credentials |
| [04-functionality.md](04-functionality.md) | understand what the user sees on each tab and the business rules behind it |
| [05-setup-and-running.md](05-setup-and-running.md) | set up a machine from scratch, choose config profiles, run it |
| [06-deployment.md](06-deployment.md) | release to, or roll back, the Windows production server |
| [07-maintenance-guide.md](07-maintenance-guide.md) | change things safely, debug, and see the known issues and risks list |
| [08-open-questions.md](08-open-questions.md) | the list of things the outgoing maintainer still needs to answer |

## The five things to know first

1. **`inst/golem-config.yml` is git-ignored: every machine has its own copy.** Create yours from [inst/golem-config.example.yml](../../inst/golem-config.example.yml). The production server's copy is maintained by hand, so **never overwrite it** when deploying, and add any new keys to it yourself ([06-deployment.md](06-deployment.md#release-procedure-reconstructed-confirm-before-use)). A dev SMTP credentials file (`mail_improvast`, git-ignored) sits in the repo root.
2. **The databases are not in git.** `DB/` is git-ignored. Production data lives only on the server. Back it up before every release.
3. **The config profile decides where emails go.** `GOLEM_CONFIG_ACTIVE` selects the profile, and its keys `smtp_creds_source` and `cashier_emails_table` decide the SMTP credential source and whether cashier reports go to real cashiers or the test list. The sidebar shows the active mode, with a red (TEST) flag. See [05-setup-and-running.md](05-setup-and-running.md#choosing-a-configuration).
4. **The automated tests are few and new.** `devtools::test()` runs a small `testthat` suite covering the Database-tab edits, the email mode and the cashier monitoring checks ([05](05-setup-and-running.md#tests)). Most of the app, including file parsing and the Excel output, still has to be checked by hand against the files in `SampleData/`.
5. **The Excel reports are built cell by cell.** [R/fct_prepare_store_monitoring.R](../../R/fct_prepare_store_monitoring.R) hard-codes column numbers that must match the named ranges in `data-raw/template_report.xlsx`.
