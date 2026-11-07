# 08 – Open questions

These are all the `⚠️ TO CONFIRM` items from the other docs, in one place. Please answer them before the handover, and then update the relevant doc and remove the marker.

## Business context

| # | Question | Raised in |
|---|---|---|
| Q1 | Who uses the app day to day (team or roles, roughly how many people), and who administers shinymanager accounts? | [README](README.md), [03](03-data-and-databases.md#dbcredentialssqlite-users) |
| Q2 | How often does each job run (CSI emails, Ticket Hour emails, cashier monitoring)? Daily, and at what time? | [README](README.md) |
| Q3 | Glossary: what exactly are *CSI*, *Ticket Hour Sales*, *ΤΑΜΕΙΑΚΗ / Tamiaki*, *VIVA*? What do the Greek cashier-report columns mean in business terms (Μεταφ., Αγορ. Πληρωμές/Εισπράξεις, Αντικ. Μετρητά/Επιταγές)? | [04](04-functionality.md) |
| Q4 | In the reconciliation, is the cashier report's **card total** meant to match the **VIVA** total? Is a positive difference (VIVA > card) acceptable, given that it gets no colour? | [04](04-functionality.md#the-generated-workbook-tameiakh_cityxlsx) |
| Q5 | Does VIVA `Transaction Type == "SA"` mean "sale"? Should refunds or other types be included? | [04](04-functionality.md#tab-3-cashier-monitoring) |
| Q6 | Should the emailed Ticket Hour attachment use the friendly column labels, like the on-screen preview does, or the internal names it uses now? | [04](04-functionality.md#tab-2-send-emails) |
| Q7 | Which source systems produce each upload file? Are their layouts stable, and who do you contact when they change? | [03](03-data-and-databases.md#uploaded-file-formats-what-the-readers-expect) |
| Q8 | Is there a **matched** sample set (Cashier + Moneygram + VIVA for the same day) for regression testing? | [05](05-setup-and-running.md#tests) |

## Data and databases

| # | Question | Raised in |
|---|---|---|
| Q9 | Where is the authoritative copy of `DB/csi_db.db` and `DB/credentials.sqlite`? Who owns them? Are they backed up, and how? | [05](05-setup-and-running.md#first-time-setup), [06](06-deployment.md) |
| Q10 | Is `city_emails2` a test-recipient list? It's now selected by `cashier_emails_table` in the `default`/`dev` profiles. Who maintains it? | [03](03-data-and-databases.md#dbcsi_dbdb-table-reference) |
| Q11 | Is the `cashier_per_store` table, and the dead "add statement to DB" code, retired for good? Can both be deleted? | [03](03-data-and-databases.md#dbcsi_dbdb-table-reference) |
| Q12 | How was `credentials.sqlite` first created (`Scripts/01-create_db_users.R`?), and who holds admin accounts? | [03](03-data-and-databases.md#dbcredentialssqlite-users) |
| Q13 | Can `DB/users.db` and `DB/credentials.csv` be deleted? Do they hold plaintext passwords? | [03](03-data-and-databases.md#dbcredentialssqlite-users), [07](07-maintenance-guide.md#security-and-data-protection) |
| Q25 | In the **production** DB, do `cashier_groups` rows have NULL `uuid`s, as 36 of 37 do in the local copy? If so, Edit and Delete don't work for them, and a one-off data fix is needed. | [07](07-maintenance-guide.md#correctness-bugs) |

## Secrets and security

| # | Question | Raised in |
|---|---|---|
| Q14 | Is (or was) the GitHub repo **public**? That decides whether the historic `DB/csi_db.db` commits (store email addresses) and the passphrase in older commits of `inst/golem-config.yml` (now git-ignored) count as exposed. | [07](07-maintenance-guide.md#security-and-data-protection) |
| Q15 | What's the production SMTP provider and host, and which sender account does it use? | [05](05-setup-and-running.md#smtp-credentials) |
| Q16 | Which Windows account on the server owns the `acs` keyring entry, and who can re-create it? | [03](03-data-and-databases.md#credentials-and-where-they-live-no-values), [06](06-deployment.md) |
| Q17 | Who knows the sheet-protection password of `data-raw/template_report.xlsx`? | [03](03-data-and-databases.md#credentials-and-where-they-live-no-values), [07](07-maintenance-guide.md#change-an-excel-template) |

## Deployment

| # | Question | Raised in |
|---|---|---|
| Q18 | Production server hostname, and the app folder. The old launcher used `C:\Users\administrator.ACSCY\Documents\CSIemail`; is that still right? | [06](06-deployment.md) |
| Q19 | *Partly answered:* it's an **NSSM service** (per `How to set up the NSSM service.txt`), now pointed at `Rscript.exe run_app.R`. What's the **service name**, and which **Windows account** does it log on as? It must own the `acs` keyring entry. | [06](06-deployment.md#16-point-the-nssm-service-at-run_appr) |
| Q20 | Is R 4.2.3 installed on the server (`renv.lock` needs it)? | [05](05-setup-and-running.md#requirements), [06](06-deployment.md) |
| Q21 | How does code get to the server: `git pull`, manual copy, or OneDrive sync? | [06](06-deployment.md) |
| Q22 | Which branch is deployed, `master` or `dev`? The two have diverged: `dev` is 13 ahead and `master` is 3 ahead. | [06](06-deployment.md) |
| Q23 | Is port 3846 still correct? (An old log shows 3845.) Is anything (proxy, firewall rule) in front of it? | [06](06-deployment.md) |
| Q24 | Is there an existing rollback or release practice? Is stdout logged anywhere by the supervisor? | [06](06-deployment.md#rollback), [06](06-deployment.md#logs-and-monitoring) |
