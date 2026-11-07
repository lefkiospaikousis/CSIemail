@ECHO off
REM ===========================================================================
REM DEPRECATED - NOT USED ANYMORE (since 2026-11-08, version 0.3.2)
REM
REM The app is now started by the NSSM service pointing straight at
REM R 4.2.3's Rscript.exe with the launcher run_app.R (project root) as its
REM argument - no .bat file. See docs\handover\06-deployment.md.
REM
REM Kept only for reference and for rolling back to an older version. It calls
REM R 4.1.0, which does not match renv.lock (R 4.2.3).
REM ===========================================================================


"C:\Program Files\R\R-4.1.0\bin\x64\Rscript.exe" %~dp0\launch_on_browser.R 

REM >> %~dp0\launch_on_browser.log 2>&1