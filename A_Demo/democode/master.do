
********************************************************************************
*
*  MASTER DO FILE — Severe Floods and Opioid Overdose Mortality
*
*  This script runs all Stata analyses and exports coefficient tables
*  for R figure generation. Edit the root path below, then run this file.
*
*  After running this file, open R and run the figure scripts in:
*    code/figures/
*
*  Software: Stata 19 with eventstudyinteract package
*            (see docs/InstallationGuide.pdf)
*
********************************************************************************

	clear all
	set more off

	** ══════════════════════════════════════════════════════════════════════
	**  SET THIS PATH to the location of this replication folder
	** ══════════════════════════════════════════════════════════════════════

	global root  "SET THIS PATH: location of CodeShare_severefloods_opioidoverdosemortality_April2025 folder on your computer"

	** ══════════════════════════════════════════════════════════════════════

	** Derived paths (do not edit)
	global data    "$root/A_Demo/demodata"
	global code    "$root/A_Demo/democode"
	global tables  "$root/A_Demo/democode/output/tables"
	global figures "$root/A_Demo/democode/output/figures"
	global logs    "$root/A_Demo/democode/output/logs"

	capture mkdir "$tables"
	capture mkdir "$figures"
	capture mkdir "$logs"

	** Log everything
	capture log close _all
	log using "$logs/master.log", replace name(master)

	di _n "============================================================"
	di    "  Replication: Severe Floods and Opioid Overdose Mortality"
	di    "  Root: $root"
	di    "  Date: `c(current_date)' `c(current_time)'"
	di    "============================================================"

	** ── 01: Main Analysis ──────────────────────────────────────────────
	di _n ">>> Running 01_main_analysis.do ..."
	do "$code/01_main_analysis.do"

	** ── 02: Subgroup Comparison ────────────────────────────────────────
	di _n ">>> Running 02_subgroup_comparison.do ..."
	do "$code/02_subgroup_comparison.do"

	** ── 03: Robustness Checks ─────────────────────────────────────────
	di _n ">>> Running 03_robustness_checks.do ..."
	do "$code/03_robustness_checks.do"

	** ── 04: Migration Robustness ──────────────────────────────────────
	di _n ">>> Running 04_migration_robustness.do ..."
	do "$code/04_migration_robustness.do"

	** ── 05: Mortality Burden ──────────────────────────────────────────
	di _n ">>> Running 05_mortality_burden.do ..."
	do "$code/05_mortality_burden.do"

	di _n "============================================================"
	di    "  All Stata analyses complete."
	di    "  Coefficient tables saved to: $tables"
	di    "  Next: Run R scripts in A_Demo/democode/figures/ to generate figures."
	di    "============================================================"

	log close master
