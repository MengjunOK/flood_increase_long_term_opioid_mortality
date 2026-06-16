# Severe Floods and Long-Term Opioid Overdose Mortality — Replication Package

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20722600.svg)](https://doi.org/10.5281/zenodo.20722600)

Replication code and (de-identified) data for the analyses in the paper. The analysis estimates
the effect of severe flooding on county-level opioid overdose mortality in rural Appalachian
counties using a Sun and Abraham (2021) event-study design.

**👉 Full setup, software requirements, repository structure, and run instructions are in
[`DemoCode_Instruction.pdf`](DemoCode_Instruction.pdf).** Data sources and ICD-10 code definitions
are in [`DataSources.pdf`](DataSources.pdf).

---

## ⚠️ Data availability — please read first

The county-by-year **opioid overdose death counts** used in this study come from the
**CDC/NCHS Restricted-Use Detailed Multiple Cause of Death (MCD) micro-data**, which we cannot
redistribute under our NCHS data-use agreement.

In the dataset included here (`A_Demo/demodata/floodopioid_demo.dta`), **all 33 overdose-death
variables are set to missing (`.`)**, but the variable **names and labels are retained** so the
data structure and all code run unchanged. The suppressed variables are:

```
death_alldrug_*   death_opioid_*   death_pre_*
```
(for population groups: `allpop, white, nonwhite, male, female, less25, 2544, 4564, over65, leh, hh`)

**To reproduce the analysis, obtain the death counts and merge them in by `county_fips` × `year`:**

1. **Public (suppressed) version — anyone can download.** Use **CDC WONDER, Multiple Cause of
   Death**: <https://wonder.cdc.gov/mcd.html>. Counts below 10 are suppressed by CDC, so this
   reproduces the *public* version. Identify drug-overdose and opioid-involved deaths with the
   ICD-10 codes in `DataSources.pdf` (underlying cause X40–X44, X60–X64, X85, Y10–Y14; contributing
   cause T40.0–T40.6).
2. **Restricted (full) version — for exact replication.** The unsuppressed micro-data require a
   research proposal approved by NCHS: <https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm>.

All other variables (population, economic, education, flood exposure, event-time indicators,
migration) are public and included with full values.

> The county boundary shapefile and the county-level intermediate burden files are not shipped
> (size / restricted-data derivation); see `DemoCode_Instruction.pdf` for how to obtain or
> regenerate them.

---

## Data Availability Statement (draft for the manuscript)

> All analysis code is publicly available at
> https://github.com/MengjunOK/flood_increase_long_term_opioid_mortality and archived at Zenodo
> (DOI: 10.5281/zenodo.20722600). All data needed to reproduce the analyses are publicly available
> except the county-level opioid overdose death counts, which derive from the CDC/NCHS
> Restricted-Use Multiple Cause of Death micro-data and cannot be redistributed under the NCHS
> data-use agreement. A suppressed (counts < 10 removed) public version can be obtained from CDC
> WONDER (https://wonder.cdc.gov/mcd.html); the restricted micro-data are available to researchers
> via NCHS proposal review (https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm). The repository
> includes the full data structure with these variables set to missing, together with the ICD-10
> code definitions needed to reconstruct them.

## License

See `LICENSE.pdf`.
