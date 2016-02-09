# R

This folder contains all R code for processing experimental data and analyzing results. These scripts are intended to be run in numerical order:

* `0-functions.R`: Support functions used by all other scripts. (This one doesn't have to be invoked directly.)
* `1-data.R`: This script reads all available Picarro outputs in `data/picarro/`, concatenating and writing to an `outputs/rawdata.csv.gz` file.
* `2-summarize.R`: This workhorse script summarizes individual (raw) Picarro observations to  summaries of "samples" (groups of consecutive observations made from a given core at a point in time). It computes gas concentration changes, performs some QC, merges the Picarro data with valve map and other ancillary data, and writes `outputs/summarydata.csv`.
* `3-remove.R` : This script removes known bad/freaky observations from the data, based on the `data/removals.csv` list. It then writes `outputs/summarydata_clean.csv`.
* `3.3-qc.R` : This **OPTIONAL** script produces a wide range of diagnostic and QC plots, of both the raw and (clean) summary datasets.
* `3.5-investigate.R` : This **OPTIONAL** and **INTERACTIVE** script is used to investigate treatment summaries with particularly high variability. Note this is only used to generate figures and statistics, identifying candidates to add to the `removals.csv` file; it doesn't make any changes to the datasets.
* `4-fluxes.R` : Computes fluxes, flags outlier fluxes, and computes cumulative fluxes (first removing and interpolating the outliers). 
* `5-analyze.R` : Final figures and runs statistical tests. This script is called by the `cpcrw_manuscript.Rmd` presentation file that generates the manuscript.

All these scripts get their inputs entirely from disk--i.e., they don't depend on any data being present in their runtime environment. They also automatically create their own output folder in `outputs/` (creating that too if necessary).
