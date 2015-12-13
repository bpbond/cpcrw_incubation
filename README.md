# **DISCLAIMER**
**All the data and results in this repository are provisional and subject to normal scientific fair use expectations.** In particular, the diagnostics below present data that may not have been fully QC'd, checked, or interpreted. We do our best, but make no guarantees as to its correctness.

# cpcrw_incubation
PNNL TES incubation of 2015 CPCRW cores. This is an **#openexperiment** with all data, code, diagnostics, and code posted here. See below for details and experimental protocol; more information and photos can be found [here](http://bpbond.github.io/cpcrw_incubation).

This work is funded by DOE's [Terrestrial Ecosystem Science Program](http://tes.science.energy.gov). 

## Current diagnostics

Latest data: 2015-12-09 (incubation day 101)

Script run: 2015-12-13 14:12:10

### Missing/problematic data

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/samples_by_date.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/orphan_samples.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/missing_mass.png)

### Observations over time

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/masses.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/masses_relative.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CO2_time.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CH4_time.png)

### Variability and outliers

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CO2_CV.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CH4_CV.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/coreCV_combined.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/coreCV_distribution.png)

### Raw data

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CO2_incday.png)

![](https://github.com/bpbond/cpcrw_incubation/blob/master/qc_plots/CH4_incday.png)

## Script organization

The R scripts are designed to be run in numerical order, in the sense that each depends on one or more outputs of previous scripts.

* `0-functions.R` - Provides common support functions. Not intended to be called directly, as it's `source`'d from all other scripts.
* `1-data.R` - reads in raw Picarro data files, checks that they're in a consistent format, and writes a single combined `rawdata.csv.gz` file.
* `2-summarize.R` - The workhorse script that summarized the raw data, combines it with ancillary measurements, and writes a `summarydata.csv` output file. Specifically:
  * Reads in the raw data, discards fractional valve position data, makes POSIXct dates
  * Assigns **sample numbers** based on when the valve position changes and computes elapsed seconds within each sample
  * Loads the valve map (`data/valvemap.csv`) and does some QC on it
  * Summarizes the raw data, computing min/max for each gas, N, etc., and matching each sample number with a row in the valve map based on the timestamp and valve number
  * Merges the summary data with the valve map and treatment data information
  * Computes per-second rates
* `3-remove.R` - Loads the removals file (`data/removals.csv`) and uses it to remove problematic data from the summarized data.
* `4-qc.R` - Runs a series of diagnostic checks on the summarized data, and then updates the `README.md` timestamps:
  * Number of samples for each core, by date
  * Orphan samples (Picarro data with no matching core number and/or mass data)
  * Variability between the core measurements on a given date, and detailed raw data plot of the highest
  * Variability between cores on a given date, for each treatment
  * Flux summaries by treatment
  * Mass data over time, for each core
* Other scripts are not functional yet.

## Study objectives and approach

**Objective:**

Examine how mineral boreal soils, sampled from the active layer just above permafrost at [CPCRW](http://www.lter.uaf.edu/bnz_cpcrw.cfm), produce GHGs: how production varies with soil moisture, temperature, and other factors.

**Rationale:**

The balance between CO2 and CH4 emissions from soils allows for particularly interesting manipulative experiments and modeling tests, as CH4 is formed under specific conditions and can be readily oxidized back to CO2. This balance thus serves as a particularly interesting mechanistic probe, and stringent test of our understanding and models.

**Approach:**

Sample CPCRW soils just above August frozen soil level–i.e., the base of the active layer using the large augur (7.5 cm w by 30 cm h). Cores should be kept cold, though don’t have to stay frozen, and shipped to PNNL.

Sampling will, as much as possible, take place within the open call transects This will allow the two projects to compare spatial similarities in active layer and permafrost soil respiration and conditions.  We anticipate sampling will take place in the black spruce-to-birch hillslope transition zone with an average separation of 2-3 m.

Split cores into six groups:

1. Immediate destructive analyses
2. Maintain at constant (field conditions) water content at 4 °C
3. Drydown (no water added) at 4 °C
4. Maintain at constant (field conditions) water content at 22 °C
5. Maintain at the higher 4 °C group water content (as closely as possible to the corresponding point in time) but at 22 °C
6. Drydown (no water added) at 22 °C

This is designed to give us two control groups that maintain field moisture and different temperatures; two drydown groups; and a drydown group at 22 °C that (hopefully) follows the 4 °C moisture tracks, allowing us to unambiguously separate temperature and moisture effects.

Groups (ii)-(vi) will be incubated for ~100 days at constant temperatures. Measure mass, adjust water content–for groups (ii), (iv), (v)–and measure CO2/CH4 evolution periodically. Destructive analyses at end of incubation.

This will require 6 groups x 6 reps = 36 cores.

The 4 and 22 °C incubation temperatures are driven by logistics (the PNNL growth chambers that can accommodate the samples and Picarro setup), but follow the incubation design of [Treat et al. (2013)](http://dx.doi.org/10.1038/nclimate2010).

