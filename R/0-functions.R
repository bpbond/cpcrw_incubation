# Common constant, file location, and function definitions
# This support script is source'd by every other script before executing
# Ben Bond-Lamberty March 2015

# -----------------------------------------------------------------------------
# Run under:
#   R version 3.2.3 (2015-12-10), x86_64-apple-darwin13.4.0 (64-bit)
# Key packages used across scripts:
library(ggplot2)       # 2.1.0
theme_set(theme_bw())
library(dplyr)         # 0.5.0
library(readr)         # 1.0.0
library(lubridate)     # 1.6.0
library(stringr)       # 1.1.0
library(luzlogr)       # 0.2.0

#if(require(checkpoint))   # 0.3.15
#    try(checkpoint("2016-04-01")) # 'try' b/c errors w/o network

# -----------------------------------------------------------------------------
# Parameters for key analytical choices
# Defined here so they can be easily used in code AND manuscript
MAX_MINCONC_TIME <- 10     # the minimum concentration has to occur in first 10 s
MAX_MAXCONC_TIME <- 45     # the maximum concentration has to occur in first 45 s
OUTLIER_GROUPS   <- 10     # Divide data into date groups and identify outliers in each
CO2_EXCLUDE_DEVS <- 5.0    # CO2 outlier boundary, in mean absolute deviations
CH4_EXCLUDE_DEVS <- 10.0   # CH4 outlier boundary, in mean absolute deviations
MAX_CO2_CV       <- 1.4    # Max allowable CO2 CV for a given core on a given day
                           # See https://github.com/bpbond/cpcrw_incubation/issues/44
FLUX_ADDITION <- 0.1       # Added to fluxes before log transformation
PARTICLE_DENSITY <- 2.65   # g/cm3

# -----------------------------------------------------------------------------
# Key files and directories shared between scripts
OUTPUT_DIR		         <- "outputs/"
DIAGNOSTICS_DIR        <- "qc_plots/"

RAWDATA_FILE           <- file.path(OUTPUT_DIR, "rawdata.csv.gz")
RAWDATA_SAMPLES_FILE   <- file.path(OUTPUT_DIR, "rawdata_samples.csv.gz")
VALVEMAP               <- "data/valvemap.csv"
TREATMENTS             <- "data/treatments.csv"
REMOVEDDATA_FILE       <- file.path(OUTPUT_DIR, "removed_data.csv")
SUMMARYDATA_FILE       <- file.path(OUTPUT_DIR, "summarydata.csv")
SUMMARYDATA_CLEAN_FILE <- file.path(OUTPUT_DIR, "summarydata_clean.csv")
FLUXDATA_FILE          <- file.path(OUTPUT_DIR, "fluxdata.csv")
FLUXDATA_FINAL_FILE    <- file.path(OUTPUT_DIR, "fluxdata_final.csv")
FLUXDATA_CUM_FILE      <- file.path(OUTPUT_DIR, "fluxdata_cum.csv")
FLUXDATA_CUM_CORE_FILE <- file.path(OUTPUT_DIR, "fluxdata_cum_core.csv")
COREDATA_FILE          <- "data/drymasses.csv"
CNDATA_FILE            <- "data/uga-soil_samples.csv"

# -----------------------------------------------------------------------------
# Miscellany
# Colorblind-friendly palette
# from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
CBPALETTE <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
SEPARATOR		  <- "-------------------"


# -----------------------------------------------------------------------------
# Print dimensions of data frame
print_dims <- function(d, dname = deparse(substitute(d))) {
  stopifnot(is.data.frame(d))
  printlog(dname, "rows =", nrow(d), "cols =", ncol(d))
} # print_dims

# -----------------------------------------------------------------------------
# Return output directory (perhaps inside a script-specific folder)
# If caller specifies `scriptfolder=FALSE`, return OUTPUT_DIR
# If caller specifies `scriptfolder=TRUE` (default), return OUTPUT_DIR/SCRIPTNAME
# This lets scripts easily write logs/data to their own folder, or not
outputdir <- function(scriptfolder = TRUE) {
  output_dir <- OUTPUT_DIR
  if(scriptfolder) output_dir <- file.path(output_dir, sub(".R$", "", SCRIPTNAME))
  if(!file.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir
} # outputdir

# -----------------------------------------------------------------------------
# Save a ggplot figure
save_plot <- function(pname, p = last_plot(), ptype = ".pdf", scriptfolder = TRUE, ...) {
  fn <- file.path(outputdir(scriptfolder), paste0(pname, ptype))
  printlog("Saving", fn)
  ggsave(fn, p, ...)
} # save_plot

# -----------------------------------------------------------------------------
# Save a data frame
save_data <- function(df, 
                      fn = paste0(deparse(substitute(df)), ".csv"), 
                      scriptfolder = TRUE, 
                      gzip = FALSE, ...) {
  # If no directory name supplied, construct a path by calling outputdir()
  if(dirname(fn) == ".") {
    fqfn <- file.path(outputdir(scriptfolder), fn)
  } else {
    if(scriptfolder) warning("Not writing to script folder")
    fqfn <- fn
  }
  
  # If ends in ".gz" strip off and set gzip flag
  if(grepl(".gz$", fqfn)) {
    fqfn <- gsub(".gz$", "", fqfn)
    gzip <- TRUE
  }
  
  printlog("Saving", fqfn)    
  readr::write_csv(df, path = fqfn, ...)
  
  # If gzip flag set, or filename ends with ".gz", attempt to gzip
  if(gzip) {
    if(require(R.utils, quietly = TRUE)) {
      printlog("gzipping", fqfn)
      R.utils::gzip(fqfn, overwrite = TRUE)    
    } else {
      warning("R.utils not available - can't gzip")
    }
  }
} # save_data

# -----------------------------------------------------------------------------
# Identify outliers.
# See: Davies, P.L. and Gather, U. (1993), "The identification of multiple outliers"
# (with discussion), J. Amer. Statist. Assoc., 88, 782-801.
is_outlier <- function(x, devs = 3.2) {
  x <- na.omit(x)
  lims <- median(x) + c(-1, 1) * devs * mad(x, constant = 1)
  x < lims[1] | x > lims[2]
} # is_outlier

# -----------------------------------------------------------------------------
# Save a plot to the diagnostic plot folder (used by README.md)
save_diagnostic <- function(p, pname, printit = TRUE, ...) {
  if(printit) print(p)
  printlog("Saving diagnostic for", pname)
  paste0(pname, ".png") %>%
    file.path(DIAGNOSTICS_DIR, .) %>%
    ggsave
  save_plot(pname, ...)
} # save_diagnostic

# -----------------------------------------------------------------------------
# 'Pretty n' function to round a numeric value and print that # of digits
pn <- function(x, n) {
  formatC(round(unlist(x), n), digits = n, format = "f")
} # pn

# -----------------------------------------------------------------------------
# 'Clean p value' function to pretty-print p value(s), specifically
pclean <- function(x, digits = 3, printP = TRUE) {
  x <- as.vector(x)
  ltstring <- paste0("< 0.", paste(rep("0", digits - 1), collapse = ""), "1")
  valstring <- ifelse(x < 10 ^ -digits, ltstring, pn(x, digits))
  if(printP) {
    paste("P", ifelse(x < 10 ^ -digits, valstring, paste("=", valstring)))
  } else {
    valstring
  }
} # pclean
