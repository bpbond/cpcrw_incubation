# Process Picarro data for CPCRW incubation
# This script remove bad/freaky/outlier observations from the data, based
# on the `data/removals.csv` list. It then writes `outputs/summarydata_clean.csv`.
# Ben Bond-Lamberty September 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "3-remove.R"
PROBLEM       <- FALSE

CATEGORY_ERROR <- "Error"

REMOVALS     <- file.path("data/", "removals.csv")

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
read_csv(SUMMARYDATA_FILE) %>%
  mutate(DATETIME = ymd_hms(DATETIME)) ->
  summarydata
print_dims(summarydata)

# -----------------------------------------------------------------------------
# Removals - outliers or errors that should be automatically removed

printlog("Removing pre-flagged 'Remove_this' data...")
removed_data <- subset(summarydata, !is.na(Remove_this))
summarydata <- subset(summarydata, is.na(Remove_this))
printlog("Summary data observations removed:", nrow(removed_data))

printlog("Reading in removal data...")
removals <- read_csv(REMOVALS, skip = 1)

summarydata_clean <- summarydata
for(i in seq_len(nrow(removals))) {
  removed <- data.frame()
  rem <- removals[i,]
  printlog(SEPARATOR)
  printlog("Removal", i)
  printlog("Reason:", rem$Reason)
  printlog("Category:", rem$Category)
  if(is.na(rem$Reason) | is.na(rem$Category)) {
    printlog("SKIPPED - 'reason' and `category` are both required, but not provided!")
    next
  }
  
  if(all(is.na(c(rem$min_CO2_dry, rem$max_CO2_dry, rem$min_CH4_dry, rem$max_CH4_dry)))) {
    
    if(rem$Category == CATEGORY_ERROR) {
      printlog("Removed entire sample", rem$samplenum)
      removed <- subset(summarydata_clean, samplenum == rem$samplenum)
    } else {
      stop("Unknown category!")
    }
    summarydata_clean <- subset(summarydata_clean, samplenum != rem$samplenum)
  } else {
    stop("Method not implemented yet!")
  }

  printlog("Summary data observations removed:", nrow(removed))
  removed_data <- rbind(removed_data, removed)
}

# Done! 

printlog(SEPARATOR)
printlog("All done with removals and exclusions")
save_data(summarydata_clean, fn = SUMMARYDATA_CLEAN_FILE, scriptfolder = FALSE)

printlog("Removed data:", nrow(removed_data), "of", nrow(summarydata), "=",
         round(nrow(removed_data) / nrow(summarydata) * 100, 1), "%")
save_data(removed_data, fn = REMOVEDDATA_FILE, scriptfolder = FALSE)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
