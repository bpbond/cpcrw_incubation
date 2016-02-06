# Process Picarro data for CPCRW incubation
# This script remove bad/freaky/outlier observations from the data, based
# on the `data/removals.csv` list. It then writes `outputs/summarydata_clean.csv`.
# Ben Bond-Lamberty September 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "2.5-remove.R"
PROBLEM       <- FALSE

CATEGORY_ERROR <- "Error"
CATEGORY_OUTLIER <- "Outlier"

SUMMARYDATA  <- file.path(OUTPUT_DIR, "summarydata.csv")  # output from script 2
REMOVALS     <- file.path("data/", "removals.csv")

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA) %>%
  mutate(DATETIME = ymd_hms(DATETIME))
print_dims(summarydata)

printlog("Reading in removal data...")
removals <- read_csv(REMOVALS, skip = 1)

summarydata_clean <- summarydata
removed_data <- data.frame()
excluded_data <- data.frame()
for(i in seq_len(nrow(removals))) {
  removed <- data.frame()
  excluded <- data.frame()
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
    } else if(rem$Category == CATEGORY_OUTLIER) {
      printlog("Excluded entire sample", rem$samplenum)
      excluded <- subset(summarydata_clean, samplenum == rem$samplenum)
    } else {
      stop("Unknown category!")
    }
    summarydata_clean <- subset(summarydata_clean, samplenum != rem$samplenum)
  } else {
    stop("Method not implemented yet!")
  }
  
  printlog("Summary data observations removed:", nrow(removed))
  removed_data <- rbind(removed_data, removed)
  printlog("Summary data observations excluded:", nrow(excluded))
  excluded_data <- rbind(excluded_data, excluded)
}


# Done! 

printlog(SEPARATOR)
printlog("All done with removals and exclusions")
save_data(summarydata_clean, scriptfolder=FALSE)

printlog("Removed data:", nrow(removed_data), "of", nrow(summarydata), "=",
         round(nrow(removed_data) / nrow(summarydata) * 100, 1), "%")
save_data(removed_data)
printlog("Excluded data:", nrow(excluded_data), "of", nrow(summarydata), "=",
         round(nrow(excluded_data) / nrow(summarydata) * 100, 1), "%")
save_data(excluded_data)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
