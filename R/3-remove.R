# Process Picarro data for CPCRW incubation
# Remove bad/freaky observations
# Ben Bond-Lamberty September 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "3-remove.R"
PROBLEM       <- FALSE

SUMMARYDATA  <- file.path(OUTPUT_DIR, "summarydata.csv")  # output from script 2
REMOVALS     <- file.path("data/", "removals.csv")

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
print_dims(summarydata)

summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)

printlog("Reading in removal data...")
removals <- read_csv(REMOVALS, skip = 1)

summarydata_clean <- summarydata
removed_all <- data.frame()
for(i in seq_len(nrow(removals))) {
  rem <- removals[i,]
  printlog(SEPARATOR)
  printlog("Removal", i)
  printlog("Reason:", rem$Reason)
  
  if(is.na(rem$Reason)) {
    printlog("SKIPPED - 'reason' is required, but not provided!")
    next
  }
  
  if(all(is.na(c(rem$min_CO2_dry, rem$max_CO2_dry, rem$min_CH4_dry, rem$max_CH4_dry)))) {
    printlog("Removed entire sample", rem$samplenum)
    removed <- subset(summarydata_clean, samplenum == rem$samplenum)
    summarydata_clean <- subset(summarydata_clean, samplenum != rem$samplenum)
  } else {
    stop("Not implemented yet!")
  }
  
  printlog("Summary data observations removed:", nrow(removed))
  removed_all <- rbind(removed_all, removed)
}


# Done! 

printlog(SEPARATOR)
printlog("All done with removals")
save_data(summarydata_clean, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
