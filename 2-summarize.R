# Process Picarro data for Peyton's DWP lab experiment
# This script summarizes individual Picarro observations to summaries
# of "samples" (groups of observations made from a given core at some
# point in time)
# Ben Bond-Lamberty July 2015

source("0-functions.R")

SCRIPTNAME  	<- "2-summarize.R"
PROBLEM       <- FALSE

RAWDATA      <- file.path(OUTPUT_DIR, "rawdata.csv.gz")  # output from script 1
VALVEMAP     <- "data/valvemap.csv"
TREATMENTS   <- "data/treatments.csv"

library(stringr)

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
rawdata <- gzfile(RAWDATA) %>% readr::read_csv(col_types = "ccddddiiiddddddddc")
print_dims(rawdata)
print(summary(rawdata))

# Fractional solenoid values mean that the analyzer was shifting
# between two samples. Discard these.
printlog( "Removing fractional MPVPosition" )
rawdata <- subset(rawdata, MPVPosition == trunc(MPVPosition))

# Handle dates
printlog( "Converting date/time info to POSIXct..." )
rawdata$DATETIME <- ymd_hms(paste(rawdata$DATE, rawdata$TIME))
# We decided to use UTC only (Peyton email 9/4/15) so this is commented out
# # Force Picarro timestamps to Pacific time (UTC -8:00)
# printlog("Picarro timestamps to Pacific time")
# rawdata$DATETIME <- rawdata$DATETIME - 60 * 60 * 8
printlog("First timestamp:")
print(min(rawdata$DATETIME))
printlog("Last timestamp:")
print(max(rawdata$DATETIME))

printlog( "Sorting by date..." )
rawdata <- arrange(rawdata, DATETIME)

# Assign a different sample number to each sample group 
# (we know we're on a new sample when MPVPosition changes)
printlog("Assigning sample numbers...")
oldsampleflag <- with(rawdata, c(FALSE, MPVPosition[-length(MPVPosition)] == MPVPosition[-1]))
rawdata$samplenum <- cumsum(!oldsampleflag)

printlog("Computing elapsed seconds...")
rawdata_samples <- rawdata %>%
  group_by(samplenum) %>%
  mutate(elapsed_seconds = (FRAC_HRS_SINCE_JAN1 - min(FRAC_HRS_SINCE_JAN1)) * 60 * 60) 

# Load MPVPosition map
printlog("Loading valve map data...")
valvemap <- read_csv(VALVEMAP, skip = 1)
printlog( "Converting date/time info to POSIXct..." )
valvemap$StartDateTime <- mdy_hm(paste(valvemap$Date, valvemap$Time_set_start))
valvemap <- arrange(valvemap, StartDateTime)
valvemap$valvemaprow <- 1:nrow(valvemap)

# QC the valve map
dupes <- valvemap %>% 
  filter(Core != "Ambient" & !is.na(MPVPosition)) %>%
  group_by(StartDateTime, MPVPosition) %>% 
  summarise(n=n()) %>% 
  filter(n > 1)
if(nrow(dupes)) {
  printlog("WARNING - MULTIPLE CORES ASSIGNED TO A VALVE ON A GIVEN DATE")
  print(dupes)
  printlog("WARNING - this will screw up the matching to Picarro data")
  PROBLEM <- TRUE
}
nomass <- valvemap %>% 
  filter(Core != "Ambient4" & Core != "Ambient22" & is.na(Mass_g))
if(nrow(nomass)) {
  printlog("WARNING - some cores have no mass data")
  print(nomass)
  PROBLEM <- TRUE
}
# Function to match up Picarro data with mapping file data
# This is done by date and valve number (see plot saved above)
matchfun <- function(DATETIME, MPVPosition) {
  DATETIME <- as.POSIXct(DATETIME, origin = lubridate::origin, tz="UTC")
  rowmatches <- which(DATETIME >= valvemap$StartDateTime & 
                        yday(DATETIME) == yday(valvemap$StartDateTime) &
                        MPVPosition == valvemap$MPVPosition)
  if(length(rowmatches) == 0) rowmatches <- NA
  max(rowmatches)  # return latest time match
}

printlog( "Computing summary statistics for each sample..." )
# The window in which we look for min and max concentrations
MAX_MINCONC_TIME <- 10  # the minimum concentration has to occur in first 10 s
MAX_MAXCONC_TIME <- 45  # the maximum concentration has to occur in first 45 s

# We want to apply different criteria here, so three different pipelines
# to compute the min and max gas concentrations
summarydata_min <- rawdata_samples %>%
  filter(elapsed_seconds <= MAX_MINCONC_TIME) %>%
  group_by(samplenum) %>%
  summarise(
    min_CO2 = min(CO2_dry),
    min_CO2_time = nth(elapsed_seconds, which.min(CO2_dry)),
    min_CH4 = min(CH4_dry),
    min_CH4_time = nth(elapsed_seconds, which.min(CH4_dry))
  )

# Now we want to look for the max concentration AFTER the minimum
rawdata_temp <- rawdata_samples %>%
  left_join(summarydata_min, by="samplenum") 
summarydata_maxCO2 <- rawdata_temp %>%
  filter(elapsed_seconds > min_CO2_time & elapsed_seconds < MAX_MAXCONC_TIME) %>%
  summarise(
    max_CO2 = max(CO2_dry),
    max_CO2_time = nth(elapsed_seconds, which.max(CO2_dry))
  )
summarydata_maxCH4 <- rawdata_temp %>%
  filter(elapsed_seconds > min_CH4_time & elapsed_seconds < MAX_MAXCONC_TIME) %>%
  summarise(
    max_CH4 = max(CH4_dry),
    max_CH4_time = nth(elapsed_seconds, which.max(CH4_dry))
  )

# Final pipeline: misc other data, and match up with valve map entries
summarydata_other <- rawdata_samples %>%
  group_by(samplenum) %>%
  summarise(
    DATETIME = mean(DATETIME),
    N = n(),
    MPVPosition	= mean(MPVPosition),
    h2o_reported = mean(h2o_reported),
    valvemaprow = matchfun(DATETIME, MPVPosition)
  ) 

# Merge pieces together to form final summary data set
printlog("Removing N=1 and MPVPosition=0 data, and merging...")
summarydata <- summarydata_other %>%
  filter(N > 1) %>% # N=1 observations are...? Picarro quirk
  filter(MPVPosition > 0) %>% # ? Picarro quirk
  left_join(summarydata_min, by="samplenum") %>%
  left_join(summarydata_maxCO2, by="samplenum") %>% 
  left_join(summarydata_maxCH4, by="samplenum")

printlog("Merging Picarro and mapping data...")
summarydata <- left_join(summarydata, valvemap, by=c("MPVPosition", "valvemaprow"), all.x=TRUE)

printlog("Reading and merging treatment data...")
trtdata <- read_csv(TREATMENTS, skip=1)
summarydata <- left_join(summarydata, trtdata, by="Core")

printlog("Computing per-second rates...")
summarydata <- summarydata %>%
  mutate(CO2_ppm_s = (max_CO2 - min_CO2) / (max_CO2_time - min_CO2_time),
         CH4_ppb_s = (max_CH4 - min_CH4) / (max_CH4_time - min_CH4_time),
         incday = 1 + as.numeric(difftime(DATETIME, min(DATETIME), units = "days")))

printlog("Saving a comparison of MPVPosition sequence in Picarro data and valvemap")
checkdata <- select(summarydata, DATETIME, MPVPosition)
checkdata$sequence <- seq_len(nrow(checkdata))
vdata <- data.frame(sequence = seq_len(nrow(valvemap)),
                    DATETIME_valvemap = paste(valvemap$Date, valvemap$Time_set_start_UTC),
                    MPVPosition_valvemap = valvemap$MPVPosition)
MPVPosition_checkdata <- left_join(vdata, checkdata, by="sequence")
save_data(MPVPosition_checkdata)

# Done! 

save_data(summarydata, scriptfolder=FALSE)

#summarydata <- subset(summarydata, !is.na(CORE), select=-c(MPVPosition, valvemaprow))	
save_data(rawdata_samples, scriptfolder=FALSE, gzip=TRUE)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
