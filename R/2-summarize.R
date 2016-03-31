# Process Picarro data for Peyton's DWP lab experiment
# This workhorse script summarizes individual (raw) Picarro observations to 
# summaries of "samples" (groups of consecutive observations made from a given 
# core at a point in time). It computes gas concentration changes, performs 
# some QC, merges the Picarro data with valve map and other ancillary data,
# and writes `outputs/summarydata.csv`.
# 
# Ben Bond-Lamberty July 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "2-summarize.R"
PROBLEM       <- FALSE


# -----------------------------------------------------------------------------
# QC the valvemap data
qc_valvemap <- function(valvemap) {
  # QC the valve map
  dupes <- valvemap %>% 
    filter(Core != "Ambient" & !is.na(MPVPosition)) %>%
    group_by(StartDateTime, MPVPosition) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
  if(nrow(dupes)) {
    flaglog("WARNING - MULTIPLE CORES ASSIGNED TO A VALVE ON A GIVEN DATE")
    print(dupes)
    printlog("WARNING - this will screw up the matching to Picarro data")
    PROBLEM <- TRUE
  }
  nomass <- valvemap %>% 
    filter(Core != "Ambient4" & Core != "Ambient20" & is.na(Mass_g))
  if(nrow(nomass)) {
    printlog("WARNING - some cores have no mass data")
    print(nomass)
    PROBLEM <- TRUE
  }
} # qc_valvemap

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
rawdata <- read_csv(RAWDATA_FILE, col_types = "ccddddiiiddddddddc") %>%
#readr::read_csv(file.path(INPUT_DIR, RAWDATA_FILE), col_types = "ccddddiiiddddddddc") %>%
  # immediate discard columns we don't need
  select(DATE, TIME, MPVPosition, CH4_dry, CO2_dry, h2o_reported)
print_dims(rawdata)
print(summary(rawdata))

# -----------------------------------------------------------------------------
# Prep work: data cleaning, dates, sample numbers

# Fractional solenoid values mean that the analyzer was shifting
# between two samples. Discard these.
printlog("Removing fractional MPVPosition")
rawdata <- subset(rawdata, MPVPosition == trunc(MPVPosition))

# Handle dates
printlog("Converting date/time info to POSIXct...")
rawdata$DATETIME <- ymd_hms(paste(rawdata$DATE, rawdata$TIME))
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
  mutate(elapsed_seconds = difftime(DATETIME, min(DATETIME), units = "secs"))

# Load and QC the valvemap data
printlog("Loading valve map data...")
valvemap <- read_csv(VALVEMAP, skip = 1, col_types = "ccnncdcc")
printlog( "Converting date/time info to POSIXct..." )
valvemap$StartDateTime <- mdy_hm(paste(valvemap$Date, valvemap$Time_set_start_UTC))
valvemap <- arrange(valvemap, StartDateTime)
valvemap$valvemaprow <- seq_len(nrow(valvemap))
qc_valvemap(valvemap)

# -----------------------------------------------------------------------------
# Compute concentration changes and match the Picarro data with valvemap data

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
  summarise(min_CO2 = min(CO2_dry),
            min_CO2_time = nth(elapsed_seconds, which.min(CO2_dry)),
            min_CH4 = min(CH4_dry),
            min_CH4_time = nth(elapsed_seconds, which.min(CH4_dry)))

# Now we want to look for the max concentration AFTER the minimum
rawdata_temp <- rawdata_samples %>%
  left_join(summarydata_min, by = "samplenum") 

summarydata_maxCO2 <- rawdata_temp %>%
  filter(elapsed_seconds > min_CO2_time & elapsed_seconds < MAX_MAXCONC_TIME) %>%
  summarise(max_CO2 = max(CO2_dry),
            max_CO2_time = nth(elapsed_seconds, which.max(CO2_dry))
            )
summarydata_maxCH4 <- rawdata_temp %>%
  filter(elapsed_seconds > min_CH4_time & elapsed_seconds < MAX_MAXCONC_TIME) %>%
  summarise(max_CH4 = max(CH4_dry),
            max_CH4_time = nth(elapsed_seconds, which.max(CH4_dry)))

# Final pipeline: misc other data, and match up with valve map entries
summarydata_other <- rawdata_samples %>%
  group_by(samplenum) %>%
  summarise(
    DATETIME = mean(DATETIME),
    N = n(),
    MPVPosition	= mean(MPVPosition),
    h2o_reported = mean(h2o_reported),
    valvemaprow = matchfun(DATETIME, MPVPosition)) 

# Merge pieces together to form final summary data set
printlog("Removing N=1 and MPVPosition=0 data, and merging...")
summarydata <- summarydata_other %>%
  filter(N > 1) %>% # N=1 observations are...? Picarro quirk
  filter(MPVPosition > 0) %>% # ? Picarro quirk
  left_join(summarydata_min, by = "samplenum") %>%
  left_join(summarydata_maxCO2, by = "samplenum") %>% 
  left_join(summarydata_maxCH4, by = "samplenum")

printlog("Merging Picarro and mapping data...")
summarydata <- left_join(summarydata, valvemap, by = c("MPVPosition", "valvemaprow"), all.x=TRUE)

printlog("Reading and merging treatment data...")
trtdata <- read_csv(TREATMENTS, skip = 1)
summarydata <- left_join(summarydata, trtdata, by = "Core")

printlog("Computing per-second rates...")
summarydata <- summarydata %>%
  mutate(CO2_ppm_s = (max_CO2 - min_CO2) / (max_CO2_time - min_CO2_time),
         CH4_ppb_s = (max_CH4 - min_CH4) / (max_CH4_time - min_CH4_time),
         inctime_days = 1 + as.numeric(difftime(DATETIME, min(DATETIME), units = "days")))

printlog("Saving a comparison of MPVPosition sequence in Picarro data and valvemap")
checkdata <- select(summarydata, DATETIME, MPVPosition)
checkdata$sequence <- seq_len(nrow(checkdata))
vdata <- data.frame(sequence = seq_len(nrow(valvemap)),
                    DATETIME_valvemap = paste(valvemap$Date, valvemap$Time_set_start_UTC),
                    MPVPosition_valvemap = valvemap$MPVPosition)
MPVPosition_checkdata <- left_join(vdata, checkdata, by = "sequence")
save_data(MPVPosition_checkdata)

# -----------------------------------------------------------------------------
# Done! 

save_data(summarydata, fn = SUMMARYDATA_FILE, scriptfolder = FALSE)
save_data(rawdata_samples, fn = RAWDATA_SAMPLES_FILE, scriptfolder = FALSE)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
