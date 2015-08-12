# Process Picarro data for Peyton's DWP lab experiment
# This script summarizes individual Picarro observations to summaries
# of "samples" (groups of observations made from a given core at some
# point in time)
# Ben Bond-Lamberty July 2015

source("0-functions.R")

SCRIPTNAME  	<- "2-summarize.R"
RAWDATA      <- file.path(OUTPUT_DIR, "rawdata.csv.gz")  # output from script 1

library(stringr)

# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
rawdata <- gzfile(RAWDATA) %>% readr::read_csv()
print_dims(rawdata)
print(summary(rawdata))

# Fractional solenoid values mean that the analyzer was shifting
# between two samples. Discard these.
printlog( "Removing fractional MPVPosition" )
rawdata <- subset(rawdata, MPVPosition == trunc(MPVPosition))

# Make true dates
printlog( "Converting date/time info to POSIXct..." )
rawdata$DATETIME <- ymd_hms(paste(rawdata$DATE, rawdata$TIME), tz="America/Los_Angeles")
# Force Picarro timestamps to Pacific time (UTC -8:00)
printlog("Picarro timestamps to Pacific time")
rawdata$DATETIME <- rawdata$DATETIME - 60 * 60 * 8
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
valvemap <- read_csv("data/DWP2014_Respiration Sample Key_21July2015.csv")
printlog( "Converting date/time info to POSIXct..." )
valvemap$STARTDATE <- mdy(valvemap$STARTDATE, tz="America/Los_Angeles")
valvemap$ENDDATE <- mdy(valvemap$ENDDATE, tz="America/Los_Angeles")
valvemap$valvemaprow <- 1:nrow(valvemap)
printlog("Trimming whitespace from categorical fields...")
valvemap$WETTING <- str_trim(valvemap$WETTING)
valvemap$INPUT <- str_trim(valvemap$INPUT)
valvemap$MOISTURE <- str_trim(valvemap$MOISTURE)
valvemap$STRUCTURE <- str_trim(valvemap$STRUCTURE)

printlog("Visualizing valve map...")
p <- ggplot(valvemap, aes(STARTDATE, MPVPosition, xend=ENDDATE, yend=MPVPosition, color=STRUCTURE))
p <- p + geom_segment(size=2)
p <- p + geom_text(aes(label=CORE), size=4, hjust=.5, vjust=-.5, show_guide=FALSE)
p <- p + ggtitle("Valve map data (showing core numbers)")
print(p)
save_plot("valvemap")

# QC the valve map
dupes <- valvemap %>% 
  group_by(paste(STARTDATE, ENDDATE), MPVPosition) %>% 
  summarise(n=n()) %>% 
  filter(n > 1)
if(nrow(dupes)) {
  printlog("WARNING - MULTIPLE CORES ASSIGNED TO A VALVE ON A GIVEN DATE")
  print(dupes)
  printlog("WARNING - this will screw up the matching to Picarro data")
}


# Function to match up Picarro data with mapping file data
# This is done by date and valve number (see plot saved above)
matchfun <- function(DATETIME, MPVPosition) {
  row <- which(DATETIME >= valvemap$STARTDATE & 
                 DATETIME <= valvemap$ENDDATE & 
                 MPVPosition == valvemap$MPVPosition)
  if(length(row) != 1) row <- NA
  row
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
    valvemaprow = matchfun(floor_date(mean(DATETIME), "day"), MPVPosition)
  )

# Merge pieces together to form final summary data set
summarydata <- summarydata_other %>%
  left_join(summarydata_min, by="samplenum") %>%
  left_join(summarydata_maxCO2, by="samplenum") %>% 
  left_join(summarydata_maxCH4, by="samplenum")

printlog("Merging Picarro and mapping data...")
summarydata <- left_join(summarydata, valvemap, by=c("MPVPosition", "valvemaprow"), all.x=TRUE)

# At this point we want to compute elapsed_minutes. The zero mark
# for this calculation is the STARTDATE + STARTTIME fields in the valve map
summarydata$STARTDATETIME <- ymd_hm(paste(summarydata$STARTDATE, 
                                          summarydata$STARTTIME), tz="America/Los_Angeles")
printlog("Computing elapsed minutes...")
summarydata <- summarydata %>%
  group_by(STARTDATETIME) %>%
  mutate(elapsed_minutes = as.numeric(difftime(DATETIME, STARTDATETIME), units="mins"))


printlog("Number of samples for each core:")
print(summarydata %>% group_by(CORE) %>% summarise(n()) %>% as.data.frame())

summarydata %>%   # save a list of samples and corresponding cores
  select(DATETIME, MPVPosition, CORE) %>% 
  save_data(fname="samplelist.csv")

printlog("Summaries for max_CH4 and max_CO2:")
summary(summarydata$max_CO2)
summary(summarydata$max_CH4)

printlog("Checking for orphan samples...")
orphan_samples <- filter(summarydata, is.na(CORE))
if(nrow(orphan_samples)) {
  printlog("NOTE:", nrow(orphan_samples), "samples have no matching core numbers")
  save_data(orphan_samples)

  printlog("Visualizing orphan samples...")
  p <- ggplot(summarydata, aes(DATETIME, MPVPosition, color=!is.na(CORE)))
  p <- p + geom_jitter() + scale_color_discrete("Has core number")
  p <- p + ggtitle("Orphan samples (no matching date/valve info)")
  print(p)
  save_plot("orphan_samples")
}

printlog("Checking for orphan cores...")
orphan_cores <- setdiff(valvemap$CORE, summarydata$CORE)
if(length(orphan_cores)) {
  printlog("NOTE: cores in the valve map but not in the summary data:")
  print(orphan_cores)
}

# Done! Drop unnecessary columns and save

summarydata <- summarydata %>%
  filter(!is.na(CORE)) %>%
  select(-MPVPosition, -valvemaprow)

save_data(summarydata, scriptfolder=FALSE)

#summarydata <- subset(summarydata, !is.na(CORE), select=-c(MPVPosition, valvemaprow))	
save_data(rawdata_samples, scriptfolder=FALSE, gzip=TRUE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
