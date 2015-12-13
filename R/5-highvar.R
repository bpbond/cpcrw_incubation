# Process Picarro data for CPCRW incubation
# Investigate treatment summaries with particularly high variability
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "5-highvar.R"
PROBLEM       <- FALSE

SUMMARYDATA  <- file.path(OUTPUT_DIR, "summarydata_clean.csv")  # output from script 3
RAWDATA      <- file.path(OUTPUT_DIR, "rawdata_samples.csv.gz")  # output from script 1
TREATMENT_SUMMARYDATA  <- file.path(OUTPUT_DIR, "treatment_summary.csv")  # output from script 3


# Main ===========================================================================

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)
printlog("Reading in treatment summaries...")
tsummary <- read_csv(TREATMENT_SUMMARYDATA)
tsummary$Date <- ymd(tsummary$Date)

printlog("Looking at high-variability CO2...")
tsummary %>%
  filter(!is.na(CO2_ppm_s_sd), Treatment != "Ambient") %>% 
  arrange(desc(CO2_ppm_s_sd)) %>%
  select(Date, Treatment, Temperature, CO2_ppm_s_sd, CO2_ppm_s, incday, samplenums) ->
  smry_co2

save_data(smry_co2)

cv_cutoff <- 0.9
smry_co2$Num <- 1:nrow(smry_co2)
smry_co2$Num[smry_co2$CO2_ppm_s_sd / smry_co2$CO2_ppm_s < cv_cutoff] <- NA
smry_co2$Num[1:10] <- 1:10  # largest 10 always get labeled

p <- ggplot(smry_co2, aes(Date, CO2_ppm_s, color = CO2_ppm_s_sd/CO2_ppm_s)) + geom_point() +
  facet_grid(Temperature ~ Treatment) + 
  geom_errorbar(aes(ymin = CO2_ppm_s - CO2_ppm_s_sd, ymax = CO2_ppm_s + CO2_ppm_s_sd)) + 
  scale_color_continuous(guide = FALSE) +
  geom_text(aes(y = CO2_ppm_s + CO2_ppm_s_sd, label = Num), vjust = -0.1, hjust = 0.1)
save_plot("summary", p = p)

ok <- TRUE
while(ok) {
  
  print(p)
  
  n <- readline("Which do you want to investigate (0 to exit)? ") %>% as.numeric()
  
  if(n %in% seq_len(nrow(smry_co2))) {
    printlog(smry_co2[n,])
    
    samplenums <- smry_co2[n, "samplenums"] %>% strsplit(" ") %>% unlist %>% as.numeric()
    
    printlog("Samples from the summary dataset:")
    sd_co2 <- subset(summarydata, samplenum %in% samplenums)
    print(sd_co2)
    
    p_closeup <- ggplot(sd_co2, aes(paste(Treatment, Temperature), CO2_ppm_s)) + 
      geom_violin() + 
      geom_text(aes(label = samplenum), position="jitter") + 
      ggtitle(paste("Closeup look at number", n, "on incday", unique(sd_co2$incday)))
    print(p_closeup)
    save_plot(paste0("closeup_", n))
    
    rd_co2 <- subset(rawdata, samplenum %in% samplenums)
    p_rawcloseup <- ggplot(rd_co2, aes(elapsed_seconds, CO2_dry, color=samplenum)) +
      geom_line() +
      ggtitle(paste("Raw closeup look at number", n, "on", unique(rd_co2$DATE)))
    print(p_rawcloseup)
    save_plot(paste0("closeup_raw_", n))
  } else {
    ok <- FALSE
  }
} # while

# Done! 

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
closelog()

if(PROBLEM) warning("There was a problem - see log")

