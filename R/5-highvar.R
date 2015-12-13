# Process Picarro data for CPCRW incubation
# Investigate treatment summaries with particularly high variability
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "5-highvar.R"
PROBLEM       <- FALSE

SUMMARYDATA  <- file.path(OUTPUT_DIR, "summarydata_clean.csv")  # output from script 3
RAWDATA      <- file.path(OUTPUT_DIR, "rawdata_samples.csv.gz")  # output from script 1
TREATMENT_SUMMARYDATA  <- file.path(OUTPUT_DIR, "treatment_summary.csv")  # output from script 3



investigate <- function(smry, summarydata, rawdata, 
                        gas, gasvar, gasvar_sd = paste0(gasvar, "_sd"), 
                        gasvar_cv = paste0(gasvar, "_cv")) {
  
  smry$ymin <- smry[,gasvar] - smry[,gasvar_sd]
  smry$ymax <- smry[,gasvar] + smry[,gasvar_sd]
  p <- ggplot(smry, aes_string("Date", gasvar, color = gasvar_cv)) + 
    geom_point() +
    facet_grid(Temperature ~ Treatment) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
    scale_color_continuous(guide = FALSE) +
    geom_text(aes(y = ymax, label = Num), vjust = -0.1, hjust = 0.1)
  save_plot(paste0("summary_", gas), p = p)
  
  ok <- TRUE
  while(ok) {
    
    printlog("Looking at gas", gas)
    print(p)
    
    n <- readline("Which do you want to investigate (0 to exit)? ") %>% as.numeric()
    
    if(n %in% seq_len(nrow(smry))) {
      printlog(smry[n,])
      
      samplenums <- smry[n, "samplenums"] %>% strsplit(" ") %>% unlist %>% as.numeric()
      
      printlog("Samples from the summary dataset:")
      sd_gas <- subset(summarydata, samplenum %in% samplenums)
      print(sd_gas)
      
      p_closeup <- ggplot(sd_gas, aes_string("DATETIME", gasvar, fill="Core")) + 
        geom_bar(stat='identity') + 
        ggtitle(paste("Closeup look at number", n, "on incday", unique(sd_gas$incday))) +
        geom_text(aes(label = samplenum), vjust = -0.5)
      print(p_closeup)
      save_plot(paste0(gas, "_closeup_", n))
      
      p_distribution <- ggplot(subset(summarydata, Treatment %in% sd_gas$Treatment & Temperature %in% sd_gas$Temperature), aes_string(x = gasvar)) + 
        geom_density() + 
        facet_grid(Temperature ~ Treatment, scales = "free") + 
        geom_vline(data=sd_gas, aes_string(xintercept = gasvar, color="Core"), linetype = 2) +
        ggtitle(paste("Distribution look at number", n))
      print(p_distribution)
      save_plot(paste0(gas, "_distribution_", n))
      
      
      rd_gas <- subset(rawdata, samplenum %in% samplenums)
      p_rawcloseup <- ggplot(rd_gas, aes_string("elapsed_seconds", paste0(gas, "_dry"), color="samplenum")) +
        geom_line() +
        ggtitle(paste("Raw closeup look at number", n, "on", unique(rd_gas$DATE)))
      print(p_rawcloseup)
      save_plot(paste0(gas, "_closeup_raw_", n))
      
    } else {
      ok <- FALSE
    }
  } # while
  
}

# Main ===========================================================================

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
#rawdata <- read_csv(RAWDATA)
printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)
printlog("Reading in treatment summaries...")
tsummary <- read_csv(TREATMENT_SUMMARYDATA)
tsummary$Date <- ymd(tsummary$Date)

# ------------------- CO2

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
smry_co2$CO2_ppm_s_cv <- with(smry_co2, CO2_ppm_s_sd / CO2_ppm_s)
investigate(smry = smry_co2, summarydata = summarydata, rawdata = rawdata, 
            gas = "CO2", gasvar = "CO2_ppm_s")

# ------------------- CH4

printlog("Looking at high-variability CH4...")
tsummary %>%
  filter(!is.na(CH4_ppb_s_sd), Treatment != "Ambient") %>% 
  arrange(desc(CH4_ppb_s_sd)) %>%
  select(Date, Treatment, Temperature, CH4_ppb_s_sd, CH4_ppb_s, incday, samplenums) ->
  smry_ch4

save_data(smry_ch4)

cv_cutoff <- 0.9
smry_ch4$Num <- 1:nrow(smry_ch4)
smry_ch4$Num[smry_ch4$CH4_ppb_s_sd / smry_ch4$CH4_ppb_s < cv_cutoff] <- NA
smry_ch4$Num[1:10] <- 1:10  # largest 10 always get labeled
smry_ch4$CH4_ppb_s_cv <- with(smry_ch4, CH4_ppb_s_sd / CH4_ppb_s)
investigate(smry = smry_ch4, summarydata = summarydata, rawdata = rawdata, 
            gas = "CH4", gasvar = "CH4_ppb_s")

# Done! 

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
closelog()

if(PROBLEM) warning("There was a problem - see log")

