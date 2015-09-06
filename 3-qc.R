# Process Picarro data for CPCRW incubation
# Quality control
# Ben Bond-Lamberty September 2015

source("0-functions.R")

SCRIPTNAME  	<- "3-qc.R"
SUMMARYDATA      <- file.path(OUTPUT_DIR, "summarydata.csv")  # output from script 2

# save_plot <- function(pname, p=last_plot(), ptype=".pdf", scriptfolder=TRUE, ...)
save_diagnostic <- function(pname, ...) {
  printlog("Saving diagnostic for", pname)
  ggsave(paste0("qc_plots/", pname, ".png"))
  save_plot(pname, ...)
}
  
# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
print_dims(summarydata)

summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)


printlog("Number of samples for each core, by date:")
samples_by_date <- summarydata %>% 
  mutate(Date = strftime(DATETIME, format="%Y-%m-%d")) %>% 
  group_by(Core, Date) %>% 
  summarise(n = n())
save_data(samples_by_date)
p <- qplot(Date, Core, data=samples_by_date, geom="tile", fill=factor(n))
p <- p + ggtitle("Number of reps by date and core") + scale_fill_discrete("Reps")
print(p)
save_diagnostic("samples_by_date")
#save_plot("samples_by_date", ptype = ".png")
#ggsave("qc_plots/samples_by_date.png")

# # At this point we want to compute elapsed_minutes. The zero mark
# # for this calculation is the STARTDATE + STARTTIME fields in the valve map
# summarydata$STARTDATETIME <- ymd_hm(paste(summarydata$STARTDATE, 
#                                           summarydata$STARTTIME), tz="America/Los_Angeles")
# printlog("Computing elapsed minutes...")
# summarydata <- summarydata %>%
#   group_by(STARTDATETIME) %>%
#   mutate(elapsed_minutes = as.numeric(difftime(DATETIME, STARTDATETIME), units="mins"))


print(summarydata %>% group_by(Core) %>% summarise(n()) %>% as.data.frame())

summarydata %>%   # save a list of samples and corresponding cores
  select(DATETIME, MPVPosition, Core) %>% 
  save_data(fname="samplelist.csv")

printlog("Summaries for max_CH4 and max_CO2:")
summary(summarydata$max_CO2)
summary(summarydata$max_CH4)

printlog("Checking for orphan samples...")
orphan_samples <- filter(summarydata, is.na(Core))
if(nrow(orphan_samples)) {
  printlog("NOTE:", nrow(orphan_samples), "samples have no matching core numbers")
  save_data(orphan_samples)
}
printlog("Visualizing orphan samples...")
p <- ggplot(summarydata, aes(DATETIME, MPVPosition, color=!is.na(Core)))
p <- p + geom_jitter() + scale_color_discrete("Has core number")
p <- p + ggtitle("Orphan samples (no matching date/valve info)")
print(p)
#save_plot("orphan_samples")
#ggsave("qc_plots/orphan_samples.png")
save_diagnostic("orphan_samples")

printlog("Computing per-date summaries...")
summarydata <- summarydata %>%
  mutate(CO2_ppm_s = (max_CO2 - min_CO2) / (max_CO2_time - min_CO2_time),
         CH4_ppb_s = (max_CH4 - min_CH4) / (max_CH4_time - min_CH4_time),
         incday = as.numeric(difftime(DATETIME, min(DATETIME), units = "days"))) %>%
  mutate(Date = strftime(DATETIME, format="%Y-%m-%d"))
smry <- summarydata %>%
  group_by(Date, Treatment, Temperature) %>%
  summarise(CO2_ppm_s_sd = sd(CO2_ppm_s), CO2_ppm_s = mean(CO2_ppm_s),
            CH4_ppb_s_sd = sd(CH4_ppb_s), CH4_ppb_s = mean(CH4_ppb_s),
            incday = as.numeric(round(mean(incday), 0)))

p <- qplot(Date, CO2_ppm_s, data=smry) +
  geom_line(aes(group=paste(Temperature, Treatment)), linetype=2) +
  geom_errorbar((aes(ymin=CO2_ppm_s-CO2_ppm_s_sd, ymax=CO2_ppm_s+CO2_ppm_s_sd))) +
  facet_grid(Temperature~Treatment) + 
  ggtitle("CO2 fluxes (ppm/s, uncorrected) by date")
print(p)
#save_plot("CO2_time")
#ggsave("qc_plots/CO2_time.png")
save_diagnostic("CO2_time")

p <- qplot(Date, CH4_ppb_s, data=smry) +
  geom_line(aes(group=paste(Temperature, Treatment)), linetype=2) +
  geom_errorbar((aes(ymin=CH4_ppb_s-CH4_ppb_s_sd, ymax=CH4_ppb_s+CH4_ppb_s_sd))) +
  facet_grid(Temperature~Treatment) + 
  ggtitle("CH4 fluxes (ppb/s, uncorrected) by date")
print(p)
#save_plot("CH4_time")
#ggsave("qc_plots/CH4_time.png")
save_diagnostic("CH4_time")

# Individual cores over time
p <- qplot(incday, CO2_ppm_s, data=summarydata, color=paste(Treatment, Temperature))
p <- p + ggtitle("CO2 fluxes (uncorrected) by rep, core, incubation day") + scale_color_discrete("")
p <- p + facet_wrap(~Core)
print(p)
#save_plot("CO2_incday")
#ggsave("qc_plots/CO2_incday.png")
save_diagnostic("CO2_incday")

p <- qplot(incday, CH4_ppb_s, data=summarydata, color=paste(Treatment, Temperature))
p <- p + ggtitle("CH4 fluxes (uncorrected) by rep, core, incubation day") + scale_color_discrete("")
p <- p + facet_wrap(~Core)
print(p)
#save_plot("CH4_incday")
#ggsave("qc_plots/CH4_incday.png")
save_diagnostic("CH4_incday")

# The README functions as a quick diagnostic summary on the webpage
printlog("Updating README.md document...")
cmd <- paste0("sed -i .bak 's/^Latest data.*$/Latest data: ", max(smry$Date), 
              " (incubation day ", max(smry$incday), ")",
              "/' README.md")
print(cmd)
try(system(cmd))
cmd <- paste0("sed -i .bak 's/^Script run.*$/Script run: ", Sys.time(), "/' README.md")
print(cmd)
try(system(cmd))

# Done! 

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log