# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "4-fluxes.R"
PROBLEM       <- FALSE

SUMMARYDATA   <- file.path(outputdir(scriptfolder = FALSE), "summarydata_clean.csv")
DRYMASSDATA   <- "data/drymasses.csv"

library(reshape2)      # 1.4.1

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

summarydata <- read_csv(SUMMARYDATA)
print_dims(summarydata)

drymassdata <- read_csv(DRYMASSDATA, skip = 1)
summarydata <- left_join(summarydata, drymassdata, by = "Core")

# Create fluxdata and compute water content
summarydata %>%
  filter(Treatment != "Ambient") %>%
  mutate(WC_gravimetric = (Mass_g - SoilDryMass_g) / SoilDryMass_g) %>%
  select(Core, SoilDryMass_g, SoilVolume_cm3, WC_gravimetric,
         Treatment, Temperature, CO2_ppm_s, CH4_ppb_s, incday) ->
  fluxdata

# At this point, `fluxdata` has slopes (CO2 ppm/s and CH4 ppb/s).
# We want to convert this to mg C/g soil/s, using
# A = dC/dt * V/M * Pa/RT (cf. Steduto et al. 2002), where
# 	A is the flux (µmol/g/s)
#	  dC/dt is raw respiration as above (mole fraction/s)
# 	V is total chamber volume (cm3)
#	  M is [dry] soil mass (g)
#	  Pa is atmospheric pressure (kPa)
#	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
#	  T is air temperature (K)

# The instrument tubing is 455 cm long by ID 1/16"
V_tubing <- (1/16 * 2.54 / 2 ) ^ 2 * pi * 455
# Headspace is in each is the total volume of the sleeve minus the soil volume
V_headspace <- (7.5 / 2) ^ 2 * pi * 30 - fluxdata$SoilVolume_cm3
# Internal volume of Picarro? 
V_picarro <- 9 # Assume same as PP-Systems

fluxdata$V_cm3 <- V_tubing + V_headspace + V_picarro

Pa 			<- 101						# kPa				(Richland is ~120 m asl)
R 			<- 8.3145e+3			# cm3 kPa K−1 mol−1
Tair    <- 273.1 + fluxdata$Temperature     # C -> K

# Calculate mass-corrected respiration, µmol/g soil/s
fluxdata$CO2_flux_µmol_g_s <- 
  with(fluxdata,
       CO2_ppm_s / 1 * # from ppm/s to µmol/s
         V_cm3 / SoilDryMass_g * Pa / (R * Tair)) # ideal gas law
fluxdata$CH4_flux_µmol_g_s <- 
  with(fluxdata,
       CH4_ppb_s / 1000 * # from ppb/s to µmol/s
         V_cm3 / SoilDryMass_g * Pa / (R * Tair)) # ideal gas law

# Calculate flux of mg C/hr
fluxdata$CO2_flux_mgC_hr <- with(fluxdata, CO2_flux_µmol_g_s * SoilDryMass_g) / # get rid of /g soil
  1e6 * # to mol 
  12 *  # to g C
  1000 * # to mg C
  60 * 60 # to /hr
fluxdata$CH4_flux_mgC_hr <- with(fluxdata, CH4_flux_µmol_g_s * SoilDryMass_g) / # get rid of /g soil
  1e6 * # to mol 
  16 *  # to g C
  1000 *  # to mg C
  60 * 60 # to /hr

# Compute cumulative C respired
printlog("Computing cumulative C respired...")
fluxdata %>%
  group_by(Core) %>%
  arrange(incday) %>%
  mutate(delta_hrs = (incday - lag(incday)) * 24,
         CO2_flux_mgC = CO2_flux_mgC_hr * delta_hrs,
         cumCO2_flux_mgC = c(0, cumsum(CO2_flux_mgC[-1])),
         CH4_flux_mgC = CH4_flux_mgC_hr * delta_hrs,
         cumCH4_flux_mgC = c(0, cumsum(CH4_flux_mgC[-1]))) %>%
  select(-CO2_flux_mgC, -CH4_flux_mgC) ->
  fluxdata

save_data(fluxdata, scriptfolder = FALSE)

# A few diagnostic plots
printlog("Cumulative flux diagnostic plots...")
p1 <- ggplot(fluxdata, aes(incday, cumCO2_flux_mgC, group = Core)) + 
  geom_line() + 
  facet_grid(Temperature ~ Treatment) +
  ggtitle("Cumulative CO2 by core and treatment")
print(p1)
save_plot("cumulative_CO2")

p2 <- ggplot(fluxdata, aes(incday, cumCH4_flux_mgC, group = Core)) + 
  geom_line() + 
  facet_grid(Temperature ~ Treatment) + 
  ggtitle("Cumulative CH4 by core and treatment")
print(p2)
save_plot("cumulative_CH4")

printlog("Calculating flux summary datasets...")
fluxdata %>%
  melt(id.vars = c("Treatment", "Temperature", "Core", "incday"), 
       measure.vars = c("cumCO2_flux_mgC", "cumCH4_flux_mgC"),
       variable.name = "Gas") %>%
  mutate(Gas = substr(Gas, start = 4, stop = 6)) %>%   # rename
  group_by(Treatment, Temperature, Gas, Core) %>%
  arrange(incday) %>%
  summarise(cum_flux_mgC = last(value)) -> 
  fd_cumulative_core

save_data(fd_cumulative_core, scriptfolder = FALSE)

fd_cumulative_core %>%   # already grouped by Treatment, Temperature, Gas
  summarise(cum_flux_mgC_sd = sd(cum_flux_mgC),
            cum_flux_mgC = mean(cum_flux_mgC)) ->
  fd_cumulative


printlog("Flux summary plot...")

# Tweak the data set - factors, names, etc.
fd_cumulative$Treatment <- factor(fd_cumulative$Treatment, 
                                  levels = c("Drought", "Controlled drought", "Field moisture"))
fd_cumulative$Temperature <- as.factor(fd_cumulative$Temperature)

# Add a few dummy rows so graph bars are spaced correctly
dmy <- expand.grid(Treatment = unique(fd_cumulative$Treatment),
                   Temperature = 4,
                   Gas = unique(fd_cumulative$Gas),
                   stringsAsFactors = FALSE)
dmy$cum_flux_mgC_sd <- NA
dmy$cum_flux_mgC <- NA
fd_cumulative <- rbind(fd_cumulative, dmy)

p3 <- ggplot(fd_cumulative, aes(Temperature, cum_flux_mgC, fill = Treatment)) + 
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(color = Treatment, 
                    ymin = cum_flux_mgC * 0.9, 
                    ymax = cum_flux_mgC + cum_flux_mgC_sd), 
                position = position_dodge(0.9), width = 0.4) +  
  facet_grid(Gas ~ ., scales = "free") +
  ylab(paste("Cumulative C (mg) over", floor(max(fluxdata$incday)), "days")) +
  ggtitle("Cumulative C by gas, treatment, temperature")
save_diagnostic(p3, "cumulative_gas")


printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
