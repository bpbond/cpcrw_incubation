# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "4-fluxes.R"
PROBLEM       <- FALSE

library(reshape2)      # 1.4.1

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

summarydata <- read_csv(SUMMARYDATA_CLEAN_FILE)
print_dims(summarydata)

drymassdata <- read_csv(COREDATA_FILE, skip = 1)
summarydata <- left_join(summarydata, drymassdata, by = "Core")

# -----------------------------------------------------------------------------
# Create fluxdata and compute water content

printlog("Computing water contents...")
summarydata %>%
  filter(Treatment != "Ambient") %>%
  mutate(WC_gravimetric = 
           (Mass_g - CoreSleeveMass_g - NonsoilMassLarge_g - SoilDryMass_g) / 
           SoilDryMass_g,
         WC_volumetric = (Mass_g - CoreSleeveMass_g - NonsoilMassLarge_g - SoilDryMass_g) / 
           SoilVolume_cm3) %>%
  select(Core, Mass_g, SoilDryMass_g, SoilVolume_cm3, 
         WC_gravimetric, WC_volumetric,
         Treatment, Temperature, CO2_ppm_s, CH4_ppb_s, inctime_days) ->
  fluxdata

print(summary(fluxdata$WC_gravimetric))
print(summary(fluxdata$WC_volumetric))

p <- ggplot(fluxdata, aes(inctime_days, WC_gravimetric, color = paste(Treatment, Temperature), group=Core))
p <- p + geom_line() + facet_wrap(~Core) + scale_color_discrete("")
print(p)
save_plot("WC_gravimetric")

printlog("WC_gravimetric summary by core:")
fluxdata %>% 
  group_by(Core) %>% 
  summarise_each(funs(min, mean, max), WC_gravimetric) %>% 
  arrange(mean) %>%
  as.data.frame %>%
  print

# -----------------------------------------------------------------------------
# Flux computation

# At this point, `fluxdata` has slopes (CO2 ppm/s and CH4 ppb/s).
# We want to convert this to mg C/s, using
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

# Calculate mass-corrected respiration, µmol/s
CO2_flux_µmol_g_s <- 
  with(fluxdata,
       CO2_ppm_s / 1 * # from ppm/s to µmol/s
         V_cm3 * Pa / (R * Tair)) # ideal gas law
CH4_flux_µmol_g_s <- 
  with(fluxdata,
       CH4_ppb_s / 1000 * # from ppb/s to µmol/s
         V_cm3 * Pa / (R * Tair)) # ideal gas law

# Calculate flux of mg C/hr
fluxdata$CO2_flux_mgC_hr <- CO2_flux_µmol_g_s /
  1e6 * # to mol 
  12 *  # to g C
  1000 * # to mg C
  60 * 60 # to /hr
fluxdata$CH4_flux_mgC_hr <- CH4_flux_µmol_g_s /
  1e6 * # to mol 
  16 *  # to g C
  1000 *  # to mg C
  60 * 60 # to /hr

# -----------------------------------------------------------------------------
# Outlier identification - see 
# For each gas, group by Treatment, Temperature, and incubation day
printlog("Identifying and plotting outliers...")
fluxdata %>%
  arrange(inctime_days) %>%
  ungroup %>%
  mutate(group = cut(1:nrow(fluxdata), OUTLIER_GROUPS)) %>%
  group_by(Treatment, Temperature, group) %>% 
         # Identify outliers by flux/mass, i.e. normalizing for mass and water
  mutate(CO2_outlier = is_outlier(CO2_flux_mgC_hr / Mass_g, devs = CO2_EXCLUDE_DEVS), 
         # CH4 is so variable we use a higher exclusion cutoff
         CH4_outlier = is_outlier(CH4_flux_mgC_hr / Mass_g, devs = CH4_EXCLUDE_DEVS)) %>%
  select(-group) ->
  fluxdata

fluxdata$incday <- NULL  # why doesn't the `select` above work?

p <- ggplot(fluxdata, aes(inctime_days, CO2_flux_mgC_hr, color = CO2_outlier))
p <- p + geom_point() + facet_grid(Temperature ~ Treatment)
print(p)
#save_plot("CO2_outliers")
save_diagnostic(p, "CO2_outliers")
p <- ggplot(fluxdata, aes(inctime_days, CH4_flux_mgC_hr, color = CH4_outlier))
p <- p + geom_point() + facet_grid(Temperature ~ Treatment)
print(p)
save_diagnostic(p, "CH4_outliers")

save_data(fluxdata, fn = FLUXDATA_FILE, scriptfolder = FALSE)


# -----------------------------------------------------------------------------
# Compute cumulative C respired

printlog("Computing cumulative C respired...")

# Create a 'clean' column with no flux rates flagged as outliers
fluxdata$CO2_flux_mgC_hr_clean <- fluxdata$CO2_flux_mgC_hr
fluxdata$CO2_flux_mgC_hr_clean[fluxdata$CO2_outlier] <- NA
fluxdata$CH4_flux_mgC_hr_clean <- fluxdata$CH4_flux_mgC_hr
fluxdata$CH4_flux_mgC_hr_clean[fluxdata$CH4_outlier] <- NA

# Interpolate the missing (b/c outlier) flux rates, then compute cumulative emissions
fluxdata %>%
  group_by(Core) %>%
  arrange(inctime_days) %>%
  mutate(CO2_flux_mgC_hr_interp = approx(inctime_days, CO2_flux_mgC_hr_clean, xout = inctime_days, rule = 2)$y,
         CH4_flux_mgC_hr_interp = approx(inctime_days, CH4_flux_mgC_hr_clean, xout = inctime_days, rule = 2)$y) %>%
  group_by(Core) %>%
  mutate(delta_hrs = (inctime_days - lag(inctime_days)) * 24,
         CO2_flux_mgC = CO2_flux_mgC_hr_interp * delta_hrs,
         cumCO2_flux_mgC = c(0, cumsum(CO2_flux_mgC[-1])),
         CH4_flux_mgC = CH4_flux_mgC_hr_interp * delta_hrs,
         cumCH4_flux_mgC = c(0, cumsum(CH4_flux_mgC[-1]))) %>%
  select(-CO2_flux_mgC_hr_interp, -CH4_flux_mgC_hr_interp,
         -CO2_flux_mgC_hr_clean, -CH4_flux_mgC_hr_clean) ->
  fluxdata


# -----------------------------------------------------------------------------
# A few diagnostic plots

printlog("Cumulative flux diagnostic plots...")
p1 <- ggplot(fluxdata, aes(inctime_days, cumCO2_flux_mgC, group = Core)) + 
  geom_line() + 
  facet_grid(Temperature ~ Treatment) +
  ggtitle("Cumulative CO2 by core and treatment")
print(p1)
save_plot("cumulative_CO2")

p2 <- ggplot(fluxdata, aes(inctime_days, cumCH4_flux_mgC, group = Core)) + 
  geom_line() + 
  facet_grid(Temperature ~ Treatment) + 
  ggtitle("Cumulative CH4 by core and treatment")
print(p2)
save_plot("cumulative_CH4")

# -----------------------------------------------------------------------------
# Cumulative fluxes

printlog("Calculating flux summary datasets...")
fluxdata %>%
  melt(id.vars = c("Treatment", "Temperature", "Core", "inctime_days"), 
       measure.vars = c("cumCO2_flux_mgC", "cumCH4_flux_mgC"),
       variable.name = "Gas") %>%
  mutate(Gas = substr(Gas, start = 4, stop = 6)) %>%   # rename
  group_by(Treatment, Temperature, Gas, Core) %>%
  arrange(inctime_days) %>%
  summarise(cum_flux_mgC = last(value)) -> 
  fd_cumulative_core

save_data(fd_cumulative_core, fn = FLUXDATA_CUM_CORE_FILE, scriptfolder = FALSE)

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
dmy <- data.frame(Treatment = "Controlled drought",
                  Temperature = 4,
                  Gas = unique(fd_cumulative$Gas),
                  cum_flux_mgC = NA,
                  cum_flux_mgC_sd = NA,
                  stringsAsFactors = FALSE)
fluxdata_cumulative <- rbind(fd_cumulative, dmy)

p3 <- ggplot(fluxdata_cumulative, aes(Temperature, cum_flux_mgC, fill = Treatment)) + 
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(color = Treatment, 
                    ymin = cum_flux_mgC * 0.9, 
                    ymax = cum_flux_mgC + cum_flux_mgC_sd), 
                position = position_dodge(0.9), width = 0.4) +  
  facet_grid(Gas ~ ., scales = "free") +
  ylab(paste("Cumulative C (mg) over", floor(max(fluxdata$inctime_days)), "days")) +
  ggtitle("Cumulative C by gas, treatment, temperature")
save_diagnostic(p3, "cumulative_gas")
save_data(fluxdata_cumulative, fn = FLUXDATA_CUM_FILE, scriptfolder = FALSE)

# -----------------------------------------------------------------------------
# Done!

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
