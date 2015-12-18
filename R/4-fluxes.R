# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "4-fluxes.R"
SUMMARYDATA   <- file.path(outputdir(scriptfolder = FALSE), "summarydata_clean.csv")
DRYMASS       <- "data/drymasses.csv"

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

summarydata <- read_csv(SUMMARYDATA)
print_dims(summarydata)
drymasses <- read_csv(DRYMASS, skip = 1)
print_dims(drymasses)

printlog("Merging in dry mass data...")
summarydata %>%
  filter(Treatment != "Ambient") %>%
  select(Core, Mass_g, Treatment, Temperature, CO2_ppm_s, CH4_ppb_s, incday) %>%
  left_join(drymasses, by = 'Core') ->
  fluxdata

# TEMPORARY - TODO - set headspace to constant value
# TODO: ask Peyton, what's the "Volume" column in the CoreData.xlsx file?
fluxdata$Headspace_cm <- 5

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
# Headspace on the core is 7.3 cm diameter by variable height
V_headspace <- (7.3 / 2) * pi * fluxdata$Headspace_cm
# Internal volume of Picarro? 
V_picarro <- 9 # Assume same as PP-Systems
fluxdata$V_cm3 <- V_tubing + V_headspace + V_picarro

Pa 			<- 101						# kPa				(Richland is ~120 m asl)
R 			<- 8.3145e+3			# cm3 kPa K−1 mol−1
Tair    <- 273.1 + 20     # unknown

# Calculate mass-corrected respiration, µmol/g soil/s
fluxdata$CO2_flux_µmol_g_s <- 
  with(fluxdata,
       CO2_ppm_s / 1 * # from ppm/s to µmol/s
         V_cm3 / DryMass_g * Pa / (R * Tair)) # ideal gas law
fluxdata$CH4_flux_µmol_g_s <- 
  with(fluxdata,
       CH4_ppb_s / 1000 * # from ppb/s to µmol/s
         V_cm3 / DryMass_g * Pa / (R * Tair)) # ideal gas law

# Calculate flux of mg C/hr
fluxdata$CO2_flux_mgC_hr <- with(fluxdata, CO2_flux_µmol_g_s * DryMass_g) / # get rid of /g soil
  1e6 * # to mol 
  12 *  # to g C
  1000 * # to mg C
  60 * 60 # to /hr
fluxdata$CH4_flux_mgC_hr <- with(fluxdata, CH4_flux_µmol_g_s * DryMass_g) / # get rid of /g soil
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
printlog("Flux diagnostic plots...")
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

printlog("Flux summary plot...")
fluxdata %>%
  melt(id.vars = c("Treatment", "Temperature", "Core", "incday"), 
       measure.vars = c("cumCO2_flux_mgC", "cumCH4_flux_mgC"),
       variable.name = "Gas") %>%
  group_by(Treatment, Temperature, Gas, Core) %>%
  arrange(incday) %>%
  summarise(cum_flux_mgC = last(value)) %>%
  summarise(cum_flux_mgC_sd = sd(cum_flux_mgC),
            cum_flux_mgC = mean(cum_flux_mgC)) ->
  fd_summary

# Tweak the data set - factors, names, etc.
fd_summary$Gas <- substr(fd_summary$Gas, start = 4, stop = 6)
fd_summary$Treatment <- factor(fd_summary$Treatment, levels = c("Drought", "Controlled drought", "Field moisture"))
fd_summary$Temperature <- as.factor(fd_summary$Temperature)

# Add a few dummy rows so graph bars are spaced correctly
dmy <- expand.grid(Treatment = unique(fd_summary$Treatment),
                   Temperature = 4,
                   Gas = unique(fd_summary$Gas),
                   stringsAsFactors = FALSE)
dmy$cum_flux_mgC_sd <- NA
dmy$cum_flux_mgC <- NA
fd_summary <- rbind(fd_summary, dmy)

p3 <- ggplot(fd_summary, aes(Temperature, cum_flux_mgC, fill = Treatment)) + 
  geom_bar(stat='identity', position = position_dodge()) +
  geom_errorbar(aes(color = Treatment, ymin = cum_flux_mgC * .9, ymax = cum_flux_mgC + cum_flux_mgC_sd), 
                position = position_dodge(0.9), width = 0.4) +  
  facet_grid(Gas~., scales = "free") +
  ylab(paste("Cumulative C (mg) over", floor(max(fluxdata$incday)), "days")) +
  ggtitle("Cumulative C by gas, treatment, temperature")
save_diagnostic(p3, "cumulative_gas")


printlog("All done with", SCRIPTNAME)
closelog()
