# NOTE THIS DOESN'T WORK WITH CURRENT DATASET YET

# Computes fluxes. In progress.
# Ben Bond-Lamberty April 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "4-fluxes.R"
summarydata      <- file.path(OUTPUT_DIR, "summarydata.csv")  # output from script 2


# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
fluxdata <- read_csv(summarydata)
print_dims(fluxdata)

printlog("Computing fluxes...")
# At this point, `fluxdata` has min and max CO2/CH4 concentrations, 
# and the times those occured at. Computing a slope (ppm or ppb/s) is thus easy.
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
# Headspace on the core is 7.3 cm diameter by 4 cm height.
V_headspace <- fluxdata$HEADSPACE_VOL_CM3
# Internal volume of Picarro? 
V_picarro <- 9 # Assume same as PP-Systems
V_cm3 <- V_tubing + V_headspace + V_picarro

Pa 			<- 101						# kPa				(Richland is ~120 m asl)
R 			<- 8.3145e+3			# cm3 kPa K−1 mol−1
Tair    <- 273.1 + 20     # unknown

m_CO2 <- with(fluxdata, (max_CO2 - min_CO2) / (max_CO2_time - min_CO2_time))  # ppm/s
m_CH4 <- with(fluxdata, (max_CH4 - min_CH4) / (max_CH4_time - min_CH4_time))  # ppb/s
fluxdata$V_cm3 <- V_cm3

# Calculate mass-corrected respiration, µmol/g soil/s
fluxdata$CO2_flux_umol_g_s <- m_CO2 / 1 * # from ppm/s to µmol/s
  V_cm3 / fluxdata$DRYWT_SOIL_G * Pa / (R * Tair) # ideal gas law
fluxdata$CH4_flux_umol_g_s <- m_CH4 / 1000 * # from ppb/s to µmol/s
  V_cm3 / fluxdata$DRYWT_SOIL_G * Pa / (R * Tair) # ideal gas law

# Calculate total flux of mg C/s
fluxdata$CO2_flux_mgC_hr <- with(fluxdata, CO2_flux_umol_g_s * DRYWT_SOIL_G) / # get rid of /g soil
  1e6 * # to mol 
  12 *  # to g C
  1000 * # to mg C
  60 * 60 # to /hr
fluxdata$CH4_flux_mgC_hr <- with(fluxdata, CH4_flux_umol_g_s * DRYWT_SOIL_G) / # get rid of /g soil
  1e6 * # to mol 
  16 *  # to g C
  1000 *  # to mg C
  60 * 60 # to /hr

# Compute cumulative C respired
printlog("Computing cumulative C respired...")
fd_notcum <- filter(fluxdata, elapsed_minutes < 0.0)
fluxdata <- fluxdata %>%
  filter(elapsed_minutes >= 0.0) %>%# & !is.na(CORE)) %>%
  group_by(CORE, WETTING, MOISTURE, STRUCTURE) %>%
  arrange(elapsed_minutes) %>%
  mutate(CO2_flux_mgC = CO2_flux_mgC_hr * (elapsed_minutes - lag(elapsed_minutes)) / 60,
         cumCO2_flux_mgC = c(0, cumsum(CO2_flux_mgC[-1])),
         CH4_flux_mgC = CH4_flux_mgC_hr * (elapsed_minutes - lag(elapsed_minutes)) / 60,
         cumCH4_flux_mgC = c(0, cumsum(CH4_flux_mgC[-1]))
  ) %>%
  bind_rows(fd_notcum) %>%
  select(-CO2_flux_mgC, -CH4_flux_mgC, -STARTDATETIME) %>%
  arrange(STARTDATE, CORE, WETTING, MOISTURE, STRUCTURE, elapsed_minutes)

#fluxdata <- fluxdata[complete.cases(fluxdata),]

save_data(fluxdata, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
closelog()
