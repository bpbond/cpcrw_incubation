# Final analysis script: statistical tests, final plots, etc.
# Called by the R Markdown document (the manuscript).
# Ben Bond-Lamberty January 2016

source("R/0-functions.R")

SCRIPTNAME  	<- "5-analyze.R"

library(nlme)          # 3.1-122
#library(MASS)          # 7.3-45

library(broom)  # 0.4.0
library(reshape2) # 1.4.1

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

coredata <- read_csv(COREDATA_FILE, skip = 1)

fluxdata_orig <- read_csv(FLUXDATA_FILE)
fluxdata_orig$Treatment <- factor(fluxdata_orig$Treatment, 
                                  levels = c("Field moisture", "Controlled drought", "Drought"))

printlog("Transforming...")
fluxdata_orig %>%
  select(samplenum, Core, Treatment, Temperature, inctime_days, 
         Mass_g, SoilDryMass_g, WC_gravimetric, WC_volumetric,
         CO2_flux_mgC_hr, CH4_flux_mgC_hr,
         CO2_outlier, CH4_outlier) %>%
  melt(measure.vars = c("CO2_flux_mgC_hr", "CH4_flux_mgC_hr"),
       value.name = "flux_mgC_hr") %>%
  mutate(Gas = substr(variable, 1, 3)) %>%
  select(-variable) ->
  fluxdata

# Combine the gas-specific outlier fields into a single outlier flag
# I originally was using reshape2::melt but this was duplicating rows
fluxdata$outlier <- fluxdata$CO2_outlier
fluxdata$outlier[fluxdata$Gas == "CH4"] <- fluxdata$CH4_outlier[fluxdata$Gas == "CH4"]
fluxdata$CO2_outlier <- fluxdata$CH4_outlier <- NULL

# -----------------------------------------------------------------------------
# Read C, N, DOC data; summarise; test for treatment effects; merge with fluxdata

printlog(SEPARATOR)
printlog("Reading", CNDATA_FILE)
read_csv(CNDATA_FILE) %>%
  select(DOC_mg_kg,	C_percent, N_percent, Core) %>%
  group_by(Core) %>%
  # Summarise by core, averaging duplicates
  summarise_each(funs(mean(., na.rm = TRUE)), DOC_mg_kg, C_percent, N_percent) ->
  cndata_orig

# Compute correlations between the variables
cndata_cor <- cor(cndata_orig[-1])

cndata_orig %>%
  left_join(read_csv(TREATMENTS, skip = 1), by = "Core") ->
  cndata

cndata %>%
  summarise_each(funs(mean, sd), DOC_mg_kg, C_percent, N_percent) %>%
  print %>%
  unlist(.[1,]) ->
  cndata_summary

lm(formula = DOC_mg_kg ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print
lm(formula = C_percent ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print
lm(formula = N_percent ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print

fluxdata %>%
  left_join(cndata_orig, by = "Core") ->
  fluxdata


# -----------------------------------------------------------------------------
# Compute normalized fluxes

printlog(SEPARATOR)
printlog("Computing normalized fluxes...")

fluxdata %>%
  mutate(flux_µgC_g_day = flux_mgC_hr / SoilDryMass_g * 1000 * 24,
         flux_µgC_gC_day = flux_µgC_g_day * C_percent) ->
  fluxdata

fluxdata %>%
  group_by(Gas) %>%
  summarise_each(funs(min, max, mean, sd), flux_µgC_g_day, flux_µgC_gC_day) ->
  fluxsummary

save_data(fluxdata, fn = "fluxdata_long.csv")

nms <- fluxsummary$Gas
fluxsummary <- as.data.frame(t(fluxsummary[-1]))
colnames(fluxsummary) <- nms

save_data(fluxsummary)

# -----------------------------------------------------------------------------
# Water content over time figure and data

printlog(SEPARATOR)
printlog("Water content over time...")

figureA <- ggplot(fluxdata_orig, aes(inctime_days, WC_gravimetric, color=Treatment, group=Core)) 
figureA <- figureA + geom_point() + geom_line()
figureA <- figureA + facet_grid(~Temperature) 
figureA <- figureA + xlab("Incubation day") + ylab("Gravimetric water content (fraction dry mass)")

fluxdata$incday <- floor(fluxdata$inctime_days)
fluxdata$stage <- NA
fluxdata$stage[fluxdata$incday == min(fluxdata$incday)] <- "Beginning"
fluxdata$stage[fluxdata$incday == max(fluxdata$incday)] <- "End"
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage, Treatment) %>%
  summarise_each(funs(mean, sd, max, min), WC_gravimetric) ->
  gwc
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage) %>% 
  summarise_each(funs(mean, sd, max, min), WC_gravimetric) ->
  gwc_alltrt
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage, Treatment) %>% 
  summarise_each(funs(mean, sd, max, min), WC_volumetric) ->
  vwc
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage) %>% 
  summarise_each(funs(mean, sd, max, min), WC_volumetric) ->
  vwc_alltrt


# -----------------------------------------------------------------------------
# Did 'Treatment' affect water content?

printlog(SEPARATOR)
printlog("Treatment effect on water content...")
lowtemp <- min(fluxdata$Temperature, na.rm = TRUE)
hightemp <- max(fluxdata$Temperature, na.rm = TRUE)
fluxdata %>%
  filter(Temperature == hightemp) ->
  fluxdata20
fluxdata20$Treatment <- factor(fluxdata20$Treatment, levels = c("Drought", "Controlled drought", "Field moisture"))
m_wc <- lme(WC_gravimetric ~ Treatment,
            data = fluxdata20, 
            random = ~ 1 | Core)
print(summary(m_wc))
p_trt_wc <- summary(m_wc)$tTable["TreatmentControlled drought", "p-value"]

# No significant difference between Drought and Controlled drought in the 20C chamber

# -----------------------------------------------------------------------------
# Fluxes over time figure

printlog(SEPARATOR)
printlog("Fluxes over time...")
fluxdata %>%
  filter(!outlier) %>%
  mutate(incday = round(inctime_days, 0)) %>%
  group_by(Gas, Temperature, Treatment, incday) %>%
  summarise(flux = mean(flux_µgC_g_day), 
            flux_sd = sd(flux_µgC_g_day),
            n = n(),
            samplenums = paste(samplenum, collapse = " ")) ->
  fluxdata_figsBC

figsBC <- function(fd) {
    ggplot(fd, aes(incday, flux)) + 
    geom_point() + 
    facet_grid(Temperature ~ Treatment) + 
    geom_errorbar(aes(ymin = flux - flux_sd, ymax = flux + flux_sd)) +
    xlab("Incubation day") + ylab(expression(µg~C~g~C^{-1}~day^{-1}))
}

fluxdata_figsBC %>%
  filter(Gas == "CO2") %>%
  figsBC ->
  figureB
fluxdata_figsBC %>%
  filter(Gas == "CH4") %>%
  figsBC ->
  figureC

# -----------------------------------------------------------------------------
# Cumulative flux figure and Tukey HSD tests

printlog(SEPARATOR)
printlog("Running Tukey HSD tests on cumulative emissions...")
fd_cumulative_core <- read_csv(FLUXDATA_CUM_CORE_FILE) %>%
  mutate(Temperature = as.factor(Temperature))
co2_aov <- aov(cum_flux_mgC ~ Treatment + Temperature, 
               data = subset(fd_cumulative_core, Gas == "CO2"))
co2_hsd <- TukeyHSD(co2_aov)
print(co2_hsd)
ch4_aov <- aov(cum_flux_mgC ~ Treatment + Temperature, 
               data = subset(fd_cumulative_core, Gas == "CH4"))
ch4_hsd <- TukeyHSD(ch4_aov)
print(ch4_hsd)

# Convert the significance data to data frame, for easier (and clearer)
# access later by the manuscript code
co2_hsd$Temperature <- as.data.frame(co2_hsd$Temperature)
co2_hsd$Treatment <- as.data.frame(co2_hsd$Treatment)
ch4_hsd$Temperature <- as.data.frame(ch4_hsd$Temperature)
ch4_hsd$Treatment <- as.data.frame(ch4_hsd$Treatment)

printlog("Making plot...")
fluxdata_cumulative <- read_csv(FLUXDATA_CUM_FILE)
fluxdata_cumulative$Treatment <- factor(fluxdata_cumulative$Treatment, 
                                        levels = c("Field moisture", "Controlled drought", "Drought"))
fluxdata_cumulative$Temperature <- as.factor(fluxdata_cumulative$Temperature)

figureD <- ggplot(fluxdata_cumulative, aes(Temperature, cum_flux_mgC, fill = Treatment)) + 
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(color = Treatment, 
                    ymin = cum_flux_mgC * 0.9, 
                    ymax = cum_flux_mgC + cum_flux_mgC_sd), 
                position = position_dodge(0.9), width = 0.4) +  
  facet_grid(Gas ~ ., scales = "free") +
  ylab(paste("Cumulative C (mg) over", floor(max(fluxdata$inctime_days)), "days"))

# -----------------------------------------------------------------------------
# Q10 based on cumulative fluxes
# TODO: I hate the hardcoded temperature values below!

stopifnot(length(unique(fluxdata_cumulative$Temperature)) == 2)
fluxdata_cumulative %>%
  reshape2::dcast(Treatment + Gas ~ Temperature, 
                  value.var = "cum_flux_mgC") %>%
  mutate(Q10 = (`20` / `4`) ^ (10 / (hightemp - lowtemp))) %>%
  reshape2::dcast(Treatment ~ Gas, value.var = "Q10") ->  
  q10_cumulative
rownames(q10_cumulative) <- q10_cumulative$Treatment

# -----------------------------------------------------------------------------
# CO2:CH4 emissions ratio

printlog(SEPARATOR)
printlog("Computing emissions ratios...")
fluxdata_cumulative %>% 
  group_by(Treatment, Temperature) %>% 
  summarise(CO2CH4_ratio = max(cum_flux_mgC) / min(cum_flux_mgC)) %>%
  na.omit %>%
  print ->
  gas_ratio

# -----------------------------------------------------------------------------
# Examine data distribution
# Log-transforming doesn't fix the lack of normality in our data,
# but it's a major improvement; see graphs.

# Remove outliers and zero fluxes from the data set
fluxdata %>% 
  filter(flux_µgC_gC_day > 0 & !outlier) ->
  fluxdata

printlog("Graphing flux distributions and testing for normality...")
p <- ggplot(fluxdata, aes(x = flux_µgC_gC_day)) + geom_histogram(bins = 30)
p <- p + facet_grid(~Gas, scales = "free") + ggtitle("Distribution of raw data")
print(p)
save_plot("distribution_raw")

p <- ggplot(fluxdata, aes(x = log(flux_µgC_gC_day))) + geom_histogram(bins = 30)
p <- p + facet_grid(~Gas, scales = "free") + ggtitle("Distribution of log-transformed data")
print(p)
save_plot("distribution_log")

printlog("Shapiro-Wilk normality test on raw data:")
fluxdata %>%
  group_by(Gas) %>%
  do(norm = shapiro.test(.$flux_µgC_gC_day)) %>%
  tidy(norm) %>%
  print ->
  shapiro
save_data(shapiro)

printlog("Shapiro-Wilk normality test on log-transformed data:")
fluxdata %>%
  group_by(Gas) %>%
  do(norm = shapiro.test(log(.$flux_µgC_gC_day))) %>%
  tidy(norm) %>%
  print ->
  shapiro_trans
save_data(shapiro_trans)

# -----------------------------------------------------------------------------
# Effects of temperature and moisture on gas fluxes

# Fit gases separately, just for simplicity
printlog(SEPARATOR)
printlog("Fitting CO2 model...")

# Add a small value to all flux values to ensure they're positive before log-transform
fluxdata$flux_µgC_gC_day1 <- fluxdata$flux_µgC_gC_day + FLUX_ADDITION

# C_percent and N_percent are *highly* correlated (r=0.98+)
# Generally it seems that N_percent produces slightly better model fits,
# so we use it here, not considering C_percent further
m_co2_lme <- lme(log(flux_µgC_gC_day1) ~ Temperature * WC_gravimetric + 
                   (Temperature + WC_gravimetric) * (N_percent + DOC_mg_kg), # C_percent + 
                 data = fluxdata,
                 subset = Gas == "CO2" & !outlier,
                 random = ~ 1 | Core, 
                 method = "ML")
step_co2_lme <- MASS::stepAIC(m_co2_lme, direction = "both")

# TODO: a Q10 calculation. Use nls?
# Q10 = R2/R1 ^ (10/(T2-T1))
# fit_q10_model <- function( d ) {		# returns model or NA if nls errors out
#   tryCatch( nls( Resp_mass ~ R20 * Q10 ^ ( ( Tair-20 )/10 ), data=d, start=c( R20=5, Q10=2 ) ),
#             error=function( e ) NA )
# }

printlog(SEPARATOR)
printlog("Fitting CH4 model...")
m_ch4_lme <- lme(log(flux_µgC_gC_day1) ~ Temperature * WC_gravimetric + 
                   (Temperature + WC_gravimetric) * (N_percent + DOC_mg_kg), # C_percent + 
                 data = fluxdata, 
                 subset = Gas == "CH4" & !outlier,
                 random = ~ 1 | Core, 
                 method = "ML")
step_ch4_lme <- MASS::stepAIC(m_ch4_lme, direction = "both")


printlog("All done with", SCRIPTNAME)
closelog()
