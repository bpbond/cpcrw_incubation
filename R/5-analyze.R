# Final analysis script: statistical tests, final plots, etc.
# Called by the R Markdown document (the manuscript).
# Ben Bond-Lamberty January 2016

source("R/0-functions.R")

SCRIPTNAME  	<- "5-analyze.R"

library(nlme)       # 3.1-128
#library(MASS)       # 7.3-45

library(broom)      # 0.4.1
library(reshape2)   # 1.4.1
library(agricolae)  # v1.2.4

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

coredata <- read_csv(COREDATA_FILE, skip = 1)

fluxdata_orig <- read_csv(FLUXDATA_FILE)
fluxdata_orig$Treatment <- factor(fluxdata_orig$Treatment, 
                                  levels = c("Field moisture", "Controlled drought", "Drought"))

# -----------------------------------------------------------------------------
# Transform into long format and handle outliers

co2_mad_outlier_count <- sum(fluxdata_orig$CO2_outlier, na.rm = TRUE)
ch4_mad_outlier_count <- sum(fluxdata_orig$CH4_outlier, na.rm = TRUE)
mad_outlier_count <- co2_mad_outlier_count + ch4_mad_outlier_count
# The CV test applies to both CO2 and CH4, so # is multiplied by 2
cv_outlier_count <- sum(fluxdata_orig$CV_outlier, na.rm = TRUE) * 2

printlog("Transforming...")
fluxdata_orig %>%
  select(samplenum, Core, Treatment, Temperature, inctime_days, 
         Mass_g, SoilDryMass_g, WC_gravimetric, WC_volumetric, WFPS_percent,
         CO2_flux_mgC_hr, CH4_flux_mgC_hr,
         CO2_outlier, CH4_outlier, CV_outlier) %>%
  melt(measure.vars = c("CO2_flux_mgC_hr", "CH4_flux_mgC_hr"),
       value.name = "flux_mgC_hr") %>%
  mutate(Gas = substr(variable, 1, 3)) %>%
  select(-variable) ->
  fluxdata

pre_outlier_count <- nrow(fluxdata)

# Combine the gas-specific outlier fields into a single outlier flag
fluxdata$outlier <- fluxdata$CO2_outlier | fluxdata$CV_outlier
fluxdata$outlier[fluxdata$Gas == "CH4"] <- fluxdata$CH4_outlier[fluxdata$Gas == "CH4"]
fluxdata$CO2_outlier <- fluxdata$CH4_outlier <- fluxdata$CV_outlier <- NULL

# Remove outliers and zero fluxes from the data set
fluxdata %>% 
  filter(flux_mgC_hr > 0 & !outlier) ->
  fluxdata

# -----------------------------------------------------------------------------
# Read C, N, DOC data; summarise; test for treatment effects; merge with fluxdata

printlog(SEPARATOR)
printlog("Reading", CNDATA_FILE)
read_csv(CNDATA_FILE) %>%
  select(DOC_mg_kg,	C_percent, N_percent, Core) %>%
  group_by(Core) %>%
  # Summarise by core, averaging duplicates
  summarise_each(funs(mean(., na.rm = TRUE)), DOC_mg_kg, C_percent, N_percent) %>%
  # Compute C:N ratio, following Reviewer 2's suggestion
  mutate(CN = ifelse(N_percent > 0, C_percent / N_percent, NA)) ->
  cndata_orig

# Compute correlations between the variables
cndata_cor <- cor(cndata_orig[-1], use = "complete.obs")

cndata_orig %>%
  left_join(read_csv(TREATMENTS, skip = 1), by = "Core") ->
  cndata

cndata %>%
  summarise_each(funs(mean, sd), DOC_mg_kg, C_percent, N_percent, CN) %>%
  print ->
  cndata_summary
# This isn't working via dplyr - compute manually
cndata_summary$CN_mean <- mean(cndata$CN, na.rm = TRUE)
cndata_summary$CN_sd <- sd(cndata$CN, na.rm = TRUE)

lm(formula = DOC_mg_kg ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print
lm(formula = C_percent ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print
lm(formula = N_percent ~ Treatment * Temperature, data = cndata) %>%
  anova %>% print
lm(formula = CN ~ Treatment * Temperature, data = cndata) %>%
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

save_data(fluxdata, fn = FLUXDATA_FINAL_FILE, scriptfolder = FALSE)

# -----------------------------------------------------------------------------
# Summarize flux information for easy access by markdown code later

fluxdata %>%
  group_by(Gas) %>%
  summarise_each(funs(min, max, mean, sd), flux_µgC_g_day, flux_µgC_gC_day) ->
  fluxsummary

nms <- fluxsummary$Gas
fluxsummary <- as.data.frame(t(fluxsummary[-1]))
colnames(fluxsummary) <- nms

save_data(fluxsummary)

# -----------------------------------------------------------------------------
# Water content over time figure and data

printlog(SEPARATOR)
printlog("Water content over time...")

figureA <- ggplot(fluxdata_orig, aes(inctime_days, WC_gravimetric, color=Treatment, group=Core)) +
  geom_point() + geom_line() + 
  facet_grid(~Temperature) + 
  xlab("Incubation day") + ylab("Gravimetric water content (fraction dry mass)") +
  scale_color_brewer(palette = "Set1")
save_plot("figureA", figureA)

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
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage, Treatment) %>% 
  summarise_each(funs(mean, sd, max, min), WFPS_percent) ->
  wfps
fluxdata %>% 
  filter(!is.na(stage)) %>%
  group_by(stage) %>% 
  summarise_each(funs(mean, sd, max, min), WFPS_percent) ->
  wfps_alltrt

# Compute key soil moisture rates for use in abstract and results of ms
gwcbegin <- subset(gwc_alltrt, stage == "Beginning")
gwcend <- subset(gwc, stage == "End")
gwcendmean <- gwcend$mean
names(gwcendmean) <- gwcend$Treatment
gwcendsd <- gwcend$sd
names(gwcendsd) <- gwcend$Treatment
vwcbegin <- subset(vwc_alltrt, stage == "Beginning")
vwcend <- subset(vwc_alltrt, stage == "End")


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
  fd <- filter(fd, !is.na(incday), !is.na(flux_sd), !is.na(flux))
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

save_plot("figureB", figureB)
save_plot("figureC", figureC)

# -----------------------------------------------------------------------------
# Cumulative flux figure and Tukey HSD tests

printlog(SEPARATOR)
printlog("Running Tukey HSD tests on cumulative emissions...")
read_csv(FLUXDATA_CUM_CORE_FILE) %>%
  mutate(Temperature = as.factor(Temperature)) ->
  fd_cumulative_core
co2_aov <- aov(cum_flux_mgC_gC ~ Treatment + Temperature, 
               data = subset(fd_cumulative_core, Gas == "CO2"))
co2_hsd <- TukeyHSD(co2_aov)
print(co2_hsd)
ch4_aov <- aov(cum_flux_mgC_gC ~ Treatment + Temperature, 
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

figureD <- ggplot(fluxdata_cumulative, aes(Temperature, cum_flux_mgC_gC, fill = Treatment)) + 
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(color = Treatment, 
                    ymin = cum_flux_mgC_gC * 0.9, 
                    ymax = cum_flux_mgC_gC + cum_flux_mgC_gC_sd), 
                position = position_dodge(0.9), width = 0.4) +  
  facet_grid(Gas ~ ., scales = "free") +
  ylab(expression(Cumulative~C~(mg~g~C^{-1}))) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
save_plot("figureD", figureD)

# Make the labels for this figure. Kind of a PITA
# Uses `HSD.test` in the `agricolae` package
fluxdata_cumulative %>% 
  group_by(Gas, Treatment, Temperature) %>% 
  summarise(cum_flux_mgC_gC = max(cum_flux_mgC_gC + cum_flux_mgC_gC_sd) * 1.1) %>%
  mutate(trt = paste(Gas, Treatment, Temperature, sep = ":")) %>%
  group_by(Gas) %>%
  mutate(cum_flux_mgC_gC = max(cum_flux_mgC_gC, na.rm = TRUE) * 1.1) ->
  labeldata
out_co2 <- HSD.test(co2_aov, trt = c("Treatment", "Temperature"))
out_co2$groups$trt <- paste("CO2", trimws(out_co2$groups$trt), sep = ":")
out_ch4 <- HSD.test(ch4_aov, trt = c("Treatment", "Temperature"))
out_ch4$groups$trt <- paste("CH4", trimws(out_ch4$groups$trt), sep = ":")
rbind(out_co2$groups, out_ch4$groups) %>%
  left_join(labeldata, by = "trt") ->
  labeldata
figureD <- figureD + geom_text(data = labeldata, 
                               aes(label = M), 
                               position = position_dodge(width = 1))

# -----------------------------------------------------------------------------
# Test effects of %C, %N, etc. on cumulative fluxes (per Referee 3 suggestion)

printlog("Merging cumulative fluxes with CN data...")
fd_cumulative_core %>%
  left_join(cndata_orig, by = "Core") ->
  fd_cumulative_r3
fd_cumulative_r3 <- fd_cumulative_r3[complete.cases(fd_cumulative_r3),]

printlog("Fitting CO2 model for cumulative fluxes...")
m_co2_cum <- lm(cum_flux_mgC_gC ~ Temperature + Treatment +
                   N_percent + DOC_mg_kg + CN, # C_percent + 
                 data = fd_cumulative_r3, 
                 subset = Gas == "CO2")
step_co2_cum <- MASS::stepAIC(m_co2_cum, direction = "both")
print(summary(m_co2_cum))

printlog("Fitting CH4 model for cumulative fluxes...")
m_ch4_cum <- lm(cum_flux_mgC_gC ~ Temperature + Treatment +
                   N_percent + DOC_mg_kg + CN, # C_percent + 
                 data = fd_cumulative_r3, 
                 subset = Gas == "CH4")
step_ch4_cum <- MASS::stepAIC(m_ch4_cum, direction = "both")
print(summary(step_ch4_cum))

# -----------------------------------------------------------------------------
# Calculate drought reduction (%)

fluxdata_cumulative %>% 
  filter(Treatment %in% c("Drought", "Field moisture"), Gas == "CO2") %>% 
  dcast(Temperature ~ Treatment, value.var = "cum_flux_mgC_gC") %>% 
  mutate(Reduction = (`Field moisture` - Drought) / `Field moisture` * 100) ->
  drought_effect
rownames(drought_effect) <- drought_effect$Temperature

# -----------------------------------------------------------------------------
# Q10 based on cumulative fluxes
# TODO: I hate the hardcoded temperature values below!

stopifnot(length(unique(fluxdata_cumulative$Temperature)) == 2)
fluxdata_cumulative %>%
  reshape2::dcast(Treatment + Gas ~ Temperature, 
                  value.var = "cum_flux_mgC_gC") %>%
  mutate(Q10 = (`20` / `4`) ^ (10 / (hightemp - lowtemp))) %>%
  reshape2::dcast(Treatment ~ Gas, value.var = "Q10") ->  
  Q10_cumulative
rownames(Q10_cumulative) <- Q10_cumulative$Treatment

fluxdata_cumulative %>%
  mutate(cum_flux_norm = cum_flux_mgC_gC / WFPS_percent) %>%
  reshape2::dcast(Treatment + Gas ~ Temperature, 
                  value.var = "cum_flux_norm") %>%
  mutate(Q10 = (`20` / `4`) ^ (10 / (hightemp - lowtemp))) %>%
  reshape2::dcast(Treatment ~ Gas, value.var = "Q10") ->  
  wfpsQ10_cumulative
rownames(wfpsQ10_cumulative) <- wfpsQ10_cumulative$Treatment

# -----------------------------------------------------------------------------
# CO2:CH4 emissions ratio

printlog(SEPARATOR)
printlog("Computing emissions ratios...")
fluxdata_cumulative %>% 
  group_by(Treatment, Temperature) %>% 
  summarise(CO2CH4_ratio = max(cum_flux_mgC_gC) / min(cum_flux_mgC_gC)) %>%
  na.omit %>%
  print ->
  gas_ratio

# Compute ratio-related text for use in manuscript
min(gas_ratio$CO2CH4_ratio) %>% 
  log10 %>% 
  floor %>%
  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")[.] ->
  minmag
min(gas_ratio$CO2CH4_ratio / 1e6)  %>% 
  formatC(digits = 1, format = 'f') ->
  minratio
mintrt <- gas_ratio$Treatment[which.min(gas_ratio$CO2CH4_ratio)]
mintemp <- gas_ratio$Temperature[which.min(gas_ratio$CO2CH4_ratio)]
max(gas_ratio$CO2CH4_ratio / 1e6) %>% 
  formatC(digits = 1, format = 'f') ->
  maxratio
maxtrt <- gas_ratio$Treatment[which.max(gas_ratio$CO2CH4_ratio)]
maxtemp <- gas_ratio$Temperature[which.max(gas_ratio$CO2CH4_ratio)]

# -----------------------------------------------------------------------------
# Examine data distribution
# Log-transforming doesn't fix the lack of normality in our data,
# but it's a major improvement; see graphs.

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
# Effects of temperature, moisture, C, N, etc. on gas fluxes

# Fit gases separately, just for simplicity
printlog(SEPARATOR)
printlog("Fitting CO2 model...")

# Add a small value to all flux values to ensure they're positive before log-transform
fluxdata$flux_µgC_gC_day1 <- fluxdata$flux_µgC_gC_day + FLUX_ADDITION

# Reviewer 1 had an interesting suggestion: see if moisture effects
# change over time (specifically, the reviewer said, "Studies have shown that 
# moisture can have a weaker effect on temperature sensitivity early on during 
# an incubation experiment, in the presence of more labile C.")
# Test this by looking at different thirds of the data
fluxdata$third <- cut(fluxdata$incday, breaks = 3)

# C_percent and N_percent are *highly* correlated (r=0.98+)
# Generally it seems that N_percent produces slightly better model fits,
# so we use it here, not considering C_percent further
# Reviewer 2 suggests adding C:N as a predictor, too, but it doesn't appear
# to be significant, and requires removing ~3% of data (due to NA)
m_co2_lme <- lme(log(flux_µgC_gC_day1) ~ Temperature * WC_gravimetric +
                   (Temperature + WC_gravimetric) * (N_percent + DOC_mg_kg), # C_percent + 
                 data = fluxdata,
                 subset = Gas == "CO2", #  & !is.na(CN)
                 random = ~ 1 | Core, 
                 method = "ML")
step_co2_lme <- MASS::stepAIC(m_co2_lme, direction = "both")

# Test Reviewer 1's suggestion about water (and temperature) 
# sensitivities changing with time
m_co2_lme_thirds <- update(m_co2_lme, ~ . + 
                             WC_gravimetric * third + 
                             Temperature * third)
step_co2_lme_thirds <- MASS::stepAIC(m_co2_lme_thirds, direction = "both")
co2_temp_time <- step_co2_lme_thirds$coefficients$fixed["Temperature:third(34.3,67.7]"]


printlog(SEPARATOR)
printlog("Fitting CH4 model...")
m_ch4_lme <- lme(log(flux_µgC_gC_day1) ~ Temperature * WC_gravimetric + 
                   WC_gravimetric * third +
                   (Temperature + WC_gravimetric) * (N_percent + DOC_mg_kg), # C_percent + 
                 data = fluxdata, 
                 subset = Gas == "CH4",
                 random = ~ 1 | Core, 
                 method = "ML")
step_ch4_lme <- MASS::stepAIC(m_ch4_lme, direction = "both")

# Test Reviewer 1's suggestion about water (and temperature) sensitivity changing with time
m_ch4_lme_thirds <- update(m_ch4_lme, ~ . + 
                             WC_gravimetric * third + 
                             Temperature * third)
step_ch4_lme_thirds <- MASS::stepAIC(m_ch4_lme_thirds, direction = "both")
ch4_wc_time <- step_ch4_lme_thirds$coefficients$fixed["WC_gravimetric:third(34.3,67.7]"]

# -----------------------------------------------------------------------------
# Compute outlier and exclusion numbers, to report in manuscript
rn    <- length(readLines(REMOVEDDATA_FILE))
sdn   <- length(readLines(SUMMARYDATA_FILE))
fluxn <- nrow(fluxdata)

printlog("All done with", SCRIPTNAME)
closelog()
