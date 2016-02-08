# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "5-analyze.R"
FLUXDATA   <- file.path(outputdir(scriptfolder = FALSE), "fluxdata.csv")

library(broom)  # 0.4.0

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

fluxdata_orig <- read_csv(FLUXDATA)

printlog("Transforming...")
fluxdata_orig %>%
  select(Core, Treatment, Temperature, inctime_days, WC_gravimetric, CO2_flux_µmol_g_s, CH4_flux_µmol_g_s,
         CO2_outlier, CH4_outlier) %>%
  melt(measure.vars = c("CO2_flux_µmol_g_s", "CH4_flux_µmol_g_s"),
       value.name = "flux_µmol_g_s") %>%
  melt(measure.vars = c("CO2_outlier", "CH4_outlier"), 
       variable.name = "outlier_name", value.name = "outlier") %>%
  mutate(Gas = substr(variable, 1, 3)) %>%
  select(-variable, -outlier_name) ->
  fluxdata

fluxdata$Treatment <- factor(fluxdata$Treatment, levels = c("Field moisture", "Controlled drought", "Drought"))

save_data(fluxdata, fname = "fluxdata_long")


# -----------------------------------------------------------------------------
# Examine data distribution
# Log-transforming doesn't fix the lack of normality in our data,
# but it's a major improvement; see graphs.

fluxdata <- fluxdata %>% filter(flux_µmol_g_s > 0) 

printlog("Graphing flux distributions and testing for normality...")
p <- ggplot(fluxdata, aes(x = flux_µmol_g_s)) + geom_histogram(bins = 30)
p <- p + facet_grid(~Gas, scales = "free") + ggtitle("Distribution of raw data")
print(p)
save_plot("distribution_raw")

p <- ggplot(fluxdata, aes(x = log(flux_µmol_g_s))) + geom_histogram(bins = 30)
p <- p + facet_grid(~Gas, scales = "free") + ggtitle("Distribution of log-transformed data")
print(p)
save_plot("distribution_log")

printlog("Shapiro-Wilk normality test on raw data:")
fluxdata %>%
  group_by(Gas) %>%
  do(norm = shapiro.test(.$flux_µmol_g_s)) %>%
  tidy(norm) %>%
  print ->
  shapiro
save_data(shapiro)

printlog("Shapiro-Wilk normality test on log-transformed data:")
fluxdata %>%
  group_by(Gas) %>%
  do(norm = shapiro.test(log(.$flux_µmol_g_s))) %>%
  tidy(norm) %>%
  print ->
  shapiro_trans
print(shapiro_trans)
save_data(shapiro_trans)

# -----------------------------------------------------------------------------
# Does WC affect gas fluxes within temperature and treatment?

printlog("Summarizing WC effect...")
fluxdata %>%
  filter(flux_µmol_g_s > 0) %>%
  group_by(Treatment, Temperature, Gas) %>%
  do(mod = lm(log(flux_µmol_g_s) ~ WC_gravimetric, data = .)) %>%
  glance(mod) %>%
  select(Treatment, Temperature, Gas, adj.r.squared, sigma, p.value, AIC) %>%
  mutate(signif = p.value < 0.05) ->
  WC_effect

print(WC_effect)
save_data(WC_effect)


# -----------------------------------------------------------------------------
# Main treatment effects

# Fitting a non-mixed-effects model for now
# TODO: later, might want to explore having Core as a random effect

# Fit gases separately, just for simplicity
printlog("Fitting CO2 model...")
m_co2 <- lm(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric,
            data = fluxdata, subset = Gas == "CO2")
print(summary(m_co2))

#lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject))
# library(nlme)
# m_co2_lme <- lme(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric,
#                  data = fluxdata, random = ~ 1 | Core)

printlog("Fitting CH4 model...")
m_ch4 <- lm(log(flux_µmol_g_s) ~ Temperature + WC_gravimetric,
            data = fluxdata, subset = Gas == "CH4")
print(summary(m_ch4))


printlog("All done with", SCRIPTNAME)
closelog()
