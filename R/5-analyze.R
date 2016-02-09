# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "5-analyze.R"
FLUXDATA   <- file.path(outputdir(scriptfolder = FALSE), "fluxdata.csv")
FLUXDATA_CUMULATIVE <- file.path(outputdir(scriptfolder = FALSE), "fluxdata_cumulative.csv")

library(broom)  # 0.4.0
library(reshape2) # 1.4.1

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
# Water content over time figure

figureA <- ggplot(fluxdata_orig, aes(inctime_days, WC_gravimetric * 100, color=Treatment, group=Core)) 
figureA <- figureA + geom_point() + geom_line()
figureA <- figureA + facet_grid(~Temperature) 
figureA <- figureA + xlab("Incubation day") + ylab("Gravimetric water content (%)")

# -----------------------------------------------------------------------------
# Fluxes over time figure

printlog("fluxes over time...")
fluxdata %>%
  mutate(incday = floor(inctime_days)) %>%
  group_by(Gas, Temperature,Treatment, incday) %>%
  summarise(flux = mean(flux_µmol_g_s), flux_sd = sd(flux_µmol_g_s)) ->
  fluxdata_figsBC

figsBC <- function(fd) {
  panellabels <- 
  ggplot(fd, aes(incday, flux)) + 
    geom_point() + 
    facet_grid(Temperature ~ Treatment) + 
    geom_errorbar(aes(ymin = flux - flux_sd, ymax = flux + flux_sd)) +
    xlab("Incubation day") + ylab(expression(µmol~g^{-1}~s^{-1}))
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
# Cumulative flux figure

fluxdata_cumulative <- read_csv(FLUXDATA_CUMULATIVE)
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
  group_by(Gas, Treatment, Temperature) %>%
  filter(!outlier) %>%
  do(mod = lm(log(flux_µmol_g_s) ~ WC_gravimetric, data = .)) %>%
  broom::glance(mod) %>%
  select(Gas, Treatment, Temperature, adj.r.squared, sigma, p.value, AIC) ->
  #  mutate(signif = p.value < 0.05) ->
  WC_effect

print(WC_effect)
save_data(WC_effect)

# A quick visualization
p <- qplot(WC_gravimetric, flux_µmol_g_s, data=fluxdata) 
p <- p + facet_grid(Gas~Temperature, scales="free") + geom_smooth(method='lm')
save_plot(p, pname = "WC_gravimetric_effect")

# -----------------------------------------------------------------------------
# Per-core models - look at R2, etc. variability

# printlog("Per-core models...")
# fluxdata %>%
#   filter(flux_µmol_g_s > 0) %>%
#   group_by(Core, Gas) %>%
#   do(mod = lm(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric, data = .)) %>%
#   glance(mod) %>%
#   select(Core, Gas, adj.r.squared, sigma, p.value, AIC) ->
#   per_core_models
# 
# print(per_core_models)
# save_data(per_core_models)
# 
# p <- ggplot(per_core_models, aes(adj.r.squared)) + geom_histogram(bins = 30)
# p <- p + facet_grid(Gas ~ .) 
# p <- p + ggtitle("Per-core temp, WC models")
# print(p)
# save_plot("per_core_models")

# -----------------------------------------------------------------------------
# Effects of temperature and moisture on gas fluxes

# Fitting a non-mixed-effects model for now
# TODO: later, might want to explore having Core as a random effect

# Fit gases separately, just for simplicity
printlog("Fitting CO2 model...")
m_co2 <- lm(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric,
            data = fluxdata, subset = Gas == "CO2")
print(summary(m_co2))

# TODO: a Q10 calculation. Use nls?
# Q10 = R2/R1 ^ (10/(T2-T1))
# fit_q10_model <- function( d ) {		# returns model or NA if nls errors out
#   tryCatch( nls( Resp_mass ~ R20 * Q10 ^ ( ( Tair-20 )/10 ), data=d, start=c( R20=5, Q10=2 ) ),
#             error=function( e ) NA )
# }

#lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject))
# library(nlme)
# m_co2_lme <- lme(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric,
#                  data = fluxdata, random = ~ 1 | Core)

printlog("Fitting CH4 model...")
m_ch4 <- lm(log(flux_µmol_g_s) ~ Temperature * WC_gravimetric,
            data = fluxdata, subset = Gas == "CH4")
print(summary(m_ch4))


printlog("All done with", SCRIPTNAME)
closelog()
