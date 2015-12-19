# Computes fluxes. In progress.
# Ben Bond-Lamberty December 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "5-analyze.R"
FLUXDATA   <- file.path(outputdir(scriptfolder = FALSE), "fluxdata.csv")

# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE) # open log

printlog("Welcome to", SCRIPTNAME)

fluxdata_orig <- read_csv(FLUXDATA)

printlog("Transforming...")
fluxdata_orig %>%
  select(Core, Treatment, Temperature, incday, WC_fraction, CO2_flux_µmol_g_s, CH4_flux_µmol_g_s) %>%
  melt(measure.vars = c("CO2_flux_µmol_g_s", "CH4_flux_µmol_g_s"),
       value.name = "flux_µmol_g_s") %>%
  mutate(Gas = substr(variable, 1, 3)) %>%
  select(-variable) ->
  fluxdata

fluxdata$Treatment <- factor(fluxdata$Treatment, levels = c("Field moisture", "Controlled drought", "Drought"))

save_data(fluxdata, fname = "fluxdata_long")

# -----------------------------------------------------------------------------
# Does WC affect gas fluxes?
fluxdata %>%
  group_by(Treatment, Temperature, Gas) %>%
  do(mod = lm(flux_µmol_g_s ~ WC_fraction, data = .)) ->
  models

models %>% 
  do(data.frame(    # from the 'do' help page
    var = names(coef(.$mod)),
    coef(summary(.$mod)))) %>%
  filter(var == "WC_fraction") %>%
  mutate(p.value = round(`Pr...t..`, 3),
         signif = p.value < 0.05) %>%
  select(-var, -`Pr...t..`, -Estimate, -`Std..Error`) ->
  modelsummary

fluxdata %>%
  group_by(Treatment, Temperature, Gas) %>% 
  summarise(n=n()) %>% 
  cbind(modelsummary) ->
  WC_effect

print(WC_effect)
save_data(WC_effect)


# -----------------------------------------------------------------------------
# Main treatment effects

# Fitting a non-mixed-effects model for now
# TODO: later, might want to explore having Core as a random effect

# Fit gases separately, just for simplicity
printlog("Fitting CO2 model...")
m_co2 <- lm(flux_µmol_g_s ~ Treatment * Temperature + WC_fraction,
            data = fluxdata, subset = Gas == "CO2")
print(summary(m_co2))

printlog("Fitting CH4 model...")
m_co2 <- lm(flux_µmol_g_s ~ Treatment * Temperature + WC_fraction,
            data = fluxdata, subset = Gas == "CH4")
print(summary(m_co2))


printlog("All done with", SCRIPTNAME)
closelog()
