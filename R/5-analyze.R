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


# m_co2 <- lm(cum_flux_mgC ~ Treatment * Temperature, 
#             data=subset(fd_summary_core, Gas=="CO2"))
# summary(m_co2)
# 
# m_ch4 <- lm(cum_flux_mgC ~ Treatment * Temperature, 
#             data=subset(fd_summary_core, Gas=="CH4"))
# summary(m_ch4)


printlog("All done with", SCRIPTNAME)
closelog()
