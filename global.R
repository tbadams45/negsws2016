#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Global script: has to be sourced in both server.R and ui.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PACKAGE & DATA SOURCES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # package managment tool
pacman::p_load(stats, plyr, magrittr, truncnorm, RColorBrewer, sirad, tidyr, readr, lubridate, ggplot2, LaplacesDemon, mvtnorm, shiny, shinyjs, shinyBS, htmlwidgets, shinythemes, shinydashboard, plotly, dplyr, wrviz, devtools, ggthemes)
pacman::p_load_gh(c("timelyportfolio/parcoords", "tbadams45/wrviz", "rstudio/DT"))
if(packageVersion("DT") < "0.1.57") {
  pacman::p_install_gh(c("rstudio/DT"))
  pacman::p_load_gh(c("rstudio/DT"))
}
#devtools::install_github("timelyportfolio/parcoords@feature/dimensions")

# Modules
source("R/phase3.R")
source("R/phase4.R")
source("R/climate_robustness.R") #used in phase 4
source("R/utils.R")


# -------------------------- Phase 3 data. -------------------------------------
parc_data <- read_csv("./data/LHS_output.csv", progress = FALSE)

surface_data <- read_csv("./data/stresstest_ffd.csv", progress = FALSE) %>%
  mutate(reliability = rel, creliability = crel) %>%
  mutate(demand = multipleReplace(demand, what=c(57, 68, 80, 91, 103, 114),
    by=c(50, 80, 110, 140, 170, 200)))

surface_data_mean <- surface_data %>%
  group_by(dataset, size, demand, temp, prec) %>%
  summarize(reliability = mean(reliability), creliability = mean(creliability),
    safeyield = mean(safeyield)) %>% mutate(nvar = 0)

surface_data %<>% bind_rows(surface_data_mean)


# ----------------- Phase 4 (comparison based) data. ---------------------------
adap_results <- read_csv("./data/mean_adaptation_results.csv")

#errors in simulation run resulted in NAs. For now, replace them with dummy values.
adap_results[is.na(adap_results)] <- 80

metrics <- c("dom_rel", "irr_rel", "eco_rel", "dom_crel", "irr_crel",
             "eco_crel", "dom_res", "irr_res", "eco_res", "dom_vul",
             "irr_vul", "eco_vul")
adap_results[metrics] <- lapply(adap_results[metrics],
                                function(x) round(x, digits = 2))

base_clim <- filter(adap_results, temp == 0 & precip == 1)
base_clim <- mutate(base_clim, ID = as.numeric(rownames(base_clim)))

CMIP5 <- read_csv("./data/CMIP5_deltaclim.csv") %>%
  mutate(del_prec = del_prec/100 + 1) %>%
  rename(Scenario = scenario)