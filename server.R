#Global script. non-essentials & one-time runs goes here..
source("global.R", local = TRUE)

shinyServer(function(input, output, session) {

# Phase 3 ---------------------------------------------------------------------- 
  ph3 <- callModule(phase3,
                    "phase3",
                    surface_data = surface_data,
                    parc_data    = parc_data)

# Phase 4, new -----------------------------------------------------------------
  adap <- callModule(phase4, 
                     "adaptation", 
                     base_clim    = base_clim, 
                     adap_results = adap_results,
                     CMIP5        = CMIP5)
})