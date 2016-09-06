source("R/utils.R", local = TRUE)

################################################################################
################################  UI  ##########################################
################################################################################

phase3pcUI <- function(id) {
  ns <- NS(id)

  # parallel coordinates plot 
  fluidPage(
    parcoordsOutput(ns("ParcoordPlot"), height = "275px"),
    column(8, 
      plotOutput(ns("npv_distribution"), height = "200px", width = "500px")
    ),
    column(4,
      sliderInput(ns("ParcoordSizeSelect"), "Reservoir size", 
        min=80, max=140, step=20, value=120, post="MCM", ticks=F)
    )
  ) #fluidpage close
}



################################################################################
############################## SERVER ##########################################
################################################################################

phase3pc <- function(input, output, session, surface_data, parc_data) {
  # MULTIVARIATE ANALYSIS ------------------------------------------------------
  parcoord_data <- reactive({
    parc_data %>% 
      filter(size == as.numeric(input$ParcoordSizeSelect)) %>%
      select(#"realization" = var, 
        "Temp" = dtemp,
        "Precip" = dprec, 
        "Price" = prc, 
        "Demand" = dem,
        "Sediment" = sed,
        "Discount Rate" = dsc,
        "NPV" = NPV) 
  })
  
  ####### PARALEL COORDINATES PLOT 
  output$ParcoordPlot <- renderParcoords({
    parcoords(
      data = parcoord_data(), 
      rownames = F,
      brushMode = "1D-axes", 
      brushPredicate = "and",
      reorderable = T, 
      axisDots = NULL,
      composite = NULL,
      margin = list(top = 20, left = 0, bottom = 50, right = 0),
      alpha = 0.3, 
      queue = T, 
      rate = 200,
      color = "steelblue",
      tasks = htmlwidgets::JS("function foo(){this.parcoords.alphaOnBrushed(0.15);}")
    )
  })
  
  
  pc_data_selected <- reactive({
    ids <- rownames(parcoord_data()) %in% input$ParcoordPlot_brushed_row_names
    rows <- parcoord_data()[ids,]
    rows
  })


  ####### NPV histogram - show distribution of selected area.
  output$npv_distribution <- renderPlot({
    req(nrow(pc_data_selected()) != 0)
    pc_data <- parcoord_data()
    pc_data$data <- 'All'
    pc_selected  <- pc_data_selected()
    pc_selected$data <- 'Selected'
    
    histo_data <- rbind(pc_data, pc_selected)
    
    ggplot(histo_data, aes(NPV)) + 
      geom_histogram(data = filter(histo_data, data == 'All'), binwidth = 80, color = "red", alpha = 0.2) + 
      geom_histogram(data = filter(histo_data, data == 'Selected'), binwidth = 80, color = "blue", alpha = 0.2) +
      theme_tufte(base_size = 14, base_family = 'GillSans') +
      coord_flip() + 
      theme(axis.title.y = element_text(angle = 0))
  })
}
