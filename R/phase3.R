source("R/utils.R", local = TRUE)

################################################################################
################################  UI  ##########################################
################################################################################

phase3UI <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    
    # surface plot
    tabPanel("Climate Only", value = ns("climate_only"),
      fluidPage(
        box(title = strong("Settings"), width=4, status="primary",
          div(id = ns("SurfaceInputs"), 
            wellPanel(
              em(strong(helpText(span("Non-climatic attributes", 
                style = "color:blue")))),
              sliderInput(ns("demand"), "Demand increase", 
                min=50, max=200, step=30, value=80, post="%", ticks = F),
              bsTooltip(ns("demand"), 
                "Increase in demand relative to the 2015 level (38 MCM/year)",
                placement = "right", options = list(container = "body")),
              sliderInput(ns("size"), "Reservoir size", 
                min=80, max=140, step=10, value=120, post="MCM", ticks = F),
              bsTooltip(ns("size"), 
                "Reservoir storage capacity",
                placement = "right", options = list(container = "body"))
            ) # wellpanel close
          ) # div close
        ), #box close
        box(title = strong("Climate response surface"), width=8, 
          align = "left", status="primary",
          plotOutput(ns("SurfacePlot"), height = "500px", width = "600px", click = clickOpts(id = ns("surface_click"))),
          br(),
          checkboxGroupInput(ns("climInfo"), label="Display climate information",
            inline=F,
            c("Historical climate (1960-2000)"=1,
              "CMIP5 projections"=2,
              "CMIP5 confidence level (95%)"=3,
              "CMIP5 confidence level (99%)"=4)),
          htmlOutput(ns("surface_click_info")),
          bsTooltip(ns("climInfo"), 
            "Information to evaluate the subjective likelihood of climate changes",
            placement = "bottom", options = list(container = "body"))
        ) #box close
      ) #fluidpage close
      
    ), # close tabPanel
    
    # parallel coordinates plot 
    tabPanel("Multivariate", value = ns("multivariate"),
      fluidPage(
        box(title = strong("Settings"), width = 3, solidHeader=F, 
          status="primary",
          wellPanel(
            em(strong(helpText(span("Design features", style = "color:blue")))),
            sliderInput(ns("ParcoordSizeSelect"), "Reservoir size", 
              min=80, max=140, step=20, value=120, post="MCM", ticks=F)
          )
        ), #box close
        box(title = strong("Vulnerability domain"), width = 9, align = "left",
          solidHeader = F, status = "primary",
          parcoordsOutput(ns("ParcoordPlot")),
          br(), br(),
          plotOutput(ns("npv_distribution"), height = "300px", width = "500px"),
          br(), br(), br(), br()
        ) #box close
      ) #fluidpage close
    ) #close tabPanel
  ) # close tabsetPanel
}



################################################################################
############################## SERVER ##########################################
################################################################################

phase3 <- function(input, output, session, surface_data, parc_data) {
  # RESPONSE SURFACE ------------------------------------------------------------
  
  # Graphical parameters for surface plot
  label <- list(
    x = expression("Temperature change (" * degree * C *")"),
    y = paste("Precipitation change (%)"))
  
  tick <- list(x = seq(0,5,1), y = seq(0.7,1.5,0.1))
  lim  <- list(x = c(-0.5,5.5), y = c(0.65,1.55))
  
  font_size <- 14
  
  theme_set(theme_bw(base_size = font_size))
  theme_update(
    plot.title    = element_text(face = "bold"),
    panel.border  = element_rect(color = "black", fill = NA),
    legend.text   = element_text(size=font_size-2),
    legend.title  = element_text(size=font_size-2)
  )
  
  
  #### SURFACEPLOT DATA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  surfaceData <- reactive({
    
    var <- "reliability"
    
    dat <- surface_data %>% rename_(value = var) %>%
      filter(dataset=="Princeton" & size==input$size & demand==input$demand)
    
    #Plot the mean trace
    dat %<>% filter(nvar == 0)
    
    bin1 <- c(seq(40,90,10),95)
    bin2 <- seq(96,100,1)
    col1 <- colorRampPalette(c("firebrick2", "white"))(length(bin1))
    col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bin2))
    var_col <- c(col1,col2)
    var_bin <- c(bin1, bin2)  
    
    dat <- mutate(dat, Bins = cut(value,
      breaks = var_bin, dig.lab = 5, include.lowest = T))
    
    list(dat = dat, var_col = var_col)
  }) 
  
  SurfacePlot <- reactive({
    
    df   <- surfaceData()$dat
    varcols <- surfaceData()$var_col
    
    p1 <- ggplot(data = df, aes(x = temp, y = prec)) +
      geom_tile(aes(fill = Bins), color = "gray60") +
      scale_x_continuous(expand=c(0,0), breaks = tick$x) +
      scale_y_continuous(expand=c(0,0), breaks = tick$y,
        labels = seq(-30, +50, 10)) +
      scale_fill_manual(name = "Range", values = varcols, drop = FALSE)  +
      guides(fill = guide_legend(order = 1, keyheight = 1.5, keywidth = 1.5),
        shape = guide_legend(order = 2, keyheight = 1.5, keywidth = 1.5)) +
      labs(x = label$x, y = label$y)
    
    if(1 %in% input$climInfo) {
      p1 <- p1 + geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
        geom_hline(yintercept = 1, linetype = "dashed", size = 1)
    }
    if(2 %in% input$climInfo) {
      p1 <- p1 + geom_point(aes(x = del_temp, y = del_prec, shape = Scenario),
        size = 2, data = CMIP5, stroke = 1.5) +
        scale_shape_manual(name = "Scenarios", values =c(21,22,23,24))
    }
    if(3 %in% input$climInfo) {
      p1 <- p1 + stat_ellipse(aes(x = del_temp, y = del_prec), data = CMIP5,
        size = 1, linetype = "dotdash", level = 0.95)
    }
    if(4 %in% input$climInfo) {
      p1 <- p1 + stat_ellipse(aes(x = del_temp, y = del_prec), data = CMIP5,
        size = 1, linetype = "dotdash", level = 0.99)
    }
    
    p1
  })
  output$SurfacePlot <- renderPlot({SurfacePlot()})

  #### Interaction with heatmap/surface plot -----------------------------------
  
  output$surface_click_info <- renderUI({
    req(input$surface_click$x, input$surface_click$y)
    
    x <- rnd(input$surface_click$x, nearest = 1)
    y <- rnd(input$surface_click$y, nearest = 0.1)
    oldData <- filter(surfaceData()$dat)
    data <- filter(surfaceData()$dat, temp == round(x, 0), prec == round(y, 1))
    metric <- cap_first("reliability")
    HTML(paste0("<div class = 'surface-plot-click-info'>", "Values for selected box: <br>", "Temperature increase: ", data$temp, " &#8451;",
           "<br> Precipitation Change: ", data$prec*100, "% <br>", metric, ": ", round(data$value, 2), "</div>"))
  })
  
  
  
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
