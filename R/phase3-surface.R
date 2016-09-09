source("R/utils.R", local = TRUE)

################################################################################
################################  UI  ##########################################
################################################################################

phase3spUI <- function(id) {
  ns <- NS(id)
    
  fluidPage(
    fluidRow(
      column(8, plotOutput(ns("SurfacePlot"), height = "400px", width = "475px", click = clickOpts(id = ns("surface_click"))),
        br(),
        checkboxGroupInput(ns("climInfo"), label="Plot Overlays",
          inline=T,
          c("Historical climate (1960-2000)"=1,
            "CMIP5 projections"=2))),
      column(4, 
        sliderInput(ns("demand"), "Demand increase", 
          min=50, max=200, step=30, value=110, post="%", ticks = F),
        sliderInput(ns("size"), "Reservoir size", 
          min=80, max=140, step=10, value=90, post="MCM", ticks = F),
        htmlOutput(ns("surface_click_info"))
        )
    )
  ) #fluidpage close
      
}



################################################################################
############################## SERVER ##########################################
################################################################################

phase3sp <- function(input, output, session, surface_data, parc_data) {
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
}
