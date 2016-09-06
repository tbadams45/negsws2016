
# Global script. non-essentials & one-time runs goes here..
source("global.R", local = TRUE)

#### Define side-bar
sidebar <- dashboardSidebar(width = 200, hr(), {
  sidebarMenu(id = "tabs", 
    menuItem("Vulnerabilty", tabName = "tab_phase3", icon = icon("circle-o")),
    menuItem("Adaptation", tabName = "tab_adap", icon = icon("circle-o")),
    tags$style(type = 'text/css', 
      "footer{position: absolute; bottom:2%; left: 5%; padding:5px;}"),
    HTML('<footer> &copy; 2016 HRG </footer>')
  )
})

#### Define body 
body <- dashboardBody(
  useShinyjs(), 
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  tabItems(
    
    # Phase 3 - climate heatmap and multivariate risk analysis
    tabItem(tabName="tab_phase3", {
      phase3UI("phase3")
    }), #tabitem close
      
    # Phase 4 - comparison based
    tabItem(tabName = "tab_adap", {
      phase4UI("adaptation", base_clim)
    }) # end adapation tab
      
  )
)

####  Define the dashboard 
dashboardPage(
  skin    = "black",
  header  = dashboardHeader(titleWidth = 157, title = "Dashboard"),
  sidebar = sidebar,
  body    = body
)