
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



shinyUI(
  
  fluidPage(
    leafletOutput("mymap", height = 1000),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 100,
                  width = 300, height = 0,
                  
                  sliderInput("time", "date",1,
                              nrow(df),
                              value = 0,
                              max = 310,
                              step=5,
                              animate=animationOptions(interval = 300, loop = FALSE))
                  
    )
  )
  
)
