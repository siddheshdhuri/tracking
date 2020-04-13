
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  
  function(input, output, session) {
    points <- reactive({
      # df %>% 
      #   filter(created_at < input$time)
      # df <- df[input$time:(input$time+step_const) ,]
      thisdf <- df[0,]
      for( thiscar in unique(df$carid)){
        this_car_df <-  df[df$carid == thiscar, ]
        
        this_car_df <- this_car_df[input$time:(input$time+step_const) ,]
        
        thisdf <- rbind.data.frame(thisdf, this_car_df)
      }
      
      thisdf
      
    })
    
    output$mymap <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng = mean(df$longitude), lat =mean(df$latitude),  zoom = 13) #%>%
        #addTiles()
      
    })
    
    observe({
      
      plotdf <- points()
      #linesdf <- df %>% group_by(carid) %>% filter(row_number()==1 | row_number()==n())
      
      
      leafletProxy("mymap") %>%
        #addProviderTiles("CartoDB.DarkMatter") %>%
        clearMarkers() %>%
        #addCircles(data = plotdf, opacity = 5, weight = 2, color = ~pal(carid)) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV1",]$latitude, lng = plotdf[plotdf$carid == "EV1",]$longitude, color = pal('EV1'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV2",]$latitude, lng = plotdf[plotdf$carid == "EV2",]$longitude, color = pal('EV3'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV3",]$latitude, lng = plotdf[plotdf$carid == "EV3",]$longitude, color = pal('EV3'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV4",]$latitude, lng = plotdf[plotdf$carid == "EV4",]$longitude, color = pal('EV4'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV5",]$latitude, lng = plotdf[plotdf$carid == "EV5",]$longitude, color = pal('EV5'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV6",]$latitude, lng = plotdf[plotdf$carid == "EV6",]$longitude, color = pal('EV6'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV7",]$latitude, lng = plotdf[plotdf$carid == "EV7",]$longitude, color = pal('EV7'), weight = 3, opacity = 5) %>%
        addPolylines(lat = plotdf[plotdf$carid == "EV8",]$latitude, lng = plotdf[plotdf$carid == "EV8",]$longitude, color = pal('EV8'), weight = 3, opacity = 5) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV1",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV2",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV3",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV4",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV5",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV6",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV7",], 1), icon = carIcon) %>%
        addMarkers(data = tail(plotdf[plotdf$carid == "EV8",], 1), icon = busIcon)
      
      
      
      #' df_1 = points() %>%
      #'   filter(carid == "EV1")
      #' 
      #' inx_1 <- max(1,nrow(df_1))
      #' if (inx_1 <= 1) df_1 <- df_1_last else df_1_last <<- df_1
      #' 
      #' #'#############
      #' 
      #' df_2 = points() %>%
      #'   filter(carid == "EV2")
      #' 
      #' inx_2 <- max(1,nrow(df_2))
      #' if(inx_2 <= 1) df_2 = df_2_last else df_2_last <<- df_2
      #' 
      #' #'#############
      #' 
      #' df_3 = points() %>%
      #'   filter(carid == "EV3")
      #' 
      #' inx_3 <- max(1,nrow(df_3))
      #' if(inx_3 <= 1) df_3 = df_3_last else df_3_last <<- df_3
      #' 
      #' #'#############
      #' 
      #' df_4 = points() %>%
      #'   filter(carid == "EV4")
      #' 
      #' inx_4 <- max(1,nrow(df_4))
      #' if(inx_4 <= 1) df_4 = df_4_last else df_4_last <<- df_4
      #' 
      #' #'#############
      #' 
      #' df_5 = points() %>%
      #'   filter(carid == "EV5")
      #' 
      #' inx_5 <- max(1,nrow(df_5))
      #' if(inx_5 <= 1) df_5 = df_5_last else df_5_last <<- df_5
      #' 
      #' #'#############
      #' 
      #' df_6 = points() %>%
      #'   filter(carid == "EV6")
      #' 
      #' inx_6 <- max(1,nrow(df_6))
      #' if(inx_6 <= 1) df_6 = df_6_last else df_6_last <<- df_6
      #' 
      #' #'#############
      #' 
      #' df_7 = points() %>%
      #'   filter(carid == "EV7")
      #' 
      #' inx_7 <- max(1,nrow(df_7))
      #' if(inx_7 <= 1) df_7 = df_7_last else df_7_last <<- df_7
      #' 
      #' #'#############
      #' 
      #' df_8 = points() %>%
      #'   filter(carid == "EV8")
      #' 
      #' inx_8 <- max(1,nrow(df_8))
      #' if(inx_8 <= 1) df_8 = df_8_last else df_8_last <<- df_8
      #' 
      #' 
      #' print(c(inx_1, inx_2, inx_3, inx_4, inx_5, inx_6, inx_7, inx_8))
      
      
      
      # pal = colorFactor(palette = "Dark2", domain = r$alternative_id)
      # leaflet() %>% 
      #   addProviderTiles("CartoDB.DarkMatter") %>%
      #   addPolylines(data = r, opacity = 1, weight = 7, color = ~pal(alternative_id))
      
      
      
      # leafletProxy("mymap") %>%
      #   #addProviderTiles("CartoDB.DarkMatter") %>%
      #   clearShapes() %>%
      #   clearMarkers() %>%
      #   #addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>%
      #   addCircles(data = df_1, color = 'red', radius = 5) %>%
      #   addCircles(data = df_2, color = 'yellow', radius = 5) %>%
      #   addCircles(data = df_3, color = 'green', radius = 5) %>%
      #   addCircles(data = df_4, color = 'black', radius = 5) %>%
      #   addCircles(data = df_5, color = 'blue', radius = 5) %>%
      #   addCircles(data = df_6, color = 'grey', radius = 5) %>%
      #   addCircles(data = df_7, color = 'purple', radius = 5) %>%
      #   addCircles(data = df_8, color = 'brown', radius = 5) %>%
      #   addMarkers(data = df_1[inx_1,], icon = carIcon) %>%
      #   addMarkers(data = df_2[inx_2,], icon = carIcon) %>%
      #   addMarkers(data = df_3[inx_3,], icon = carIcon) %>%
      #   addMarkers(data = df_4[inx_4,], icon = carIcon) %>%
      #   addMarkers(data = df_5[inx_5,], icon = carIcon) %>%
      #   addMarkers(data = df_6[inx_6,], icon = carIcon) %>%
      #   addMarkers(data = df_7[inx_7,], icon = carIcon) %>%
      #   addMarkers(data = df_8[inx_8,], icon = busIcon) 
      
      #addTiles() %>%
      # addPolylines(lat = df_1$latitude, lng = df_1$longitude, color = 'red', weight = 5 ) %>%
      # addPolylines(lat = df_2$latitude, lng = df_2$longitude, color = 'green', weight = 5) %>%
      # addPolylines(lat = df_3$latitude, lng = df_3$longitude, color = 'blue', weight = 5) %>%
      # addPolylines(lat = df_4$latitude, lng = df_4$longitude, color = 'black', weight = 5) %>%
      # addPolylines(lat = df_5$latitude, lng = df_5$longitude, color = 'yellow', weight = 5) %>%
      # addPolylines(lat = df_6$latitude, lng = df_6$longitude, color = 'grey', weight = 5) %>%
      # addPolylines(lat = df_7$latitude, lng = df_7$longitude, color = 'purple', weight = 5) %>%
      # addPolylines(lat = df_8$latitude, lng = df_8$longitude, color = 'brown', weight = 5)
      
      
    })
    
    
  }
  
  
)
