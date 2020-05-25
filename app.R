library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(tidyr)
library(class)
library(lubridate)
library(dplyr)
library(ggpubr)
library(gganimate)
library(shinythemes)

#data import
taxi <- readRDS("taxi.Rds")
landmark <- vroom::vroom("https://data.cityofchicago.org/resource/tdab-kixi.csv") %>% 
  mutate_each(function(x){ifelse(is.na(x),"Unknown",x)})

weather <- vroom::vroom("https://data.cityofchicago.org/resource/yw6r-xr7g.csv?$limit=1000000") %>% 
  select(measurement_timestamp, air_temperature) %>% 
  mutate(measurement_timestamp = str_remove_all(measurement_timestamp,"\\s.*")) %>% 
  group_by(measurement_timestamp) %>% 
  summarise(air_temperature = mean(air_temperature))

#UI
ui = navbarPage("Chicago Taxi Rides 2016",
                
                tags$head(div(img(src = "image/chicago.png",height=180, width = 1500), 
                              style="text-align: center;")),
                
                theme = shinytheme("superhero"),
                
                tabPanel("Introduction", 
                         titlePanel(img(src  ="image/taxi.png", height=300, width = 800)),
                         includeMarkdown("intro.rmd")),
                
                tabPanel("Frequency Map",
                         absolutePanel(
                           id = "controls_freq", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = "auto", left = "auto", right = 180, bottom = "auto",
                           width = 300, height = "auto", style = "opacity: 0.8; z-index: 10;",
                           
                           selectInput("day_freq", label = "Day of the Week", 
                                       choices = list("Monday" = "Monday", "Tuesday" = "Tuesday", 
                                                      "Wednesday" = "Wednesday","Thursday" = "Thursday", 
                                                      "Friday" = "Friday","Saturday" = "Saturday",
                                                      "Sunday" = "Sunday"), selected = "Friday"),
                           
                           selectInput("month_freq", label = "Month", 
                                       choices = c("Jan"=1,"Feb"=2,"Mar"=3,"Apr"=4,"May"=5,"Jun"=6,
                                                   "Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12),
                                       selected = "Jan"),
                           
                           selectInput("time_freq", label = "Time of the Day", 
                                       choices = list("0AM-3AM" = "0-2", 
                                                      "3AM-6AM" = "3-5",
                                                      "6AM-9AM" = "6-8",
                                                      "9AM-12AM" = "9-11", 
                                                      "12PM-3PM" = "12-14", 
                                                      "3PM-6PM" = "15-17",
                                                      "6PM-9PM" = "18-20", 
                                                      "9PM-12AM" = "21-23"), selected = "0AM-3AM")
                           
                         ),
                         tabBox(width = 12,
                                tabPanel(title = "Normal",id = "normal_map", 
                                         tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                         leafletOutput(outputId = "map")),
                                tabPanel(title = "Heat",id = "heat_map", 
                                         tags$style(type = "text/css", "#heatmap {height: calc(100vh - 80px) !important;}"),
                                         leafletOutput(outputId = "heatmap"))
                         )
                ),
                tabPanel("Visitor Map",
                         absolutePanel(
                           id = "controls_vis", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = "auto", left = "auto", right = 50, bottom = "auto",
                           width = 300, height = "auto",style = "opacity: 0.8; z-index: 10;",
                           
                           selectInput("day_vis", label = "Day of the Week", 
                                       choices = list("Monday" = "Monday", "Tuesday" = "Tuesday", 
                                                      "Wednesday" = "Wednesday","Thursday" = "Thursday", 
                                                      "Friday" = "Friday","Saturday" = "Saturday",
                                                      "Sunday" = "Sunday"), selected = "Friday"),
                           
                           selectInput("month_vis", label = "Month", 
                                       choices = c("Jan"=1,"Feb"=2,"Mar"=3,"Apr"=4,"May"=5,"Jun"=6,
                                                   "Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12),
                                       selected = "Jan"),
                           
                           selectInput("time_vis", label = "Time of the Day", 
                                       choices = list("0AM-3AM" = "0-2", 
                                                      "3AM-6AM" = "3-5",
                                                      "6AM-9AM" = "6-8",
                                                      "9AM-12AM" = "9-11", 
                                                      "12PM-3PM" = "12-14", 
                                                      "3PM-6PM" = "15-17",
                                                      "6PM-9PM" = "18-20", 
                                                      "9PM-12AM" = "21-23"), selected = "0AM-3AM"),
                           checkboxGroupInput("pop_vis", label = "Popularity of Landmark",
                                              choices = c("Most Popular" = "red","Popular" = "orange", 
                                                          "Little Popular" = "green","Not Popular" = "blue"),
                                              selected = c("red","orange","green","blue"))
                         ),
                         
                         tags$style(type = "text/css", "#tourmap {height: calc(100vh - 80px) !important;}"),
                         leafletOutput(outputId = "tourmap")
                ),
                tabPanel("Diagrams",
                         sidebarLayout(
                           sidebarPanel(
                             
                             selectInput("color", label = "Color of the Graph", 
                                         choices = c("Orange", "Purple", "Pink", "Blue"), selected = "Pink"),
                             
                             selectInput("month", label = "Month", 
                                         choices = c("Jan"=1,"Feb"=2,"Mar"=3,"Apr"=4,"May"=5,"Jun"=6,
                                                     "Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12),
                                         selected = "Jan"),
                             em(h5("Warning: It would take a minute for all plots to show up 
                               according to your selection.", style = "color:#B0C4DE"))
                           ),
                           mainPanel(
                             fluidRow(column(6, plotOutput("hist")),
                                      column(6, plotOutput("hist2"))),
                             br(),
                             plotOutput("month_temper")
                           )
                         ))
)

server = function(input, output, session) {
  
  #subset data according to time and month date - freq
  taxi_freq <- reactive({
    time1 <- as.integer(unlist(str_split(input$time_freq,"-"))[1])
    time2 <- as.integer(unlist(str_split(input$time_freq,"-"))[2])
    result <- taxi %>% 
      filter(start_hour>=time1, start_hour<=time2, month(trip_start_timestamp)== input$month_freq,
             weekdays(trip_start_timestamp) == input$day_freq)
    return(result)
  })
  
  #count the number of subset of data
  taxi_count <- reactive(paste0("taxi use from ",unlist(str_split(input$time_freq,"-"))[1]," to ",
                                unlist(str_split(input$time_freq,"-"))[2]," is  ",
                                "<span style='color: #ff0000;'><strong>",
                                count(taxi_freq())))
  
  #subset data according to time and month date - freq
  taxi_vis <- reactive({
    time1 <- as.integer(unlist(str_split(input$time_vis,"-"))[1])
    time2 <- as.integer(unlist(str_split(input$time_vis,"-"))[2])
    result <- taxi %>% 
      filter(start_hour>=time1, start_hour<=time2, month(trip_start_timestamp)== input$month_vis,
             weekdays(trip_start_timestamp) == input$day_vis)
    return(result)
  })
  
  taxi_vis_land <- reactive({
    data.frame(destination = knn(train = landmark[,c("latitude","longitude")], 
                                 test = taxi_vis()[,c("dropoff_centroid_latitude", "dropoff_centroid_longitude")],
                                 k=1, cl = landmark$landmark_name)) %>% 
      group_by(destination) %>% 
      summarise(count = n()) %>% 
      left_join(landmark, by = c("destination" = "landmark_name"))
  })
  
  get_pop <- function(data){
    sapply(data$count,function(x){
      if(x < dim(taxi_vis())[1]/500){
        "blue"
      } else if(x < dim(taxi_vis())[1]/100){
        "green"
      } else if(x < dim(taxi_vis())[1]/50){
        "orange"
      } else "red"
    })
  }
  
  taxi_vis_land_pop <- reactive({
    taxi_vis_land() %>% 
      filter(get_pop(taxi_vis_land()) %in% input$pop_vis)
  })
  
  myicon <- reactive(makeAwesomeIcon(
    icon = "star",
    iconColor = "white",
    markerColor = get_pop(taxi_vis_land_pop())
  ))
  
  icon_legend <- paste0(img(src = "image/red.png", height="22", width="18" )," Most Popular",
                        "<br>",img(src = "image/orange.png",height="22", width="17")," Popular",
                        "<br>",img(src = "image/green.png",height="22", width="16")," Little Popular",
                        "<br>",img(src = "image/blue.png",height="22", width="17")," Not Popular")
  
  output$map <- renderLeaflet({
    leaflet(data = taxi_freq()) %>% 
      setView(lat = 41.881832, lng = -87.623177, zoom = 11) %>%
      addTiles() %>%
      addMarkers(~dropoff_centroid_longitude, ~dropoff_centroid_latitude,
                 popup = paste0(
                   "<span style='color: #7f0000'><strong>Description of trip</strong></span>",
                   "<br><span style='color: salmon;'><strong>Trip time: </strong></span>", 
                   taxi_freq()$trip_seconds, 
                   "<br><span style='color: salmon;'><strong>Trip mile: </strong></span>", 
                   taxi_freq()$trip_miles,
                   "<br><span style='color: salmon;'><strong>Fare: </strong></span>", 
                   taxi_freq()$fare), 
                 clusterOptions = markerClusterOptions()) %>% 
      addControl(html = taxi_count(), position = "bottomright") %>% 
      addMiniMap(position = 'topright',
                 zoomLevelOffset = -4,
                 zoomAnimation = TRUE)
  })
  
  output$heatmap <- renderLeaflet(
    leaflet(data = taxi_freq()) %>% 
      setView(lat = 41.881832, lng = -87.623177, zoom = 11) %>%
      addTiles() %>%
      addHeatmap(lng = ~dropoff_centroid_longitude, lat = ~dropoff_centroid_latitude,
                 blur = 20, max = 0.05, radius = 10)
  )
  
  output$tourmap <- renderLeaflet({
    leaflet(data = taxi_vis_land_pop()) %>% 
      setView(lat = 41.881832, lng = -87.623177, zoom = 13) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude,
                        popup = paste0(
                          "<span style='color: #7f0000'><strong>Description Landmark</strong></span>",
                          "<br><span style='color: salmon;'><strong>Name: </strong></span>", 
                          taxi_vis_land_pop()$destination, 
                          "<br><span style='color: salmon;'><strong>Address: </strong></span>", 
                          taxi_vis_land_pop()$address,
                          "<br><span style='color: salmon;'><strong>Built at: </strong></span>", 
                          taxi_vis_land_pop()$date_built,
                          "<br><span style='color: salmon;'><strong>Architect: </strong></span>", 
                          taxi_vis_land_pop()$architect,
                          "<br><span style='color: salmon;'><strong>Visitor: </strong></span>", 
                          taxi_vis_land_pop()$count
                        ),
                        icon = myicon()
      ) %>% 
      addControl(html = icon_legend, position = "bottomleft")
  })
  output$hist <- renderPlot({
    fill_color <- switch(input$color, "Orange" = "#D2691E", "Purple" = "#bebada", "Pink" = "#F08080", 
                         "Blue" = "#87CEEB")
    
    taxi$start_day = factor(taxi$start_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                       "Friday", "Saturday", "Sunday"))
    taxi = taxi %>% mutate(month = month(as.POSIXct(trip_start_timestamp, format = "%Y-%m-%d")))
    
    taxi %>% filter(month == input$month) %>% 
      ggplot(taxi, mapping = aes(x = start_day)) + geom_bar(fill = fill_color, alpha = 0.5) + 
      geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 1.05)) +
      labs(x = "", y = "") + theme(panel.background = element_rect(fill = '#F0F8FF'),
                                   panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank(),
                                   plot.background = element_rect(fill = "#F0F8FF"),
                                   axis.line = element_line(colour = "#778899"))
  })
  
  output$hist2 <- renderPlot({
    fill_color <- switch(input$color, "Orange" = "#D2691E", "Purple" = "#bebada", "Pink" = "#F08080", 
                         "Blue" = "#87CEEB")
    
    taxi$start_day = factor(taxi$start_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                       "Friday", "Saturday", "Sunday"))
    taxi = taxi %>% mutate(month = month(as.POSIXct(trip_start_timestamp, format = "%Y-%m-%d")))
    
    taxi %>% filter(month == input$month) %>% 
      ggplot(taxi, mapping = aes(x = start_hour)) + geom_bar(fill = fill_color, alpha = 0.5) + 
      geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 1.1)) +
      labs(x = "", y = "") + theme(panel.background = element_rect(fill = '#F0F8FF'),
                                   panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank(),
                                   plot.background = element_rect(fill = "#F0F8FF"),
                                   axis.line = element_line(colour = "#778899")) +
      annotate("text", x = c(2,6,10,14,18,22) , y = 0, 
               label = c("0-4AM", "4-8AM", "8-12AM", "12-4PM", "4-8PM", "9-12PM"))
  })
  
  # weather data
  weather = weather %>% mutate(month = month(as.POSIXct(measurement_timestamp, format = "%Y-%m-%d")))
  weather$measurement_timestamp = as.Date(weather$measurement_timestamp, format = "%Y-%m-%d")
  
  output$month_temper <- renderImage({
    fill_color <- switch(input$color, "Orange" = "#D2691E", "Purple" = "#bebada", "Pink" = "#F08080", 
                         "Blue" = "#87CEEB")
    
    taxi_wthr <- taxi %>% filter(month(trip_start_timestamp) == input$month) %>% 
      group_by(date = as.Date(trip_start_timestamp)) %>% 
      summarise(count = n())
    colnames(taxi_wthr)[1] = "measurement_timestamp"
    
    outfile <- tempfile(fileext = '.gif')
    
    cols <- c("Air temperature" = fill_color, "Taxi Usage" = "blue")
    p <- weather %>% filter(month == input$month) %>% 
      ggplot(weather, mapping = aes(measurement_timestamp, air_temperature)) + 
      geom_line(aes(color = "Air temperature")) + geom_point(aes(color = "Air temperature")) + 
      geom_line(data = taxi_wthr, aes(x = measurement_timestamp, y = count/1000, color = "Taxi Usage")) +
      geom_point(data = taxi_wthr, aes(x = measurement_timestamp, y = count/1000,color = "Taxi Usage")) +
      labs(x = "Date", y = "Air Temperature") +
      scale_color_manual(name = "line", values = cols) +
      scale_x_date(name = "Date", date_breaks = "week") +
      scale_y_continuous(name = "Air Temperature",
                         sec.axis = sec_axis(~.*1000, name = "Taxi Usage",
                                             labels = function(x){paste0(x/1000,"k")})) +
      transition_reveal(measurement_timestamp) + theme(panel.background = element_rect(fill = '#F0F8FF'),
                                                       panel.grid.minor = element_blank(),
                                                       panel.grid.major = element_blank(),
                                                       plot.background = element_rect(fill = "#F0F8FF"),
                                                       legend.background = element_rect(fill = "#F0F8FF"),
                                                       axis.line = element_line(colour = "#778899"))
    
    animate(p, width = 800, height = 400)
    anim_save("outfile.gif")
    
    list(src = "outfile.gif", contentType = 'image/gif')
  }, deleteFile = TRUE)
}

shinyApp(ui = ui, server = server)
