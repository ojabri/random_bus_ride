library(shiny)

shinyServer(function(input, output) {

 points <- eventReactive( c(input$n,input$update, input$w) , {
 
 a = random.bus.ride(random.walk.max = input$w, random.sit.max = input$n)
 b = merge(a$stops, bus.stops)
 c = convertToLatLong(b$Location_Easting, b$Location_Northing)
 b = cbind(b, c)
 b = b[order(b$order),]
 
 list("seq" = b, "summary" = a$summary)
 }, ignoreNULL = FALSE)

 output$plot1 <- renderLeaflet({

 pal <- colorFactor(c("blue", "black", "red"), domain = c("start", "intermediate", "end"))
 
 leaflet() %>%
  addProviderTiles("Stamen.TonerLite",
    options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addCircleMarkers(
   ~Longitude, ~Latitude,
   radius = ~ifelse(type=="intermediate", 3,5),
   color = ~pal(type),
   stroke = FALSE, fillOpacity = 0.25,
   popup = ~Stop_Name,
   data = points()$seq ) %>%
  #addPolylines(~Longitude, ~Latitude, data=points() ) %>%
  #addPolygons(~Longitude, ~Latitude, data = wc.chull, color="red") %>%
  addPolygons(~Longitude, ~Latitude, data = centralLondonPostcodes.chull, color="red") #%>%
  #addMarkers(~Longitude, ~Latitude, data=post.codes.central, clusterOptions = markerClusterOptions(), popup = ~Postcode )

 })

 output$text <- renderDataTable({
  subset(points()$seq, type!="intermediate")[c("order", "Stop_Code_LBSL", "Stop_Name", "type")]
  #points()$summary
 })
  
})

