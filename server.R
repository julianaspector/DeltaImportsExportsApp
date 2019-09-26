server<-function(input,output,session){
  
  #Render the sliders
  output$sliders <- renderUI({
    # First, create a list of sliders each with a different name
    sliders <- lapply(1:length(xAxisGroup), function(i) {
      inputName <- xAxisGroup[i]
      sliderInput(inputName, inputName, min=as.Date(data_minDate$V1[i]), max=as.Date(data_maxDate$V1[i]), value=as.Date(data_minDate$V1[i]), timeFormat="%b %Y")
    })
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders)
  })
  
  # subset the data based on user choice
  dataInput_one <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 1")) & imports_exports$WYT== 1,]
  })
  dataInput_two <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 2")) & imports_exports$WYT== 2,]
  })
  dataInput_three <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 3")) & imports_exports$WYT== 3,]
  })
  dataInput_four <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 4")) & imports_exports$WYT== 4,]
  })
  dataInput_five <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 5")) & imports_exports$WYT== 5,]
  })
  
  # render basemap and legend for each water year type (static)

  output$map_one <- renderLeaflet({
        leaflet(data) %>% addTiles() %>%
        fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat))%>%
        addLegend(data=dataInput_one(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 1")
      })
  
  output$map_two <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
    fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
    addLegend(data=dataInput_two(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 2")
  })
  
  output$map_three <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_three(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 3")
  })
  output$map_four <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_four(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 4")
  })
  output$map_five <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_five(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 5")
  })

# color palette for maps
pal <- colorFactor(palette=c("#ef8a62", "#67a9cf"), levels=sort(imports_exports$Label))

# interactive data for maps
observe({
  map<-leafletProxy("map_one")%>%
  clearShapes() %>%
  addCircles(data=dataInput_one(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                          "<br>", "Flow:", as.character(Flow), "<br>", "Units:", as.character(Units)))
})

observe({
  map<-leafletProxy("map_two")%>%
    clearShapes() %>%
    addCircles(data=dataInput_two(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                  "<br>", "Flow:", as.character(Flow),
                                                                                  "<br>", "Units:", as.character(Units)))
})
observe({
  map<-leafletProxy("map_three")%>%
    clearShapes() %>%
    addCircles(data=dataInput_three(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                  "<br>", "Flow:", as.character(Flow),
                                                                                  "<br>", "Units:", as.character(Units)))  
})
observe({
  map<-leafletProxy("map_four")%>%
    clearShapes() %>%
    addCircles(data=dataInput_four(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                  "<br>", "Flow:", as.character(Flow),
                                                                                  "<br>", "Units:", as.character(Units)))
})
observe({
  map<-leafletProxy("map_five")%>%
    clearShapes() %>%
    addCircles(data=dataInput_five(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                  "<br>", "Flow:", as.character(Flow),
                                                                                  "<br>", "Units:", as.character(Units)))
    
})

output$table_one <- renderTable({
  head(wyTypes_one, n = 23)}, bordered=TRUE, digits=0)

output$table_two <- renderTable({
  tail(wyTypes_one, n = -23)}, bordered=TRUE, digits=0)

output$table_three <- renderTable({
  head(wyTypes_two, n = 23)}, bordered=TRUE, digits=0)

output$table_four <- renderTable({
  tail(wyTypes_two, n = -23)}, bordered=TRUE, digits=0)

}