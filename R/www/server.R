

shinyServer(function(input, output, session) {
  
  
 output$leafletmap <- renderLeaflet({
    
   uk_map <- leaflet()%>%addProviderTiles("Esri.OceanBasemap")%>%setView(lat= 54.344, lng=-1.971 , zoom=5) %>% addPolygons(data= shape ,stroke=NULL, color= ~col(nos), fillOpacity=0.7, popup= ~paste(NAME2, ":", round(nos), "Million")) %>%addCircles(data=city, weight= ~Population/100000, color = ~col2(Population), popup= ~paste(Cities..towns..districts,":", Population), fillOpacity = 0.9)
   uk_map
   
  })

 output$plotly <- renderPlotly({
   plot
 })
 
 output$hm <- renderD3heatmap({
   
   d3 <- d3heatmap(mtcars2, scale = "column", colors = "Blues", dendrogram = "none", symm = TRUE, xaxis_height = 150, yaxis_width = 150, xaxis_font_size = 6, yaxis_font_size = 7, show_grid = FALSE)
   return(d3)
 })
 
 output$dg <- renderDygraph({
   dygraph(lungDeaths)%>%dyRangeSelector()%>%dyRoller(rollPeriod = 5)%>%dyOptions(stackedGraph=TRUE)
   
 })
 
 output$fc <- renderPlotly({
   ggplotly(salesfc2)
   
 })
 
 output$store <- renderPlotly({
  plot_ly(data=storesales, y=~profit, x=~store_no, type='box') %>% layout(xaxis= list(title= "Store ID"), yaxis= list(title= "Profit (Mil USD)"), showlegend=FALSE)
   
 })
 
 output$donut <- renderSunburst({
   sunburst(donutdata, legend=list (w= 0,h =0, s=0,t=0)) 
   })
 
 output$inventory <- renderPlotly({
   
   plot_ly(si, x=~Product, y=~`Inventory Turnover`, color=~Division, type='bar', colors=c('#d01c8b', '#f1b6da', '#4dac26'))#, hoverinfo='text', text=paste(si$Division,":", si$`Inventory Turnover`))#, ",", si$Dept, ",", si$Class, ",", si$Subclass))
   
 })
 
})
  