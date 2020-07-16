library(shiny)
library(htmltools)
library(leaflet)

htmlTemplate("blog_single2.html",
             #before=h2("here"),
             store = plotlyOutput("store"),
             shinyplot= plotlyOutput("plotly"),
             shinymap=leafletOutput('leafletmap'),
             heatmap = d3heatmapOutput("hm"),
             dygraph = dygraphOutput("dg"), 
             fcast = plotlyOutput("fc"),
             donut = sunburstOutput("donut"),
             invent = plotlyOutput("inventory")
             )
    
  