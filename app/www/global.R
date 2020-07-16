library(rgdal)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(sp)
library(networkD3)
library(d3heatmap)
library(dygraphs)
library(sunburstR)
library(xts)
library(lubridate)
library(forecast)
library(dygraphs)
library(ggfortify)
library(plotly)

#load(file="../../Documents/ukmap.RData")
#load(file="../../Documents/forecast.RData")
#load(file="../../Documents/boxplot_store.RData")
#load(file="../../Documents/inven/inv.RData")

#load(file="ukmap.RData")
load(file="shape.RData")

load(file="ukcities.RData")

load(file="forecast.RData")
load(file="forecast2.RData")
load(file="storesales.RData")
load(file="boxplot_store.RData")
load(file="inv.RData")

f <- list(
  family = "Calibri",
  size = 14,
  color = "#7f7f7f"
)
x <- list(
  title = "Total Spend",
  titlefont = f
)
y <- list(
  title = "Days since Acquisition",
  titlefont = f
)

y2 <- list(
  title = "Visit Frequency",
  titlefont = f
  
)

y3 <- list(
  title = "Recency",
  titlefont = f, 
  overlaying="y",
  side="right", 
  range= c(0,5),
  zeroline= FALSE
)




a <- list()
for (i in 1:4) {
  if (i==1){
    a[[i]] <- list(
      x = 10,
      y = 550,
      text = "Potential",
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      arrowhead = 7
      #ax = 20,
      #ay = -40
    )
    
  }
  
  if (i==2){
    a[[i]] <- list(
      x = 33,
      y = 550,
      text = "Stars",
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      arrowhead = 7
      #ax = 20,
      #ay = -40
    )
    
  }
  
  if (i==3){
    a[[i]] <- list(
      x = 33,
      y = 40,
      text = "Cash Cows",
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      arrowhead = 7
      #ax = 20,
      #ay = -40
    )
    
  }
  
  if (i==4){
    a[[i]] <- list(
      x = 10,
      y = 40,
      text = "Dogs",
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      arrowhead = 7
      #ax = 20,
      #ay = -40
    )
    
  }
}

someids <- as.integer(abs(rnorm(1:32))*3300)



plot <- plot_ly(data=mtcars, x = ~mpg, y = ~disp , type="scatter", text = ~paste("Customer ID:",someids) , mode = "markers", name="") %>%
  add_trace(x = c(~mean(mpg), ~mean(mpg)), y= c(0, ~(max(disp))*1.15), mode = "lines") %>%  add_trace(x = c(0,~(max(mpg))*1.25), y= c(~mean(disp),~mean(disp)), mode = "lines") %>%
  layout(xaxis = ~x, yaxis = ~y) %>% layout(annotations= ~a) %>% layout(showlegend=FALSE)



lungDeaths <- cbind(mdeaths, fdeaths)
dimnames(lungDeaths)[[2]] <- c("Sales", "Profits")


mtcars2 <- mtcars[1:11,]
rownames(mtcars2) <- c("Potato Chips", "Soft Drinks", "Alcoholic Beverages", "Whole Milk", "Chicken", "Root Vegetables", "Cheese", "Bread", "Citrus Fruits", "Exotic Fruits" ,"Other Snack")
colnames(mtcars2) <- c("Detergent", "Leafy Vegetable", "Yogurt", "Red Meat", "Chocolate", "Nuts", "Processed Meats", "Biscuits", "Condiments", "Frozen Meat" ,"Seafood")

mtcars2[4,] <- mtcars2[4,]*2


donuts <- read.csv("donuts.csv", stringsAsFactors = FALSE)
dont <- paste(donuts[[1]], donuts[[2]], donuts[[3]], donuts[[4]], sep = "-")
donutdata <- data.frame(dont, values= rnorm(1:48, mean= 500))

#invsubset <- subset(invall, select= c("Avg_Stock", "Revenue"))
col <- colorNumeric("PiYG", shape$nos)
col2 <- colorNumeric("PuOr", city$Population)

