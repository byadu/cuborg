library(modgetxl)
library(modchart)
library(libcubmeta)
library(libcubolap)
library(shiny)
library(shinyWidgets)
#library(shinyTree)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(RMySQL)
#library(jsonlite)
#library(sunburstR)
#library(plotly)
#library(rpivotTable)
#library(DT)
#library(rgdal)
#library(leaflet)
#library(RColorBrewer)
#library(treemap)
#library(chorddiag)
#library(highcharter)
#library(dygraphs)
#library(networkD3)
#library(sparkline)
#library(dplyr)
#
#library(lazyeval)
#library(radarchart)
#library(reshape2)
#library(lubridate)
#library(xts)
#library(tidyr)
#library(d3heatmap)
#library(data.table)
#library(readxl)
uid<-NULL
#source("../lib/libs.R")

#convertMenuItem <- function(mi,tabName) {
   # mi$children[[1]]$attribs['data-toggle']="tab"
   # mi$children[[1]]$attribs['data-value'] = tabName
   # mi
#	}
basicPanel <- function(..., width = 12) {
	column(
		width = width,
		tags$div(
			class="panel panel-default", style="border: 1px solid; border-color: gray;",
			tags$div(
				class="panel-body",
				...
				)
			)
		)
	}
