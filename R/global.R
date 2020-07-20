library(modgetxl)
library(modchart)
library(modcubmodel)
library(modcubingest)
library(libcubmeta)
library(libcubolap)
library(modcubqry)
library(modcubfilt)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(RMySQL)
library(lazyeval)
library(dplyr)
library(reshape2)
library(DT)
uid<-NULL

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
