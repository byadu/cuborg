output$sidemenu<- renderMenu({
	m1<- repside() 
	m2<- dashside()
	m3<- menuItem( "Explorer", menuSubItem("Create Reports", tabName="BI"))
	m6<- menuItem( "Insights", menuSubItem("Analytics", tabName="AI"))
	m4<- menuItem( "Data Model", menuSubItem(" Table Joins", tabName="Joins"), menuSubItem("Metrics and Attributes", tabName="MD"))
	m5<- menuItem( "Data Ingestion", menuSubItem("From Excel", tabName="fromxl"), menuSubItem("From Database", tabName="fromdb")) 
 	sbs<- sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label="Search")
	user<- sidebarUserPanel( span("Logged in as ", uid), subtitle = a(icon("sign-out"), "Logout", href="__logout__"))

#	req(input$rdi)
#	if(input$rdi=="Data")
#		sidebarMenu(m4,m5)
#	else if(input$rdi == "Reports")
		sidebarMenu(m3, do.call(menuItem, m1), do.call(menuItem, m2))
#	else if(input$rdi=="Insights")
#		sidebarMenu(m6)
	})

output$analyze<- renderUI({
	a<-analyzeUI(ns('main'))
	callModule(analyze, 'main')
	a
	})

output$dmodel<- renderUI({
	a<-dmodelUI(ns('main'))
	callModule(dmodel, 'main')
	a
	})

output$metrics<- renderUI({
	a<-cfgmetricsUI(ns('main-metrics'))
	callModule(cfgmetrics, 'main-metrics')
	a
	})
output$attributes<- renderUI({
	a<-cfgattribsUI(ns('main-attribs'))
	callModule(cfgattribs, 'main-attribs')
	a
	})
output$menudtls<- renderUI({
	tabsetPanel(
		tabPanel("Metrics", uiOutput('metrics')),
		tabPanel("Attributes", uiOutput('attributes'))
		)
	})

output$fromdb<- renderUI({
	a<-addtablesUI(ns('main'))
	callModule(addtables, 'main')
	a
	})

output$fromxl<- renderUI({
	a<-addxlUI(ns('main'))
	callModule(addxl, 'main')
	a
	})

output$distPlot <- renderPlot({
  x    <- faithful[, 2]  # Old Faithful Geyser data
  bins <- seq(min(x), max(x), length.out = 10 + 1)
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

output$mainbody<- renderUI({
	t1<- dashboards()
	t2<- reportscharts()
	t3<- list(); t3[[1]]<- tabItem(tabName="BI", uiOutput('analyze'))
	t4<- list(); t4[[1]]<- tabItem(tabName="AI", plotOutput("distPlot"))
	t5<- list(); t5[[1]]<- tabItem(tabName="Joins", uiOutput("dmodel"))
	t6<- list(); t6[[1]]<- tabItem(tabName="MD", uiOutput("menudtls"))
	t7<- list(); t7[[1]]<- tabItem(tabName="fromdb", uiOutput("fromdb"))
	t8<- list(); t8[[1]]<- tabItem(tabName="fromxl", uiOutput("fromxl"))
	do.call(tabItems, c(t1,t2,t3,t4,t5,t6,t7,t8))
	})

output$filtersmain<- renderUI({
	req(xr$dimgrp)
	callModule(filters, 'filters')
	filtersUI(ns('filters'), xr$dimgrp)
	})
