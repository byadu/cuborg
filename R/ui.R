shinyUI(
	dashboardPagePlus(skin='purple',
		sidebar_fullCollapse=T,
		header=dashboardHeaderPlus(title = 'My Reports and Dashboards', titleWidth=350, enable_rightsidebar = TRUE, rightSidebarIcon = "filter"),
		sidebar=dashboardSidebar(width=350, radioGroupButtons(inputId="rdi", choices=c("Reports", "Data", "Insights"), justified=T, status="primary"), sidebarMenuOutput('sidemenu')),
	#	sidebar=dashboardSidebar(width=350, sidebarMenuOutput('sidemenu')),
	
		body=dashboardBody(uiOutput('mainbody')), 
		rightsidebar=rightSidebar(background='light', width=400, rightSidebarTabContent(id=1, icon='', active=T, title="Apply or Create a Filter On:",uiOutput('filtersmain'))),
    #	footer = dashboardFooter( left_text = "cuborg.io", right_text = "v2020.06")
    	footer = NULL
		)
	)
