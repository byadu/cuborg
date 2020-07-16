saveobservers<-0
savereport<- function(input, output, session, g, currfilt) {
	ns<- session$ns

	if(!saveobservers) {
	observeEvent(input$saverep, ignoreInit=T,{
		repname<- isolate(input$repname)
		foldid<- isolate(input$parfold)
		if(is.na(as.numeric(foldid)))
			foldid<- addfolder(foldid, uid, M)
		else
			foldid<- as.numeric(foldid)
		gfid<- addgraph(repname, foldid, g, currfilt, M)
		rr$repchanged<- isolate(rr$repchanged)+1
		createAlert(session, ns("saved"), ns("alsaved"), title="", content=paste("Report", strong(repname), "saved in folder", strong(foldername(M$cfg, foldid))))
		})
	saveobservers<<- 1
	}
	}

savereportUI<- function(id, ft) {
	ns<- NS(id)

	fluidPage(
		fluidRow(
		column(3, selectizeInput(ns("parfold"), "Folder", ft, selected='', multiple=F, options=list(create=T, placeholder='Select or Add'))),
		column(5, textInput(ns("repname"), "", placeholder="Your report name")),
		column(2, HTML("<br>"), actionButton(ns("saverep"), "Save",icon("save")))
		),
		fluidRow(column(10, offset=1,bsAlert(ns("saved"))))
		)
	}
