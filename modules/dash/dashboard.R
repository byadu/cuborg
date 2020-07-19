dashboard<- function(input, output, session, dashid) {
	ns<- session$ns

	print('dash')
	rl<- replist(M$cfg, dashid)
	print(rl)
	for(j in 1:nrow(rl)) {
		local({
			my_j<- j
			repid<- rl[my_j,1]
			output[[repid]]<- renderUI({
				g<- setrepid(M$cfg, repid)
				if(!is.null(g)) {
					callModule(chart, repid, g, noopt=1)
					chartUI(ns(repid), g, noopt=1)
					}
				})
			})
		}
	}

dashboardUI<- function(id, dashid, dashname) {
	ns<- NS(id)

	print('dashui')
	rl<- replist(M$cfg, dashid)

	htmlfile<- paste0("templs/", dashname, ".html")
	if(file.exists(htmlfile)) {
		ui<-paste0('htmlTemplate("', htmlfile, '",')
		for(j in 1:nrow(rl)) {
			repid<- rl[j,1]
			a=paste0('R', j, '=', 'uiOutput(ns(', repid, '))')
			if(j > 1)
				ui<- paste(ui, ',')
			ui<- paste(ui,a)
			}
		ui<- paste(ui, ')')
		eval(parse(text=ui))
		}

	else {			# draw nx2 grid 
		p1<- ""
		fr<- 0
		frow<- list()

		rl<- replist(M$cfg, dashid)
		for(j in 1:nrow(rl)) {
			repid<- rl[j,1]
			reptitle<- rl[j,2]
			p<- uiOutput(ns(repid))
	
			if(j %% 2)
				p1<- p
			else {
				fr<- fr+1
				frow[[fr]]<-fluidRow(column(8,p1), column(4, p))
				}
			}
		if(j%%2) {
			fr<- fr+1
			frow[[fr]]<- fluidRow(column(12,p1))
			}

	print('end dashui')
		a<-do.call(fluidPage, frow)
		a
		}
	}
