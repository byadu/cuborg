dashboards<- function() {
	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)

	for(i in 1:nrow(repf)) {
		local({
			my_i<- i
			id<- as.character(repf[my_i,1])
			name<- repf[my_i,2]
			output[[id]]<- renderUI({
				callModule(dashboard, id, id)
				dashboardUI(ns(id), id, name)
				})
			})
		}

	ti<- list()
	for(i in 1:nrow(repf)) {
		id<- as.character(repf[i,1])
		name<- repf[i,2]
		ti[[i]]<- tabItem(tabName=id, uiOutput(id))
		}
	ti
	}

dashside<- function() {
	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)
	mi<- list()
	mi[[1]]<- "Dashboards"
	mi[["tabName"]]<- "Dashboards"
	for(i in 1:nrow(repf))
		mi[[2+i]]<- menuSubItem(repf[i,2],tabName=as.character(repf[i,1]))
	mi
	}
