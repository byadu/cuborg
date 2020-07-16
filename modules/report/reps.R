reportscharts<- function() {
	observeEvent(input$del, ignoreInit=T, {
		delreport(M$mycfg, isolate(rg$g))
		rr$reportrow<- NULL
		rr$repchanged<<- isolate(rr$repchanged)+1
		})
#rr$repchanged
	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)

	for(i in 1:nrow(repf)) {
		local({
			my_i<- i
			rl<- replist(M$cfg, repf[my_i,1])
			for(j in 1:nrow(rl)) {
				local({
					my_j<- j
					repid<- as.character(rl[my_j,1])
					output[[repid]]<- renderUI({
						g<- setrepid(M$cfg, repid)
						rg$g<- g
						xr$gtype<- NULL
	#	rg$g$gp$gtype<- xr$gtype
						if(!is.null(g)) {
#							callModule(chart, repid, 1, g)
#							a<-	chartUI(repid, g)
							callModule(analyze, repid)
							a<-	analyzeUI(repid)
							}
						a
						})
					})
				}
			})
		}

	ti<- list()
	k<- 1
	for(i in 1:nrow(repf)) {
		reps<- replist(M$cfg, repf[i,1])
		for(j in 1:nrow(reps)) {
			ti[[k]]<- tabItem(tabName=as.character(reps[j,1]), uiOutput(reps[j,1]))
			k<- k+1
			}
		}
	ti
	}

repside<- function() {
	rr$repchanged

	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)
	mi<- list()
	mi[[1]]<- "Reports"
	mi[["tabName"]]<- "Reports"
	for(i in 1:nrow(repf)) {
		smi<- list()
		smi[1]<- repf[i,2]
		smi[["tabName"]]<- as.character(repf[i,1])
		reps<- replist(M$cfg, repf[i,1])
		for(j in 1:nrow(reps)) {
			smi[[j+2]]<- menuSubItem(reps[j,2], tabName=as.character(reps[j,1]))
			}
		mi[[2+i]]<- do.call(menuItem, smi)
		}
	mi
	}
