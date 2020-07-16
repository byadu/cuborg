shinyServer(function(input, output, session) {
options(shiny.maxRequestSize=40*1024^2)

	sink(file=stderr())
	ns<- session$ns
	app <- reactiveValues(starting = TRUE)
	session$onFlushed(function() { app$starting <- FALSE })

	source("initapp.R", local=T)
	initsession<- function() {
		q<- parseQueryString("cfgdb=aviso&datadb=aviso")
		q2<-parseQueryString(isolate(session$clientData$url_search))
		if(!is.null(q2) & length(q2)>0)
			q<- q2
	
		uid<-'yadu'
		if(!is.null(q$uid))
			uid<- q$uid
	
		configdbuser<- 'cubot'
		datadbuser<- 'cubot'
	
		D<- initdata(q$datadb, datadbuser)
		M<- initcfg(q$cfgdb, configdbuser, uid)
	
		cat(file=stderr(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "start session:", session$token, "\n")
		return(list(M=M,D=D,uid=uid))
		}

	s<- initsession()
	M<- s$M; D<- s$D; uid<<- s$uid

	session$onSessionEnded(function() {
		dbDisconnect(M$mycfg)
		dbDisconnect(D$mydata)
		cat(file=stderr(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "end session:", session$token, "\n")
 		})

	source("main.R", local=T)

	rr<- reactiveValues(repchanged=0, reportrow=0)
	source("../modules/filters/filters.R", local=T)
	source("../modules/anlz/anlz.R", local=T)
	source("../modules/report/reps.R", local=T)
	source("../modules/dash/dash.R", local=T)
	source("../modules/dmodel/dmodel.R", local=T)
	source("../modules/ingest/ingest.R", local=T)
	})
