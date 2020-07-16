getdash<- function(cfg, uid) {
	# dashboards
#	dashq<-paste("select uc_content from user_content where uc_contentid='Dashboard' and uc_userid='", uid, "'", sep="")
#	dash<-dbGetQuery(configdb, dashq)
	dash<- filter(cfg$user_content, uc_contentid=='Dashboard' & uc_userid==uid) %>% selectcols('uc_content')
	dash<- as.data.frame(dash)

	dashboards<-c()
	pages<-data.frame()
	reportids<-list()
	filters<-list()
	gfdim<-1

	for(i in 1:nrow(dash)) {
		d<-fromJSON(dash[i,])
		dashboards[i]<-d$name
		p<-as.character(strsplit(sub("@#","",d$pages), " "))
		reps<-list()
		fils<-list()
		# pages
		for(j in 1:length(p)) {
		#	pageq<-paste("select uc_content from user_content where uc_userid='",uid,"' and uc_contentid='Page' and us_itemId=",p[j],sep="")
		#	page<-dbGetQuery(configdb, pageq)
			page<- filter(cfg$user_content, uc_contentid=='Page' & uc_userid==uid & us_itemId==p[j]) %>% selectcols('uc_content')
			page<- as.data.frame(page)
			page<-fromJSON(as.character(page))
			pages[i,j]<-page$name
			r<-page$reports$ReportId
			if(length(r) > 0) {
				reps[[j]]<- r
				}
			}
			if(length(reps) > 0) {
				reportids[[i]]<-reps
				}
		}
	return(list(dashboards=dashboards, pages=pages, reportids=reportids))
	}

getreps<- function(cfg, reportids) {
	if(length(reportids) <= 0)
		return(NULL)
	dashrepsxy<-list()
	dashgprops<-list()
	for(i in 1:length(reportids)) {
		pagereps<-reportids[[i]]
	#	if(length(pagereps) <= 0)
	#		next
		pagerepsxy<-list()
		pagegprops<-list()
		for(j in 1:length(pagereps)) {
			reports<-pagereps[[j]]
	#		if(length(reports) <= 0)
	#			next
			repsxy<-list()
			gprops<-list()
			for(k in 1:length(reports)) {
				g<-getxy(cfg, reports[k])
				if(is.null(g$dxy)) next
				repsxy[[k]]<-g$dxy
				gprops[[k]]<-g$gp
				}
			if(length(repsxy)==0) next
			pagerepsxy[[j]]<-repsxy
			pagegprops[[j]]<-gprops
			}
		dashrepsxy[[i]]<-pagerepsxy
		dashgprops[[i]]<-pagegprops
		}
	return(list(dashgprops=dashgprops,dashrepsxy=dashrepsxy))
	}

findrep<- function(id) {
	if(is.null(id))
		return(NULL)
	if(length(reportids) <= 0)
		return(NULL)
	for(i in 1:length(reportids)) {
		pagereps<-reportids[[i]]
		if(length(pagereps) <= 0)
			next
		for(j in 1:length(pagereps)) {
			reports<-pagereps[[j]]
			if(length(reports) <= 0)
				next
			for(k in 1:length(reports)) {
				if(reports[k] == id) {
					dxy<-dashrepsxy[[i]][[j]][[k]]
					gp<-dashgprops[[i]][[j]][[k]]
					return(list(gp=gp, dxy=dxy))
					}
				}
			}
		}
	return(NULL)
	}
