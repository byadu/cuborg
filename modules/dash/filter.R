filterxy<- function(filtname, filtval, pagereps, i, j) {
	filter<-data.frame(filtval)
	colnames(filter)<-filtname
	for(r in 1:length(pagereps)) {
		rep<-findrep(pagereps[r])
		dxy<-rep$dxy
		x<-match(filtname, colnames(dxy))
		if(!is.na(x)) {
			dxy<-dxy[dxy[,x]==filtval,]
			if(dashgprops[[i]][[j]][[r]]$gfdim > 1) {
				dxy<-dxy[,!colnames(dxy) == filtname]
				dashgprops[[i]][[j]][[r]]$gfdim<<-dashgprops[[i]][[j]][[r]]$gfdim-1
				}
			dashrepsxy[[i]][[j]][[r]]<<-dxy
			}
		}
	}

applyfilter<-function(filtreps, filtval) {
	if(is.null(filtval)) {
		return(NULL)
		}

	if(length(filtreps) == 0) {
		return (NULL)
		}

	i<-filtreps[1]
	j<-filtreps[2]
	k<-filtreps[3]
	pagereps<-reportids[[i]][[j]]
	f<-curr_fils[[i]][[j]]

	if(filtval != "All") {
		filtname<-f$filternames[k]
		filterxy(filtname, filtval, pagereps, i, j)
		thispagefilters[k]<<-filtval
		}
	else {
		for(r in 1:length(pagereps)) {
			dashrepsxy[[i]][[j]][[r]]<<-savedxy[[i]][[j]][[r]]
			dashgprops[[i]][[j]][[r]]<<-savedgprops[[i]][[j]][[r]]
			}
		if(!is.null(thispagefilters)) {
			thispagefilters[k]<<-NA
			for(l in 1:length(thispagefilters)) {
				if(!is.na(thispagefilters[l])) {
					filtname<-f$filternames[l]
					filterxy(filtname, thispagefilters[l], pagereps, i, j)
					}
				}
			}
		}
	}

getfilters<- function(pagereps) {
	for(i in 1:length(pagereps)) {
		gp<-findrep(pagereps[[i]])$gp
		if(i==1) {
			filters<- unlist(gp$xids)
			filternames<- unlist(gp$xnames)
			}
		else {
			filters<- unique(c(filters, unlist(gp$xids)))
			filternames<- unique(c(filternames, unlist(gp$xnames)))
			}
		}
	nf<-length(filternames)
	if(nf == 0)
		return(list(filters=NULL,filternames=NULL,fv=NULL))

	fv<-list()
	for(f in 1:length(filternames)) {
		filt<-filternames[f]
		filtvalues<-c()
		for(i in 1:length(pagereps)) {
			data<-findrep(pagereps[[i]])$dxy
			cn<-colnames(data)
			if(length(cn) == 0)
				next
			for(k in 1:length(cn))
				if(filt == cn[k]) {
					thisfilt<-as.character(data[,k])
					filtvalues<-unique(c(filtvalues,thisfilt))
					}
			}
		fv[[f]]<-filtvalues
		}

	return(list(filters=filters,filternames=filternames,fv=fv))
	}

