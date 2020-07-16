
notfiltered<- function(thisfilt, currfilts) {
	for(i in 1:length(currfilts))
		if(thisfilt$mdtl$md_id == currfilts[[i]]$mdtl$md_id)
			return(0)
	return(1)
	}
addfilters<- function(f1, f2) {
	af<- f2
	if(!is.null(f1) & !is.null(f2)) {
		k<- length(f2)
		if(k == 0) 
			af<- f1
		else if(length(f1) > 0) {
			j<- 0
			for(i in 1:length(f1))
				if(notfiltered(f1[[i]], f2)) {
					j<- j+1
					af[[k+j]]<- f1[[i]]
					}
			}
		}
	af
	}

extract_filts<- function(filter) {
	i<- gregexpr('\"', filter)[[1]]
	if(i[1] == -1) { 
		ftype<- 'char'
		f<- filter
		}
	else {
		ftype<- 'vec'
		f<- c()
		k<- 0
		for(j in seq(1,length(i),2)) {
			k<- k+1
			f[k]<- substr(filter, i[j]+1, i[j+1]-1)
			}
		}
	return(list(ftype=ftype, fval=f))
	}

filteredcolumns<- function(filters) {
	f<- list(); k<- 0
	filts<- list()
	fdims<- isolate(s$dsel)
	fmeas<- isolate(s$msel)
	gfdim<- isolate(rg$g$gp$gfdim)
	for(i in 1:length(filters)) {
		filter<- filters[i]
		if(!is.null(filter) & filter!='') {
			filter<- extract_filts(filter)
			if(i <= gfdim)
				mdtl<- getdims(fdims[i])[[1]]
			else
				mdtl<- getmeas(fmeas[i-gfdim])[[1]]
			k<- k+1
			f[[k]]<- list(mdtl=mdtl,ftype=filter$ftype,fval=filter$fval)
			}
		}
	return(f)
	}
