initcfg<- function(configdb, configdbuser, uid) {

	loginuser<- function(cfg, uid) {
		# select distinct md_table from menu_dtls where md_id in (select mt_id from menu_trees,login_tree where lt_uid='stat' and lt_role=mt_role);
		my_role<- as.data.frame(filter(cfg$login_tree, lt_uid==!!uid))$lt_role
		my_mt<- as.data.frame(filter(cfg$menu_trees, mt_role %in% !!my_role))$mt_id
		my_mt<- as.data.frame(filter(cfg$menu_trees, mt_role %in% !!my_role))$mt_id
		my_tabs<- unique(as.data.frame(filter(cfg$menu_dtls, md_id %in% my_mt))$md_table)
		return(list(mt=my_mt, tabs=my_tabs))
		}

	foldertree<- function(cfg, uid) {
		repf<- repfold(cfg, uid)
		ids<-as.character(repf[,1])
		names<-as.character(repf[,2])
		ft<-list()
		ft[names]<-ids
		return(list(repf=repf, ft=ft))
		}

	configdbhost<-'localhost'

	dplycfgdb<- opendb(db=configdb, host=configdbhost, user=configdbuser)
	mycfg<-dbConnect(MySQL(), user=configdbuser,password='',dbname=configdb, host=configdbhost)

	cfg<- initconfig(dplycfgdb)
	my<- loginuser(cfg, uid)
	im<- getmodel(cfg, my$tabs)
	mt<- initmenu(cfg, my$mt)
	tjoins<- im$tj
	factabs<- im$tl[im$tl$type=='F',1]
	dimtabs<- im$tl[im$tl$type=='D',1]
	dm<- list()
	dm$factabs<- factabs
	dm$dimtabs<- dimtabs
	model<- im$matrix
	
	fold<- foldertree(cfg, uid)	

	return(list(uid=uid, mycfg=mycfg, cfg=cfg, dm=dm, mt=mt, fold=fold, tjoins=tjoins, model=model))
	}

initdata<- function(dbname, datadbuser) {
	datadbhost<- 'localhost'
	datadbpass<-''

	datadb<- opendb(db=dbname, host=datadbhost, user=datadbuser, password=datadbpass)
	mydata<-dbConnect(MySQL(), user=datadbuser, password=datadbpass, dbname=dbname, host=datadbhost)
	D<- list()
	D$datadb<- datadb
	D$mydata<- mydata
	return(D)
	}
