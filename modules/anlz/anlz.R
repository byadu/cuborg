xr<- reactiveValues(dimgrp=NULL, measgrp=NULL, mselids=NULL, dselids=NULL, mselnames=NULL, dselnames=NULL, fdimsel=NULL, gtype='dt', currfilters=NULL)
f<- reactiveValues(rows_selected=NULL)
rg<- reactiveValues(g=NULL)

#source("../modules/anlz/libanlz.R", local=T)
source("../modules/anlz/save.R", local=T)
source("../modules/anlz/analyze.R", local=T)
