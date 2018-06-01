invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (TRUE) {
      display(ursa_dummy(1),units="Required 99 labels; displayed less"
              ,colorbar.labels=99,las=3,gridline.trim=FALSE,colorbar.trim=1L)
   }
   cname <- c("Apple","Orange","Cherry","Blueberry","Strawberry","Currant")
  # print(abbreviate(cname))
   a <- ursa_dummy(4)
   b <- list(colorize(a[1],value=seq(50,200,length=length(cname))
                     ,name=cname)#,stretch="category")
            ,colorize(a[2]*10,ramp=FALSE),colorize(a[3]*100),colorize(a[4]/10))
   la <- legend_align(b[3:4])
   leg <- vector("list",10)
   leg[[1]] <- "left"
   leg[[2]] <- "right"
   for (i in seq(4)) {
      leg[[i+2]] <- list("top",i)
      leg[[i+6]] <- list("bottom",i)
   }
   compose_open(layout=c(1,4),legend=leg,scale=NA,dev=FALSE) # use 'dev=TRUE' to check layout
   compose_panel(b)
   legend_colorbar(b[[1]],lomar=20,himar=0) ## "left"
   legend_colorbar(b[[4]],labels=c(6,7.5,12,15,20)
                  ,units=as.expression(substitute(italic(paste(
                   "Manual set of labels. Units are suitable to be ",degree*C))))) ## "right"
   legend_colorbar(b[[1]],las=2,adj=0.5,turn=TRUE,lomar=6,himar=6
                  ,units="Central adjustment; inverse order") ## ("top",1)
   legend_colorbar(b[[2]],cex=0.9
                  ,units="Horizontal labels can be overlapped") ## ("top",2)
   legend_colorbar(b[[3]],las=3,align=la
                  ,units="Increased width, but aligned -->") ## ("top",3)
   legend_colorbar(b[[4]],las=3,align=la,labels=3
                  ,units="<-- Reduced width, but aligned") ## ("top",4)
   legend_colorbar(b[[1]],las=2,adj=0,shift=0.9,turn=FALSE,lomar=2,himar=10
                  ,units="Left adjustement. Non-optimal; shifted") ## ("bottom",1)
   legend_colorbar(b[[2]],las=3,adj=0
                  ,units="But right adj. is default for numeric") ## ("bottom",2)
   legend_colorbar(b[[3]],labels=99,las=3,trim=2L
                  ,units="Required 99 labels, but displayed less") ## ("bottom",3)
   legend_colorbar('Caption from named item'=b[[4]],labels=99) ## ("bottom",4)
   compose_close()
})
