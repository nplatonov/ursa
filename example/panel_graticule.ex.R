invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # Sys.setenv(LANGUAGE="ru") # \dontrun
   cl <- compose_design(layout=c(2,2),legend=NULL)
   if (!FALSE) {
      session_grid(regrid(lim=3.2*1e6*c(-1,-1,1,1)))
      compose_open(cl)
      for (i in 1:4) {
         panel_new()
         panel_coastline()
         panel_graticule(decor=TRUE,trim=i %in% c(2:4))
         panel_annotation(text=as.character(i))
         panel_scalebar(scalebar=i==3)
      }
      compose_close()
   }
   if (!FALSE) {
      session_grid(regrid(lim=1e6*c(-0.5,0.5,1.5,2.5)))
      compose_open(layout=c(2,2),legend=NULL,skip=4)
      for (i in seq(getOption("ursaPngLayout")$image)) {
         panel_new()
         panel_coastline()
         if (i==1)
            panel_graticule()
         else if (i==2)
            panel_graticule(decor=TRUE,lon=seq(0,360,by=20)[-1],lat=seq(-90,90,by=5))
         else if (i==3)
            panel_graticule(decor=TRUE,lon=seq(0,360,by=10)[-1],lat=seq(-90,90,by=2.5)
                           ,trim=TRUE)
         else if (i==4)
            panel_graticule(gridline=FALSE)
         panel_scalebar(scalebar=1)
         panel_annotation(text=as.character(i))
      }
      compose_close()
   }
   if (!FALSE) {
      session_grid(NULL)
      display(pixelsize(),decor=TRUE,grid.col="green3",coast.col="darkgreen",side=2)
   }
  # Sys.setenv(LANGUAGE="") # \dontrun
})
