invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # p <- colorize(pixelsize())
   compose_open(layout=c(2,3))
   for (i in seq(6)) {
      panel_new()
      if (i==1) {
         panel_decor()
        # next
      }
      panel_decor(coast=i==2,grid=!(i %in% c(2,3)))
      if (i==3)
         panel_decor(grid=TRUE,coast=FALSE)
      if (i==4) {
         pal <- palette(rainbow(12))
         panel_decor(col=sample(palette()))
         palette(pal) ## if failed, use 'palette("default")'
      }
      panel_decor(coast=5,grid=5,coast.col="darkblue"
                 ,coast.fill="lightyellow",grid.lty=1,grid.col="salmon4")
      if (i==6) {
         panel_graticule()
         panel_coastline(fill="#FFFF7F8F")
      }
      panel_annotation(text=c("default","only coastline","only gridline"
                              ,"rainbow","solid lines and fill"
                              ,"land over gridline"))
   }
   compose_close()
})
