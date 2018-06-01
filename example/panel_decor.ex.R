invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(nband=1,min=0,max=100)
   a[a<30] <- NA
   compose_open()
   panel_new()
   ct <- panel_raster(a)
   panel_decor(graticule.col="green4",graticule.lwd=2,scalebar.col="brown")
   compose_legend(ct)
   compose_close()
})
