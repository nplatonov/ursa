invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   session_grid(regrid(mul=1/2))
   a <- ursa_dummy(4,min=0,max=255)
   a[a<70] <- NA
   compose_open(layout=c(1,4),legend=NULL)
   for (i in seq(4)) {
      panel_new()
      panel_plot(as.raster(a[seq(i)]),interpolate=FALSE)
      panel_annotation(paste("Number of channels:",i))
   }
   compose_close()

   plutil::pdf.start(plutil::pdfname())
   op <- par(mfrow=c(2,2),mar=rep(0.5,4))
   plot(as.raster(a[1:1]),interpolate=FALSE)
   plot(as.raster(a[1:2]),interpolate=FALSE)
   plot(as.raster(a[1:3]),interpolate=FALSE)
   plot(as.raster(a[1:4]),interpolate=FALSE)
   par(op)
   plutil::pdf.stop(plutil::pdfname())
})
