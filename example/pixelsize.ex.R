invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   print(pixelsize())
  ## Not run: ## \dontrun{
   dpath <- file.path("ftp://sidads.colorado.edu/pub/DATASETS"
                     ,"nsidc0081_nrt_nasateam_seaice/north")
   dst <- tempfile(fileext=".bin")
   isOK <- FALSE
   d3 <- Sys.Date()
   for (i in seq(5)) {
      src <- file.path(dpath,format(d3,"nt_%Y%m%d_f18_nrt_n.bin"))
      a <- try(download.file(src,dst,mode="wb"))
      if ((is.integer(a))&&(a==0)) {
         isOK <- TRUE
         break
      }
      d3 <- d3-1
   }
   if (isOK) {
      g1 <- regrid(bbox=c(-385,-535,375,585)*1e4,res=25*1e3
                  ,proj4=paste("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45"
                              ,"+k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449"
                              ,"+units=m +no_defs"))
      session_grid(g1)
      b <- readBin(dst,integer(),size=1L,n=136492L,signed=FALSE)
      ice <- ursa_new(value=tail(b,-300))
      ice[ice>251] <- NA ## keep Pole
      ice[ice==251] <- 250 ## consider 100% ice at Pole
      ice <- ice/2.5 ## uncategorize
      ice[ice<15] <- 0 ## not ice, if less 15%
      extent1 <- band_sum(ice*1e-2*with(g1,resx*resy*1e-6))*1e-6
      extent2 <- band_sum(ice*1e-2*pixelsize(ice))*1e-6
      message(paste("Near real-time Arctic sea ice extent(NASA Team algorithm, NSIDC)"))
      message(sprintf("   Direct area calculation:         %5.2f*1e6 km^2.",extent1))
      message(sprintf("   Distortion in area is corrected: %5.2f*1e6 km^2.",extent2))
   }
   else
      message("It is failed to get sea ice concentration data.")
  ## } ## End not run
})
