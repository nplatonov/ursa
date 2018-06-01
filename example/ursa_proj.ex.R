invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(nband=1)
   print(ursa_proj(a))
   p4s <- "+init=epsg:3576"
   ursa_proj(a) <- p4s
   print(ursa_proj(a))
   fname <- tempfile()
   write_envi(a,fname)
   a2 <- read_envi(fname,resetGrid=TRUE)
   print(ursa_proj(a2))
   try(print(rgdal::CRSargs(sp::CRS(p4s))))
   envi_remove(fname)
})
