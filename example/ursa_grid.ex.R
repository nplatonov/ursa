invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- pixelsize()
   print(ursa_grid(a))
   ursa_grid(a)$proj4 <- gsub("\\.0+","",ursa_grid(a)$proj4) ## in Rd "\\\\.0+"
   print(ursa_grid(a))
})
