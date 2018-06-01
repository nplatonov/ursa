invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   print(methods(class="ursaRaster"))

   a <- pixelsize()
   print(a)
   print(a,grid=TRUE)
   s <- substr(as.character(sessionInfo()),1,48)
   b <- rep(a,length(s))
   bandname(b) <- s
   print(b)

   require(datasets)
   data(volcano)
   print(is.ursa(a))
   print(is.ursa(volcano))
   print(is.ursa(as.ursa(volcano)))
})
