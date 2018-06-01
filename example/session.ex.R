invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   print(getOption("ursaSessionGrid")) ## NULL
   print(g1 <- session_grid()) ## default
   g1$resx <- g1$resy <- 12500
   g1$columns <- as.integer(with(g1,(maxx-minx)/resx))
   g1$rows <- as.integer(with(g1,(maxy-miny)/resy))
   print(session_grid(g1))
   print(session_grid(NULL))
   a <- ursa_new(value=3)
   print(session_grid(a))

   print(session_pngviewer())

})
