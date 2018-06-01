invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- colorize(pixelsize(),nbreak=4)
   t1 <- as.table(a)
   print(t1)
   str(t1)
   ursa_colortable(a) <- NULL
   t2 <- as.table(a)
   print(t2)
})
