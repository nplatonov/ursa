invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- pixelsize()
   a <- a-min(a)+0.5
   str(ursa_value(a))
   print(storage.mode(a$value))
   b <- as.integer(a)
   str(ursa_value(b))
   print(storage.mode(b$value))
})
