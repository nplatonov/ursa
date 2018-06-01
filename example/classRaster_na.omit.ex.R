invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   session_grid(regrid(mul=1/4))
   a <- ursa_new(value=1:3)
   print(a)
   a[2] <- NA
   print(a)
   a2 <- na.omit(a)
   print(a2)
})
