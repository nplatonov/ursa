invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   session_grid(regrid(mul=1/4))
   a <- ursa_dummy(nband=2,min=0,max=100)
   print(a)
   print(is.na(a))
   a2 <- ursa_new(nband=2)
   print(a2)
   print(is.na(a2))
   a3 <- a
   a3[a3<30 | a3>70] <- NA
   print(a3)
   print(is.na(a3))
   is.na(a3) <- 200
   print(a3)
})
