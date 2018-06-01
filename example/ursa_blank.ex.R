invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_new(bandname=c("first","second","third","fourth"))
   ursa_value(a,"first") <- 0 ## 'a[1] <- 1' works, but it is slow
   print(ursa_blank(a))
   a[3] <- pixelsize()
   a[4] <- a[3]>625
   print(a)
   print(band_blank(a))
   print(which(band_blank(a)))
   print(ursa_blank(a))
})
