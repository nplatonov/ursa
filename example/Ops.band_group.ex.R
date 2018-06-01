invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy()
   a[a<80] <- NA
   b1 <- band_stat(a)
   print(b1)
   b2.n <- band_n(a)
   str(b2.n)
   b2.mean <- band_mean(a)
   print(b1$mean)
   print(b2.mean)
   print(b1$mean-b2.mean)
})
