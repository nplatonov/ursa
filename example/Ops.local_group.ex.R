invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   b <- ursa_dummy(nband=7,min=0,max=100)
   b[b<40] <- NA
   print(b)
   res <- c(mean=mean(b),mean=local_mean(b)
           ,sum0=local_sum(b,cover=0),sum1=local_sum(b,cover=1))
   print(res)

  # \dontrun {
   display(b)
   display(res)
  # }
})
