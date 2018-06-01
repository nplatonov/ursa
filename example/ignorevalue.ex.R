invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- round(ursa_dummy(nband=1,min=0.500001,max=4.499999))
   print(a)
   print(as.table(a))
   print(ignorevalue(a))
   ignorevalue(a) <- NA
   print(as.table(a))
   print(ignorevalue(a))
   ignorevalue(a) <- 4
   print(as.table(a))
   print(ignorevalue(a))
   print(a)
})
