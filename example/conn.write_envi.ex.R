invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   
   list1a <- envi_list(tempdir())
   list1b <- envi_list()
   fname <- tempfile()
   a <- ursa_dummy()
   bandname(a) <- c("first","second","third")
   write_envi(a)
   write_envi(a,fname)
   list2a <- envi_list(tempdir())
   list2b <- envi_list()
   fname1 <- list2a[!(list2a %in% list1a)]
   fname2 <- list2b[!(list2b %in% list1b)]
   a2 <- open_envi(fname1)
   print(a2)
   close(a2)
   envi_remove(c(fname1,fname2))
})
