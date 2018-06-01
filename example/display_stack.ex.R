invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(nband=7,min=0,max=250)
   display_stack(a,layout=c(NA,3))
  # a <- list(a,sqrt(a[1]))
  # print(a)
  # display(a)
})
