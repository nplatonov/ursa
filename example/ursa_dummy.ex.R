invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a1 <- as.integer(ursa_dummy(nband=1,elements=1e3))  ## white noise
   display(a1,legend=NULL)
   a2 <- ursa_dummy()
   print(a2)
   display_brick(a2)
   display_stack(a2)
   display_rgb(a2)
})
