invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   set.seed(500)
   a <- ursa_dummy(nband=3,min=0,max=255)
   a <- ursa_stack(a)
   display_rgb(a,decor=FALSE) ## ,device="windows"
})
