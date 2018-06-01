invisible({
   mysource("plaster.R")
   session_grid(regrid(mul=1/4))
   a <- ursa_dummy(nband=7,min=0,max=100)
   filled.contour(a)
})
