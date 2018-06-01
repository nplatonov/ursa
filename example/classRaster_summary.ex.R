invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   session_grid(regrid(mul=1/4))
   a <- ursa_dummy(nband=3)
   print(summary(a))
   print(summary(ursa_value(a)))
   print(a)
})
