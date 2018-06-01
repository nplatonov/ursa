invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(nband=3,min=0,max=250)
   a[2] <- -a[1]
   a[3] <- sqrt(a[1])
   a <- ursa_stack(a)
   print(a)
   display_brick(a,stretch="eq",labels=c(-150,-100,0,10,12,20,100,150))
})
