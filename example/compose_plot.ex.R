invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(nband=6,min=0,max=255,mul=1/4)
   if (example1 <- TRUE) {
      b1 <- ursa_brick(a)
     # b1 <- colorize(b1,stretch="positive",ramp=FALSE)
      compose_open(b1)
      compose_plot(b1,grid=FALSE,coast=FALSE,scale=FALSE,trim=1
                  ,stretch="positive",ramp=!FALSE)
      compose_close(crop="crop2")
   }
   if (example2 <- TRUE) {
      b2 <- ursa_stack(a)
      compose_open(b2)
      compose_plot(b2,grid=FALSE,coast=FALSE,labels=5,trim=2,las=0)
      compose_close()
   }
})
