invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   session_grid(regrid(mul=1/4))
   a1 <- ursa_dummy(nband=3,min=-5*pi,max=5*pi)
   print(a1)

   try(print(complex1 <- Re(a1)))

   print(math1 <- a2 <- round(a1))
   print(math1 <- sin(a1))
   print(math2 <- floor(a1))
   print(math3 <- ceiling(a1))
   print(math4 <- cumsum(a1)) ## does this have a sense for rasters?

   print(ops1 <- a1-2*rev(a1)+mean(a1))
   print(mean(ops1)) ## vanishing
   a2 <- ursa_new(value=c(1,2,4),bandname=c("single","double","quadruple"))
   print(a2)
   print(ops2 <- a2[1]==a2[2])
   print(ops3 <- a2[1]==a2[2]/2)
   print(ops4 <- a1>0)
   print(a1[a1>0])

   print(sum1 <- sum(a1))
   print(sum2 <- range(a1))
})
