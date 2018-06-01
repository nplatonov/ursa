invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(1,mul=1/8,elements=0,bandname="src")
   a[a<80] <- NA
   bF <- c(fillNA.F=focal_median(a[1],size=5,cover=0.5,fillNA=FALSE))
   bT <- c(fillNA.T=focal_median(a[1],size=5,cover=0.5,fillNA=TRUE))
   print(c(diff=bT-bF))
   d <- c(a,bF,bT)
   print(d)
   display(d)
})
