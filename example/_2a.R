invisible({
   mysource("plaster.R")
   a <- ursa_dummy(3,min=0,max=100)
   a <- a[a>=20 & a<=80]
   ignorevalue(a) <- 121
   b1 <- as.list(a[2])
   str(b1)
  # image(b1)
   b2 <- as.list(a[2:3])
   print(identical(b1,b2))
   a2 <- as.ursa(b2)
   res <- c(src=a[2],exported_then_imported=a2,diff=a[2]-a2)
   print(res)
})
