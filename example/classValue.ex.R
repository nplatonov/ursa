invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   require(ursa)
   session_grid(regrid(mul=1/4))
   a1 <- create_envi("exam1.envi",bandname=c("today","tomorrow"))
   str(ursa_value(a1))
   close(a1)
   envi_remove("exam1")
   a2 <- ursa_dummy(nband=4,min=1,max=99)
   bn2 <- bandname(a2)[2]
   str(ursa_value(a2),digits=3)
   a3 <- as.integer(a2)
   str(ursa_value(a3))
   str(ursa_value(a3,2))
   print(ursa_value(a3))
   ursa_value(a3,bn2) <- 199
   ursa_value(a3)[,3] <- 299
   a3[4] <- 399
   print(a3)
   ursa_value(a3[1:3]) <- ursa_value(a3[4])
   print(a3)
   ursa_value(a3[1:3]) <- -c(1:3)
   print(a3)
})
