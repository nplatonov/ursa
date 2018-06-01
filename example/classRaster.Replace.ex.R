invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   ## Prepare
   session_grid(regrid(mul=1/4))
   a <- pixelsize()
   w <- c("first","second","third","fourth","fifth","sixth")
   b1 <- rep(a/mean(a),length(w))+seq(length(w))-1
   bandname(b1) <- w
   nr <- ursa_rows(b1)
   bottom <- (as.integer(nr/2)):nr
   write_envi(b1,"tmp1",compress=FALSE,interleave="bil")
   b2 <- b1
   print(b1)

   ## Replace
   b2[1] <- 10+b1["second"]
   b2[2] <- 20
   try({
      data(volcano)
      b2[3] <- 30+volcano
   }) ## error: unable to coerce
   b2["fourth"] <- 40+as.matrix(b1[3])
   b2[5] <- 50+as.array(b1[4])
   set.seed(352)
   try({
      opW <- options(warn=10)
      b2["six"] <- 60+6+runif(5,min=-1,max=1)
      options(opW)
   }) ## warning/error: unable to coerce
   print(b2)
   print(object.size(b2))

   ## Write
   b3 <- create_envi(b2,"tmp2")
   print(object.size(b3))
   for (i in chunk_line(b3,0.04))
   {
      b3[,i] <- b2[,i]+100
      if (5 %in% i)
         print(object.size(b3))
   }
   close(b3)
   print(object.size(b3))
   b4 <- read_envi("tmp2")
   print(b4)
   envi_remove("tmp[12]")
})
