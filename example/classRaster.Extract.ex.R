invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   ## Prepare
   session_grid(regrid(mul=1/8))
   a <- pixelsize()
   w <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"
         ,"MondayAgain")
   b <- rep(a/mean(a),length(w))+seq(length(w))-1
   bandname(b) <- w
   nr <- ursa_rows(b)
   bottom <- (as.integer(nr/2)):nr
   write_envi(b,"tmp1",compress=FALSE,interleave="bil")

   ## Extract
   print(b["Monday",regexp=TRUE])
   print(b["Monday",regexp=FALSE])
   print(b["s"])
   print(b["^s"])
   d1 <- b[6,bottom]
   rm(b)

   ## Read from file
   b <- open_envi("tmp1")
   print(b[])
   print(b[-c(6:8)])
   d2 <- b[,bottom][6] ## don't use b[6,bottom]
   close(b)
   envi_remove("tmp1")

   ## Compare
   print(d1)
   print(d2)
})
