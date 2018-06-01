invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   pr <- get_earthdata(bbox=NULL)
   print(pr,quote=FALSE)
   if (makenotrun <- TRUE) {
      a1 <- get_earthdata()
      display(a1)
      a2 <- get_earthdata(product=2,date=Sys.Date()-7L
                         ,res=7,bbox=c(57.8,69.4,62.3,70.8))
      display(a2)
   }
})
