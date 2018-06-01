invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   n1 <- 3
   n2 <- 283 # 283 683
   p <- 0.0051
   a <- ls.str()
   pb <- ursaProgressBar(min=0,max=n1,title="first")
   for (i in seq(n1)) {
      pb2 <- ursaProgressBar(min=0,max=n2,title="second")
      for (i in seq(n2)) {
         setUrsaProgressBar(pb2)
         Sys.sleep(p)
      }
      close(pb2)
      setUrsaProgressBar(pb)
   }
   close(pb)
})
