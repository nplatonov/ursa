invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   
   a1 <- as.ursa(volcano)
   print(a1)
   display(a1)

   session_grid(NULL)
   b <- ursa_dummy(mul=1/16,bandname=format(Sys.Date()+seq(3)-1,"%A"))
   print(b)
   
   c1 <- b[[1]] ## equal to 'c1 <- as.matrix(b[1],coords=TRUE)'
   str(c1)
   b1a <- as.ursa(c1)
   print(c(original=b[1],imported=b1a))
   print(c(projection.b1a=ursa_proj(b1a)))
   session_grid(NULL)
   b1b <- as.ursa(c1$z)
   print(b1b)
   print(c(projection.b1b=ursa_proj(b1b)))

   c2 <- as.data.frame(b)
   str(c2)
   session_grid(NULL)
   b2a <- as.ursa(c2)
   print(b2a)

   session_grid(NULL)
   attr(c2,"proj4") <- NULL
   b2b <- as.ursa(c2)
   print(b2b)
   print(ursa_grid(b2b))
   
   c3 <- unclass(as.matrix(b,coords=TRUE))
   str(c3)
   session_grid(b)
   b3a <- as.ursa(c3)
   print(b3a)
   print(ursa_grid(b3a))
   session_grid(NULL)
   b3b <- as.ursa(c3)
   print(b3b)
   print(ursa_grid(b3b))
   
   c4 <- as.array(b)
   str(c4)
   session_grid(b)
   b4a <- as.ursa(c4)
   print(b4a)
   print(ursa_grid(b4a))
   session_grid(NULL)
   b4b <- as.ursa(c4)
   print(b4b)
   print(ursa_grid(b4b))
   
   n <- 20
   c5 <- data.frame(y=runif(n,min=1000000,max=5000000)
                   ,x=runif(n,min=-3000000,max=1000000)
                   ,value=runif(n,min=0,max=10))
   print(head(c5))
   session_grid(b)
   b5a <- as.ursa(c5)
   print(b5a)
   display(b5a)
   session_grid(NULL)
   b5b <- as.ursa(c5)
   print(b5b)
   display(b5b)
   
   b6 <- as.ursa(system.file("pictures/erdas_spnad83.tif",package="rgdal"))
   print(b6)
   display(b6,coast=FALSE,col="orange")
   
   { ## \dontrun
      require(raster)
      r <- brick(system.file("external/rlogo.gri",package="raster"))
      print(r)
      b7 <- as.ursa(r)
      ursa_proj(b7) <- ""
      print(b7)
      display_rgb(b7)
   } ##
})
