invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   g1 <- session_grid(regrid(session_grid(),mul=1/10))
   n <- 1000
   x <- with(g1,runif(n,min=minx,max=maxx))
   y <- with(g1,runif(n,min=miny,max=maxy))
   z <- with(g1,runif(n,min=0,max=10))
   da <- data.frame(x=x,y=y,value=z)
   res <- c(mean=allocate(da,fun="mean")
           ,mean_=NA
           ,sum=allocate(da,fun="sum")
           ,count=allocate(da,fun="n"))
   res["mean_"]=res["sum"]/res["count"]
   print(res)
})
