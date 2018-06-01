invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   deprecated <- FALSE
   if (deprecated)
      require(methods)
   a <- ursa_dummy(mul=1/16)
   a <- a[a>100]
   print(a)
   print(band_mean(a))
   if (not_dontrun <- !TRUE) {
      fname <- tempfile()
      b1 <- polygonize(a,fname)
      list1 <- dir(path=dirname(fname),pattern=basename(fname),full.names=TRUE)
      print(list1)
      file.remove(list1)
   }
   b2 <- polygonize(a,engine="sp") ## try 'engine="sf"'
   if (deprecated) {
      print(apply(b2@data,2,mean,na.rm=TRUE))
      print(with(ursa_grid(a),array(c(minx,miny,maxx,maxy),dim=c(2,2)
                                   ,dimnames=list(c("x","y"),c("min","max")))))
      print(b2@bbox)
   }
   else {
     # print(apply(spatial_data(b2),2,mean,na.rm=TRUE))
      print(colMeans(spatial_data(b2),na.rm=TRUE))
      print(ursa_bbox(a))
      print(spatial_bbox(b2))
   }
})
