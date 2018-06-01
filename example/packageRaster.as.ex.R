invisible({
   require(plutil)
   plutil::ursula(3)
   session_grid(NULL)
   if (requireNamespace("raster")) {
     # pdf.start(pdfname())
      require(methods)
      msk <- ursa_dummy(1,min=0,max=100)>40
      a1 <- ursa_dummy(1,min=200,max=500)[msk]
      a2 <- colorize(a1,ramp=FALSE)
      a3 <- as.integer(ursa_dummy(3,min=0,max=255.99))
      a4 <- ursa_stack(a3[msk])
     # names(ursa_colortable(a2)) <- paste0("value=",names(ursa_colortable(a2)))
      if (isLayer <- TRUE) {
         r1 <- as.Raster(a1)
         message(as.character(class(r1)))
         raster::plot(r1)
         b1 <- as.ursa(r1)
         print(c(exported=a1,imported=b1,failed=b1-a1))
         print(c(theSameValue=identical(ursa_value(a1),ursa_value(b1))
                ,rheSameGrid=identical(ursa_grid(a1),ursa_grid(b1))))
        # display(list(c(exported=a1,imported=b1),c(failed=b1-a1)*1e11)
        #        ,legend=list(list("bottom",1:2),"right"))
      }
      if (isLayerColortable <- TRUE) {
         r2 <- as.Raster(a2)
         message(as.character(class(r2)))
         print(r2)
         raster::plot(r2)
         b2 <- as.ursa(r2)
         print(c(theSameValue=identical(ursa_value(a2),ursa_value(b2))
                ,rheSameGrid=identical(ursa_grid(a2),ursa_grid(b2))))
        # display(list(c(a2,b2),c(diff=b2-a2)),legend=list(list("bottom",1:2),"right"))
      }
      if (isBrickOrRGB <- TRUE) {
         r3 <- as.Raster(a3)
         message(as.character(class(r3)))
         print(r3)
         raster::plot(r3)
         raster::plotRGB(r3)
         b3 <- as.ursa(r3)
         print(c(theSameValue=identical(ursa_value(a3),ursa_value(b3))
                ,rheSameGrid=identical(ursa_grid(a3),ursa_grid(b3))))
      }
      if (isStack <- TRUE) {
         r4 <- as.Raster(a4)
         message(as.character(class(r4)))
         print(r4)
         raster::plot(r4)
         b4 <- as.ursa(r4)
         print(c(theSameValue=identical(ursa_value(a4),ursa_value(b4))
                ,theSameGrid=identical(ursa_grid(a4),ursa_grid(b4))))
      }
     # r3 <- as.Raster(a3)
     # message(as.character(class(r3)))
     # print(r3)
     # plot(r3)
     # pdf.stop(pdfname(),open=!FALSE)
   }
})
