invisible({
   plutil::mysource(ursa)
   session_grid(NULL)

   a1 <- ursa(volcano)
   print(a1)
   display(a1)
   
   a2 <- ursa(volcano,flip=TRUE)
   print(a2)
   display(a2)

   a3 <- ursa(volcano,permute=TRUE)
   print(a3)
   display(a3)

   a4 <- ursa(volcano,flip=TRUE,permute=TRUE)
   print(a4)
   display(a4)

   dima <- c(200,300,4)
   b1 <- ursa(array(runif(prod(dima)),dim=dima))
   print(b1)
   display_brick(b1,scale=1,pal.rotate=0,pal.hue=0,decor=FALSE)
   
   session_grid(NULL)
   
   c1 <- ursa(seq(3))
   print(c1)
   c2 <- ursa(seq(3),bands=3)
   print(c2)
   
   c3 <- ursa(value=FALSE)
   str(ursa(c3,"value"))
   
   c4 <- ursa(bands=2,nodata=-99L)
   print(c4)
   print(ursa(c4,"nodata"))
   
   c5 <- ursa(bandname=format(Sys.Date()+seq(7)-1,"%A"))
   ursa(c5,"value") <- rev(seq(nband(c5)))
   c5 <- colorize(c5)
   ct <- ursa(c5,"colortable")
   print(c5)
   
   v <- ursa(c5[3:5],"value")
   str(v)
   v <- c(v)
   str(v)
   c6 <- ursa(v,colortable=ct)
   print(c6)
   print(ursa(c6,"colortable"))
})
