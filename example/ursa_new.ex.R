invisible({
   plutil::mysource(ursa)
   session_grid(NULL)

   a1 <- ursa_new(volcano)
   print(a1)
   display(a1)
   
   a2 <- ursa_new(volcano,flip=TRUE)
   print(a2)
   display(a2)

   a3 <- ursa_new(volcano,permute=TRUE)
   print(a3)
   display(a3)

   a4 <- ursa_new(volcano,flip=TRUE,permute=TRUE)
   print(a4)
   display(a4)

   dima <- c(200,300,4)
   b1 <- as.ursa(array(runif(prod(dima)),dim=dima))
   print(b1)
   display_brick(b1,scale=1,palname="Greys",decor=FALSE)
   
   session_grid(NULL)
   
   c1 <- ursa_new(seq(3))
   print(c1)
   c2 <- ursa_new(seq(3),bands=3)
   print(c2)
   
   c3 <- ursa_new(value=FALSE)
   str(ursa_value(c3))
   
   c4 <- ursa_new(bands=2,nodata=-99L)
   print(c4)
   print(ignorevalue(c4))
   
   c5 <- ursa_new(bandname=format(Sys.Date()+seq(7)-1,"%A"))
   ursa_value(c5) <- rev(seq(nband(c5)))
   c5 <- colorize(c5)
   ct <- ursa_colortable(c5)
   print(c5)
   
   v <- ursa_value(c5[3:5])
   str(v)
   v <- c(v)
   str(v)
   c6 <- ursa_new(v,colortable=ct)
   print(c6)
   print(ursa_colortable(c6))
})
