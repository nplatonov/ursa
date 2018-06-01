invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (!FALSE) {
      display(pixelsize(NULL),raster.verb=TRUE)
   }
   if (!FALSE) {
      session_grid(regrid(mul=1/32))
      dima <- with(session_grid(),c(columns,rows,3))
      a <- ursa_new(value=array(runif(prod(dima),min=127,max=255),dim=dima))
      p <- colorize(a,pal=c("black","white"),ramp=TRUE,value=0:256)
      compose_open(layout=c(2,3),skip=4,dev=FALSE
                  ,legend=list(list("top","full"),list("bottom",2:3)))
      for (i in seq(6)) {
         panel_new(col="white")
         if (i<4)
            panel_raster(p[i])
         else
            panel_raster(a,interpolate=i==5)
         panel_decor(col="black")
         panel_annotation(c("red","green","blue"
                          ,"interpolate=FALSE","interpolate=TRUE"))
      }
      legend_colorbar(p,label=seq(0,256,by=16),units="channels")
      legend_mtext("color composite")
      compose_close()
   }
   if (!FALSE) {
      compose_open()
      panel_new()
      ct <- panel_raster(pixelsize(),pal.rich=240,pal.rotate=0)
      panel_decor()
      compose_legend(ct)
      compose_close()
   }
   if (FALSE) {
      if (!TRUE) {
         histogram(read_envi("c:/tmp/bathy"))
      }
      else {
         a <- read_envi("c:/tmp/bathy")
         histogram(a)
      }
   }
   if (FALSE) {
     # q()
      a0 <- -sample(1:20,1)*10-10
      a <- read_envi("c:/tmp/depth")[1]+a0
      print(a)
     # b <- reclass(a,byte=TRUE)
     # display(a,stretch="equal",palname="internal")
     # display(a,stretch="equal",palname="Blues")
      b <- colorize(a[a>a0],stretch="eq",colors=sample(1:20,1)*10,,pal.rich=-15,pal.rotate=0,interval=TRUE)
      print(length(b$colortable))
      write_envi(b,"c:/tmp/tmp1.envi")
      bt <- as.table(b)
     # print(c(head(bt,25),NA,tail(bt,25)))
      lab <- sample(2:18,1)
      print(lab)
     # histogram(b,labels=lab)
      display(b,side=1,labels=lab)
     # display(a,stretch="bathy")
   }
})
