invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (TRUE) { # example no.1 -- direct usage
      a <- colorize(pixelsize())
      compose_open(a)
      panel_new()
      panel_raster(a)
      panel_scalebar()
      compose_close()
   }
   if (TRUE) { # example no.2 -- indirect usage
      display_rgb(ursa_dummy(nband=3,min=0,max=255)
                 ,scalebar=TRUE,scalebar.col="white",scalebar.fill="black")
   }
   if (TRUE) { # example no.3 -- for paper copy
      a <- colorize(pixelsize(),breakvalue=seq(400,650,by=50),palname="Greys",inv=FALSE)
      compose_open(scale="1:95000000",dpi=150,device="cairo",family="Times")
      compose_plot(a,gridline=FALSE,scalebar=TRUE,scalebar.x=1,units=expression(km^2))
      compose_close(bpp=8)
   }
   if (requireNamespace("ggmap")) { # example no.4 -- length distortion in the Transverse Mercator projection
     # \dontrun{
     # m2 <- ggmap::get_openstreetmap(bbox=c(left=42,bottom=70,right=70,top=82)
     #                               ,format="png",messaging=TRUE
     #                               ,scale=34942642,filename="ggmapTempMy")
      m2 <- ggmap::get_googlemap(center=c(lon=56,lat=77.5),zoom=4,scale=1
                                ,size=c(320,640),format="png8",messaging=TRUE)
     # file.remove("ggmapTempMy.png")
      res <- as.ursa(m2)
      compose_open(res,legend=NULL,scale=1,pointsize=12)
      panel_new()
      panel_raster(res)
      panel_graticule(decor=TRUE)
      for (p in c("bottom","center","top"))
         panel_scalebar(pos=p,w=100)
      compose_close()#}
   }
})
