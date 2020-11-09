'panel_wind' <- function(u,v,x,y,col="black",lwd=0.5,rarity=32,self=FALSE) {
   if ((!self)&&(.skipPlot(TRUE)))
      return(NULL)
   if ((!missing(u))&&(missing(v))&&(missing(x))&&(missing(y))) {
      v <- u[2]
      u <- u[1]
   }
   g1 <- session_grid()
   s <- 2*pi/3
   b <- as.data.frame(regrid(c(u=u,v=v),mul=1/rarity))
   b$r <- with(b,sqrt(u*u+v*v))
   b$a <- with(b,atan2(v,u))#*180/pi
   b$m <- 0.65*session_cellsize()
   b$x2 <- b$x-b$m*cos(b$a)
   b$y2 <- b$y-b$m*sin(b$a)
   if (FALSE)
      print(series(b),digits=3)
   b$x3 <- with(b,x+0.8*(x2-x))
   b$y3 <- with(b,y+0.8*(y2-y))
   b$x4 <- with(b,x3+0.2*m*cos(a+s))
   b$y4 <- with(b,y3+0.2*m*sin(a+s))
   b$x5 <- b$x2
   b$y5 <- b$y2
   b$x6 <- with(b,x5+0.4*m*cos(a+s))
   b$y6 <- with(b,y5+0.4*m*sin(a+s))
   b$x7 <- with(b,x+0.6*(x2-x))
   b$y7 <- with(b,y+0.6*(y2-y))
   b$x8 <- with(b,x7+0.2*m*cos(a+s))
   b$y8 <- with(b,y7+0.2*m*sin(a+s))
   d <- as.ursa(b)
   if (self) {
      compose_open(scale=NA)
      sc <- getOption("ursaPngScale")
      panel_new()
      ct <- panel_raster(d["r"])
   }
   ind0 <- which(b$r<2)
   ind1 <- which(b$r>=2 & b$r<4)
   ind2 <- which(b$r>=4 & b$r<8)
   ind3 <- which(b$r>=8 & b$r<12)
   ind4 <- which(b$r>=12)
   if (FALSE)
      print(c('0'=length(ind0),'1'=length(ind1),'2'=length(ind2)
             ,'3'=length(ind3),'4'=length(ind4)))
   b0 <- b[ind0,]
   b1 <- b[ind1,]
   b2 <- b[ind2,]
   b3 <- b[ind3,]
   b4 <- b[ind4,]
   for (i in seq(nrow(b0))) {
      with(b0,{
         points(x[i],y[i],col=col)
      })
   }
   for (i in seq(nrow(b1))) {
      with(b1,{
         segments(x[i],y[i],x2[i],y2[i],col=col,lwd=lwd)
         segments(x3,y3,x4,y4,col=col,lwd=lwd)
      })
   }
   for (i in seq(nrow(b2))) {
      with(b2,{
         segments(x[i],y[i],x2[i],y2[i],col=col,lwd=lwd)
         segments(x5,y5,x6,y6,col=col,lwd=lwd)
      })
   }
   for (i in seq(nrow(b3))) {
      with(b3,{
         segments(x[i],y[i],x2[i],y2[i],col=col,lwd=lwd)
         segments(x3,y3,x4,y4,col=col,lwd=lwd)
         segments(x5,y5,x6,y6,col=col,lwd=lwd)
      })
   }
   for (i in seq(nrow(b4))) {
      with(b4,{
         segments(x[i],y[i],x2[i],y2[i],col=col,lwd=lwd)
         segments(x3,y3,x4,y4,col=col,lwd=lwd)
         segments(x5,y5,x6,y6,col=col,lwd=lwd)
         segments(x7,y7,x8,y8,col=col,lwd=lwd)
      })
   }
   if (self) {
      panel_decor()
      compose_legend(ct)
      compose_close(render=T)
   }
   session_grid(g1)
  # display(d)
   invisible(NULL)
}
