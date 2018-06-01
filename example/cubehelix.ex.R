invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   set.seed(352)
   session_grid(regrid(mul=1/16))
   a <- ursa_dummy(9,min=0,max=255)
   b4 <- b3 <- b2 <- b1 <- vector("list",length(a))
   for (i in seq_along(b1)) {
      b1[[i]] <- colorize(a[i],pal=cubehelix(11,weak=45*i,rotate=+270),ncolor=11)
      b2[[i]] <- colorize(a[i],pal=cubehelix(11,weak=45*i,rotate=-270),ncolor=11)
      b3[[i]] <- colorize(a[i]-127,pal=cubehelix)
      hue <- sample(seq(2)-1,1)
      s <- ifelse(hue==0,NA,runif(1,min=91,max=223))
      b4[[i]] <- colorize(a[i]-127,pal=cubehelix,pal.hue=hue,pal.dark=s,pal.light=s)
   }
   display(c(b1,b2),layout=c(2,NA),decor=FALSE)
   display(c(b3,b4),layout=c(2,NA),decor=FALSE)
})
