invisible({
   plutil::mysource(ursa)
   '.makeRaster' <- function(nc=6,nr=8) {
      as.ursa(t(matrix(runif(nc*nr,min=0,max=255),ncol=nc,nrow=nr)))
   }
   # ex.1 -- change grid
   session_grid(NULL)
   print(g1 <- session_grid())
   print(g2 <- regrid(g1,mul=2))
   print(g3 <- regrid(g1,res=50000,lim=c(-1200000,-1400000,1600000,1800000)))
   print(g4 <- regrid(g1,res=50000,lim=c(-1200100,-1400900,1600900,1800100),verbose=TRUE))
   print(g5 <- regrid(g1,mul=1/4))
   print(g6 <- regrid(g1,mul=1/4,cut=c(-1,-2,3,4)*25000))
   print(g7 <- regrid(g1,mul=1/4,expand=1.05)
   print(session_grid()) ## equal to 'g1'
   print(a <- regrid(g1,mul=1/4,border=3,raster=TRUE))
   print(session_grid()) ## not equal to 'g1'
   
   # ex.2 -- resize/resample
   session_grid(NULL)
   a <- .makeRaster(12,18)
   expand <- 1/3
   a1 <- regrid(regrid(a,mul=expand,resample=FALSE),a,resample=FALSE)
   a2 <- regrid(regrid(a,mul=expand,resample=TRUE),a,resample=FALSE)
   b <- c('source'=a,'contract'=a1,'aggregation'=a2)
   print(b)
   ##~ display_brick(b,grid=TRUE
                ##~ ,grid.lon=(seq(ncol(a)*expand+1)-1)/expand
                ##~ ,grid.lat=(seq(nrow(a)*expand+1)-1)/expand)
   session_grid(NULL)
   a <- .makeRaster(6,8)
   expand <- 3
   b <- c("source"=regrid(a,mul=expand,resample=FALSE,resetGrid=FALSE)
         ,"simple"=regrid(a,mul=expand,cascade=TRUE,resetGrid=FALSE)
         ,"cascaded"=regrid(a,mul=expand,cascade=FALSE,resetGrid=FALSE))
   display_brick(b)
   session_grid(a)
   eps <- 1e-4
   r <- c(0,expand^(-2)-eps,expand^(-2)+eps,1,expand^0.5
         ,(expand+2/3)^2-eps,(expand+2/3)^2+eps,99)
   g2 <- regrid(mul=expand)
   session_grid(g2)
   b <- ursa_new(bandname=sprintf("Resample=%.4f",r))
   for (i in seq(b))
      b[i] <- regrid(a,g2,resample=r[i])
   print(b)
   display_brick(b,layout=c(2,NA)
                ,grid=TRUE,grid.lon=seq(ncol(a)+1)-1,grid.lat=seq(nrow(a)+1)-1)
})
