'panel_cluster' <- function(obj,cut=1,cex=1,overlap=1.05,ratio=0.2,col=NULL
                           ,method=c("complete","centroid","single")
                           ,fun=c("count","sum","mean"),label=fun %in% "count"
                           ,ngroup=NA,separate=FALSE,repel=20L,legend="bottomright") {
   ##~ method <- c('1'="ward.D",'2'="ward.D2",'3'="single",'4'="complete"
              ##~ ,'5'="average",'6'="mcquitty",'7'="median"
              ##~ ,'8'="centroid")[4] ## 3 4! 8 
   method <- match.arg(method)
   fun <- match.arg(fun)
   da <- spatial_data(obj)
   if (!is.null(da)) {
      indCat <- which(sapply(colnames(da),function(x)
                                     inherits(da[,x],c("character","factor"))))[1]
      isCat <- !is.na(indCat)
      if (fun %in% c("count")) {
         indNum <- which(sapply(colnames(da),function(x)
                                        inherits(da[,x],c("integer"))))[1]
         isNum <- !is.na(indNum)
      }
      else
         isNum <- FALSE
   }
   else {
      isCat <- FALSE
      isNum <- FALSE
   }
   if (isCat) {
      aname <- obj[[indCat]]
      nameCat <- colnames(da)[indCat]
   }
   else {
      if (separate)
         separate=F
      legend <- NULL
   }
  # print(indCat)
  # print(indNum)
   g1 <- getOption("ursaPngPanelGrid")
   xy <- spatial_coordinates(spatial_transform(spatial_geometry(obj),ursa_crs(g1)))
   xy <- cbind(xy,da)
   n <- if (!isNum) rep(1L,spatial_count(obj)) else obj[[indNum]]
   xy4 <- xy[do.call(c,lapply(seq_along(n),function(i) rep(i,n[i]))),,drop=FALSE]
   rownames(xy4) <- NULL
  # xy4[[colnames(da)[indNum]]] <- 1L
  # print(table(as.integer(rownames(xy4))))
   cell <- ursa(g1,"cellsize")
  # .ursaOptions()
   scale <- getOption("ursaPngScale")
   dpi <- getOption("ursaPngDpi")
   ps <- getOption("ursaPngPointsize")
   retina <- getOption("ursaPngRetina")
   s <- unname((cex*c(annotation=1.5))*cut*ps/scale*cell*dpi/96*sqrt(2))
   if (!label)
      s <- s*0.5
   print(data.frame(cell=cell,retina=retina,scale=scale,cut=cut,dpi=dpi,ps=ps,cex=cex,s=s))
   if (isCat)
      bname <- if (is.factor(aname)) levels(aname) else unique(aname)
   else {
      indFun <- which(sapply(colnames(da),function(x)
                                     inherits(da[,x],c("integer","numeric"))))[1]
      if (!is.na(indFun))
         bname <- names(indFun)
      else
         bname <- ".undefined"
   }
   if ((separate)&&(isCat)&&(!is.na(ngroup))&&(length(ngroup)!=length(bname))&&(ngroup[1]>2)) {
      tg <- table(xy4[[nameCat]])
     # print(tg)
     # mul <- ngroup/max(tg)
     # ngroup <- ceiling(as.numeric(tg)*mul)
     # print(length(tg))
     # print(ngroup)
      if (any(nchar(names(ngroup)))) {
         ng <- ngroup
         ngroup <- rep(NA,length(tg))
         names(ngroup) <- names(tg)
         if (length(ind <- match(names(ng),names(ngroup))))
            ngroup[ind] <- ng
      }
      else {
         ngroup <- rep(ngroup,length(tg))
         names(ngroup) <- names(tg)
      }
     # print(ngroup)
   }
  # print(ngroup)
   lutList <- lapply(if (separate) bname else ".+",function(sep) {
     # message(sQuote(sep),":")
     # print(ngroup[sep])
     # return(NULL)
     # str(xy4)
     # str(grep(paste0("^",sep,"$"),xy4[[nameCat]]))
      if (isCat) {
        # xy5 <- xy4[grep(paste0("^",sep,"$"),xy4[[nameCat]]),]
         xy5 <- if (separate) xy4[xy4[[nameCat]] %in% sep,] else xy4
      }
      else
         xy5 <- xy4
      if (nrow(xy5)<2) {
         chcD <- 1L
      }
      else {
         len <- dist(xy5[,c("x","y")])
        # str(xy5)
        # print(summary(len))
         chc <- hclust(len,method=method)
         if ((length(ngroup)>1)&&(!is.null(names(ngroup))))
            ng <- ngroup[sep]
         else
            ng <- ngroup
         if (min(len[len>0])>s*0.75)
            ng <- NA
        # print(data.frame(ng=ng,len=min(len[len>0])))
         if (!is.na(ng)) {
            chcD <- cutree(chc,k=min(c(ng,nrow(xy5))))
         }
         else {
            chcD <- cutree(chc,h=s)
         }
      }
      ta <- table(chcD)
      ##~ print(ta)
      ##~ print(table(cutree(chc,h=s)))
      ##~ print(nrow(xy5))
      ##~ print(sum(ta))
     # pal <- paste0(cubehelix(length(bname),dark=127,light=127,rotate="circle"),"A0")
      lut <- array(0L,dim=c(length(ta),length(bname)),dimnames=list(names(ta),bname))
      lut <- cbind(.x=NA,.y=NA,.r=NA,.n=NA,data.frame(lut,check.names=FALSE))
      xy5 <- data.frame(xy5,.cluster=chcD)
      for (i in seq(nrow(lut))) {
         da2 <- xy5[xy5$.cluster==i,]#c("x","y")]
        # print(da2)
         if (isCat) {
            ta2 <- table(da2[[nameCat]])
            lut[i,match(names(ta2),colnames(lut))] <- as.integer(ta2)
         }
         else {
            if ((bname!=".undefined")&&(fun %in% c("mean"))) {
               lut[i,bname] <- mean(da2[[bname]])
            }
            else if ((bname!=".undefined")&&(fun %in% c("sum"))) {
               lut[i,bname] <- sum(da2[[bname]])
            }
            else
               lut[i,bname] <- nrow(da2)
         }
         lut$.n[i] <- nrow(da2)
         lut$.x[i] <- mean(da2$x)
         lut$.y[i] <- mean(da2$y)
      }
      lut <- aggregate(lut[,-grep("^\\.[xy]",colnames(lut))]
                      ,by=list(.x=lut$.x,.y=lut$.y),sum)
      lut
   })
   lut <- do.call(rbind,lutList)
   rownames(lut) <- NULL
   lut$.r <- lut$.n^ratio # rowSums(lut[,bname,drop=FALSE])^ratio
   lut$.r <- lut$.r/min(lut$.r)
   if (repel) {
      if (isTRUE(repel))
         repel <- 20L
      else
         repel <- as.integer(repel)
     # S <- 1+dist(lut$.r)
     # print(lut)
     # gr <- expand.grid(i=seq(nrow(lut)),j=seq(nrow(lut)),KEEP.OUT.ATTRS=FALSE)
      S <- 0.5*rowSums(expand.grid(a=lut$.r,b=lut$.r,KEEP.OUT.ATTRS=FALSE))
      S <- as.dist(matrix(S,nrow=nrow(lut),byrow=T))
      S <- as.dist(S)
      xy <- as.matrix(lut[,c(".x",".y")])
      dimnames(xy) <- NULL
     # xy <- cbind(xy,S)
      d <- s/20
      iter <- 100
      R2 <- 0.5*s*overlap
      k <- 0L
      isProgress <- FALSE
      repeat({
         D <- dist(xy)/S
         ind1 <- which(D<2*R2)
         ind1 <- ind1[!.is.eq(D[ind1],2*R2)]
         if (!length(ind1))
            break
         if (!k) {
           # str(ind1)
            iter <- repel*length(ind1)
            pb <- ursaProgressBar(iter,title="Repel clusters")
            isProgress <- TRUE
         }
         if (isProgress)
            setUrsaProgressBar(pb)
         ind2 <- .sample(ind1,1)
         D1 <- as.matrix(D)
         ind3 <- match(D[ind2],c(D1))
         j <- c(col(D1))[ind3]
         i <- c(row(D1))[ind3]
         xy2 <- xy[c(i,j),]
         dxy <- c(diff(xy2[,1]),diff(xy2[,2]))
         L <- sqrt(sum(dxy^2))
         d2 <- if (T | L/2+d/2<R2) d else (2*R2-L)/2
         alpha <- atan2(dxy[2],dxy[1])
         xy2[,1] <- xy2[,1]+c(-d2,d2)*cos(alpha)
         xy2[,2] <- xy2[,2]+c(-d2,d2)*sin(alpha)
         xy[c(i,j),] <- xy2
         if (k>iter)
            break
         k <- k+1L
      })
      if (isProgress)
         close(pb)
     # print(c(convergent=k,niter=iter))
      lut[,c(".x",".y")] <- xy
   }
   lut <- lut[order(lut$.r,decreasing=TRUE),]
   d <- dist(lut[,c(".x",".y")])
   indCrd <- grep("^\\.[xy]$",colnames(lut))
   if (F) {
      p <- spatial_buffer(spatialize(lut[,indCrd],coords=c(".x",".y")
                                    ,crs=ursa_crs(g1)),s/2)
      spatial_write(p,"C:/platt/R/ursa-package/run/panel_cluster/mammal.geojson")
      q()
   }
   if (is.character(col)) {
      ct <- ursa_colortable(colorize(bname
                             ,pal=rep(col,length.out=length(bname)),alpha="A0"))
      ctInd <- ct
   }
   else if (fun %in% c("count")) {
      ct <- colorize(bname,alpha="A0"
                                    ,pal.dark=127,pal.light=127,pal.rotate="circle"
                                    )
      ctInd <- ursa_colortable(ct)
   }
   else {
      ct <- colorize(lut[[bname]],stretch="linear",alpha="FF")
      ctInd <- ursa_colortable(ct)[ursa_colorindex(ct)]
   }
   bg <- if (separate) "white" else ctInd
   if (length(bg)==1)
      bg <- rep(bg,length(ctInd))
   bg <- col2rgb(bg)/255
   bg <- rgb(bg[1,],bg[2,],bg[3,],alpha=0.2)
   s2 <- s/ifelse(label,2,1.5)/cut
   for (i in seq(nrow(lut))) {
      x <- lut$.x[i] # <- mean(da2$x)
      y <- lut$.y[i] # <- mean(da2$y)
      r <- lut$.r[i]
      if (fun %in% "count") {
         v <- as.integer(lut[i,bname])
         .panel_pie(v,x=x,y=y,radius=lut$.r[i]*s2,col=ctInd,bg=bg,ball=!label) # lwd=0.5
      }
      else {
         .panel_pie(1,x=x,y=y,radius=lut$.r[i]*s2,col=ctInd[i],ball=!label)
      }
      p <- sf::st_as_sf(lut[i,],coords=c(".x",".y"),crs=ursa_crs(g1))
      if (F)
         panel_plot(spatial_buffer(p,s/2),col="transparent",border="grey40",lwd=0.5)
     # panel_plot(spatial_buffer(p,s/2),col="transparent",border="white",lwd=0.5)
      if (F) ## donut/bagel
         panel_plot(spatial_buffer(p,(c(s-10*cell*cex,0.75*s*r)[2])/2)
                   ,col="#FFFFFFAF",border="transparent")
      if (label) {
         if (fun %in% "count")
            lab <- sum(v)
         else
            lab <- lut[i,bname]
         panel_annotation(x=x,y=y,label=as.character(lab),cex=cex,adj=c(0.5,0.53)
                        # ,fg="#FFFFFFA0",bg="#000000AF"
                        # ,buffer=2/scale
                         )
      }
      if (F) {
         da2 <- xy4[xy4$.cluster==i,]
         for (j in seq(nrow(da2)))
            segments(x,y,da2$x[j],da2$y[j],col="#00000030",lwd=0.5)
      }
   }
   if (!is.null(legend))
      legend(legend,legend=bname
            ,col=ctInd,cex=c(1,cex)[1]/par("cex")
            ,pch=21,pt.lwd=1*2.4/par("cex"),pt.cex=1.8/par("cex")
            ,box.lwd=0.1,bg="#FFFFFF7F"
           # ,pt.bg=ursa_colortable(colorize(seq_along(ctInd),pal=ctInd,alpha="30"))
            ,pt.bg=if (label) bg else ctInd
            )
   return(invisible(ct)) ## colortable of obj[[indCat]]
}
'.panel_pie' <- function(z,x=0,y=0,radius=1,edges=200,clockwise=TRUE,init.angle=90
                        ,col=NULL,bg="white"
                        ,border="white",lty=NULL,lwd=NULL,ball=FALSE) {
   if (!is.numeric(z) || any(is.na(z) | z < 0)) 
       stop("'z' values must be positive.")
   g0 <- getOption("ursaPngPanelGrid")
   cell <- ursa(g0,"cellsize")
   z <- z/sum(z)
   ind <- which(z>0)
   mul <- cell/radius
  # print(c(cell=cell,radius=radius,mul=radius/cell))
   if (any(z[ind]<mul)) {
      z[ind] <- z[ind]+mul
      z <- z/sum(z)
   }
   z <- c(0,cumsum(z))
   dz <- diff(z)
   nz <- length(dz)
   if (max(dz)==0) {
      border <- "transparent"
     # if (!is.null(lwd))
     #    lwd <- NULL
   }
   pin <- par("pin")
   usr <- par("usr")
   asp <- c((pin[2]/(usr[4]-usr[3]))/(pin[1]/(usr[2]-usr[1])),1)
   if (is.null(col))
      col <- ursa_colortable(colorize(seq(nz)))
   if (!is.null(border)) 
      border <- rep_len(border, nz)
   if (!is.null(lty)) 
      lty <- rep_len(lty, nz)
   if (!is.null(lwd)) 
      lwd <- rep_len(lwd, nz)
   twopi <- if (clockwise) -2*pi else 2*pi
   t2xy <- function(t,scale=c(1,0.7)) {
      t2p <- twopi*t+init.angle*pi/180
      if (max(dz)==1) {
         xp <- c(asp[1]*radius*scale[1]*cos(t2p)+x)
         yp <- c(asp[2]*radius*scale[1]*sin(t2p)+y)
      }
      else {
         xp <- c(0+x,asp[1]*radius*scale[1]*cos(t2p)+x,0+x)
         yp <- c(0+y,asp[2]*radius*scale[1]*sin(t2p)+y,0+y)
      }
      if (length(scale)>1) {
         if (max(dz)==1) {
            xp <- c(xp,NA,asp[1]*radius*scale[2]*cos(t2p)+x)
            yp <- c(yp,NA,asp[2]*radius*scale[2]*sin(t2p)+y)
         }
         else {
            xp <- c(xp,NA,0+x,asp[1]*radius*scale[2]*cos(t2p)+x,0+x)
            yp <- c(yp,NA,0+y,asp[2]*radius*scale[2]*sin(t2p)+y,0+y)
         }
      }
      list(x=xp,y=yp)
   }
   col1 <- col # ursa_colortable(colorize(seq_along(col),pal=col,alpha="A0"))
   if (!is.character(bg))
      col2 <- ursa_colortable(colorize(seq_along(col),pal=col,alpha="30"))
   else {
      col2 <- bg
   }
   for (i in seq_len(nz)) {
      n <- max(2,floor(edges*dz[i]))
      if (!ball) {
         P <- t2xy(seq.int(z[i],z[i+1],length.out=n),scale=c(1,0.7))
         ##~ polygon(c(P$x,0+x),c(P$y,0+y),border=border[i],col=col[i]
                ##~ ,lty=lty[i],lwd=lwd[i]
                ##~ )
         polypath(P$x,P$y,border=border[i],col=col1[i]
                 ,lty=lty[i],lwd=lwd[i]
                 ,rule=c("winding","evenodd")[2]
                 )
      }
      P <- t2xy(seq.int(z[i],z[i+1],length.out=n),scale=ifelse(ball,0.7,0.7))
      polypath(P$x,P$y,border=border[i],col=if (ball) col1[i] else col2[i]
              ,lty=lty[i],lwd=lwd[i]
              ,rule=c("winding","evenodd")[2]
              )
   }
   col
}
