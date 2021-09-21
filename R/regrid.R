##~ 'resize' <- function(x,scale=1,grid=NULL,cover=0.5-1e-3,resample=1
                      ##~ ,resetGrid=TRUE,verbose=0L)
'regrid' <- function(x,...)
{
  # print("resize")
   arglist <- list(...)
   if (missing(x)) {
      result <- .regrid(...)
      resetGrid <- .getPrm(arglist,name="^reset",default=TRUE)
      if (resetGrid)
         session_grid(result)
      return(result)
   }
   fun <- "resize" #as.character(match.call())[1]
   if (.is.ursa_stack(x)) {
      return(lapply(x,regrid,...))
   }
   if (!is.ursa(x)) {
      return(.regrid(x,...))
     # return(NULL)
   }
  # grid <- do.call("regrid",as.list(match.call())[-1])
  # session_grid(x) ## removed 20160619
   cond <- .getPrm(arglist,class="ursaRaster",default=NULL)
   if ((FALSE)&&((is.ursa(cond))||(length(arglist)==0))) { ## or don't include '!length'?
      return(ursa_crop(x,...))
   }
   else
      g2 <- do.call(".regrid",arglist)
   session_grid(x) ## added 20160619
   resetGrid <- TRUE
   cover <- NA
   resample <- 1
   verbose <- 0L
   cascade <- FALSE
   myname <- names(arglist)
   for (i in seq_along(arglist)) {
      a <- arglist[[i]]
      n <- myname[i]
      if (is.ursa(arglist[[i]]))
         g2 <- ursa_grid(a)
      else if (.lgrep("^reset",n))
         resetGrid <- as.logical(a)
      else if (.lgrep("resample",n))
         resample <- as.numeric(a)
      else if (.lgrep("cover",n))
         cover <- as.numeric(a)
      else if (.lgrep("cascade",n))
         cascade <- as.logical(a)
      else if (.lgrep("verb(ose)*",n))
         verbose <- as.integer(a)
   }
   if (is.na(cover))
      cover <- 0.5-1e-3
   g1 <- x$grid
  # isCT <- x$category # .is.category(x)
   ct <- x$colortable
   isCT <- length(ct)>0
   clValue <- class(x$value)
   smValue <- storage.mode(x$value)
   x <- .extract(x)
   if (cascade) {
      m0 <- sqrt((g1$resx*g1$resy)/(g2$resx*g2$resy))
      mul <- NULL
      for (i in seq(1000)) {
         m2 <- c(mul,2)
         if (prod(m2)>m0)
            break
         mul <- m2
      }
      mul <- c(mul,m0/prod(mul))
      for (m in head(mul,-1)) {
         x <- regrid(x,mul=m,resample=resample,cover=cover,verbose=verbose)
      }
      res <- regrid(x,g2,resample=resample,cover=cover,verbose=verbose)
      if (!resetGrid)
         session_grid(g1)
      return(res)
   }
   if (!is.na(x$con$posZ[1]))
      nb <- length(x$con$posZ)
   else
      nb <- x$dim[2]
   dimx <- with(g1,c(columns,rows,nb))
   dimy <- with(g2,c(columns,rows,nb))
   dim(x$value) <- dimx
   x$value <- as.numeric(x$value)
   if (verbose>1)
      print(summary(x$value))
   if (verbose>2)
      .elapsedTime(paste0("start:nodata-assing:",fun))
   nodata <- x$con$nodata
   missedNodata <- is.na(nodata)
   if (missedNodata)
     # nodata <- max(x$value,na.rm=TRUE)+1
      nodata <- .optimal.nodata(x$value)
   x$value[is.na(x$value)] <- nodata
   if (verbose>2)
      .elapsedTime(paste0("finish:nodata-assing:",fun))
   if (verbose>1)
      print(summary(x$value))
   if (verbose)
      .elapsedTime(paste0("start:",fun))
   session_grid(g2)
  # isCT <- .is.colortable(x$colortable)
   y <- as.ursa(NA,bandname=bandname(x),nodata=nodata) ## ursa_new
   ##~ if (isCT)
      ##~ y$colortable <- x$colortable
   y$value <- .Cursa("resampl4",x=x$value,nodata=as.numeric(nodata)
                ,dim1=as.integer(dimx),dim2=as.integer(dimy)
                ,lim1=as.numeric(with(g1,c(minx,miny,maxx,maxy)))
                ,lim2=as.numeric(with(g2,c(minx,miny,maxx,maxy)))
                ,cover=as.numeric(cover),area=as.numeric(resample)
                ,verbose=as.integer(verbose)
                ,res=numeric(prod(dimy)),NAOK=FALSE)$res
   if (verbose)
      .elapsedTime(paste0("finish:",fun))
   if (verbose>1)
      print(summary(y$value))
   if (verbose>2)
      .elapsedTime(paste0("start:nodata-restore:",fun))
   if (TRUE) ## added 20160406
      y$value[.is.eq(y$value,nodata)] <- NA
   else { ## deprecated
      if (abs(nodata)<1)
         y$value[abs(y$value-nodata)<1e-27] <- NA
      else
         y$value[abs(y$value/nodata-1)<1e-6] <- NA
   }
   if (verbose>2)
      .elapsedTime(paste0("finish:nodata-restore:",fun))
   if (isCT) {
      y <- reclass(discolor(y),ct)
   }
   if (verbose>1)
      print(summary(y$value))
   if (FALSE) {
      if (FALSE) {
         if ((resample==0)&&(smValue=="integer")) {
            if (TRUE) ## quick
               storage.mode(y$value) <- "integer"
            else
               y$value <- as.integer(round(y$value))
         }
         if ((resample==0)&&(isCT)) {
            class(y$value) <- "ursaCategory"
         }
         else
            class(y$value) <- "ursaNumeric"
      }
      else
         class(y$value) <- "ursaNumeric"
   }
   dim(y$value) <- with(g2,c(columns*rows,nb))
   if (TRUE) { ## added 20170524
      if (resample==0) {
         class(y$value) <- clValue
         storage.mode(y$value) <- smValue
      }
      else
         class(y$value) <- "ursaNumeric"
   }
  # if ((.is.colortable(x$colortable))&&(length(unique(y$value))==length(x$colortable)))
  #    y$colortable <- x$colortable
   if (missedNodata)
      y$con$nodata <- NA
   if (!resetGrid)
      session_grid(g1)
   y
}
'.regrid' <- function(grid=NULL,mul=NA,res=NA,resx=NA,resy=NA
                             ,setbound=NA,columns=NA,rows=NA,dim=NA
                             ,bbox=NA,expand=NA
                             ,minx=NA,miny=NA,maxx=NA,maxy=NA,cut=NA
                             ,proj4=NA,crs=NA,border=0
                             ,zero=c("keep","node","center")
                             ,raster=FALSE,tolerance=NA #1e-10
                             ,zoom=NA
                             ,verbose=FALSE,...)
{
   if (is.character(border)) ## cuttof 'border' in 'plot' functions
      border <- 0
  # print("regrid")
  # verbose <- TRUE
   if (length(arglist <- list(...))) {
      res <- .getPrm(arglist,name="cell",default=as.numeric(res)) ## cell, cellsize -> res
   }
   mtol <- 1e5 # [1e2->1e5 20170720]
   etol <- 1e-14
   zero <- match.arg(zero)
   if (missing(grid)) {
      checkZero <- FALSE
      if (verbose)
         message("grid is missing")
      g <- session_grid()
   }
   else if (is.null(grid)) {
      checkZero <- FALSE
      if (verbose)
         message("grid is NULL")
      g <- .grid.skeleton()
   }
   else if (is.ursa(grid)) {
      checkZero <- TRUE
      if (verbose)
         message("grid from raster")
      g <- ursa_grid(grid)
   }
   else if ((is.character(grid))&&(envi_exists(grid,exact=TRUE))) {
      checkZero <- TRUE
      if (verbose)
         message("grid from ENVI")
      g <- ursa_grid(grid)
   }
   else {
      checkZero <- TRUE
      if (verbose)
         message("grid as is")
      g <- grid
   }
   if (FALSE) {
      x <- seq(g,"x")
      y <- seq(g,"y")
      print(x)
      print(y)
   }
   step1 <- TRUE
   dima <- length(dim)
   if (length(dim)==2) {
      rows <- dim[1]
      columns <- dim[2]
   }
   if (!is.na(setbound)[1]) {
      setbound <- rep(setbound,length=4)
      g$minx <- unname(setbound[1])
      g$miny <- unname(setbound[2])
      g$maxx <- unname(setbound[3])
      g$maxy <- unname(setbound[4])
      toDefine <- 0L
     # if ((is.na(g$columns))&&(!is.na(columns))) { ## -- 20170613
      if (!is.na(columns)) { ## ++ 20170613
         g$columns <- as.integer(round(columns))
         toDefine <- toDefine+1L
      }
     # if ((is.na(g$rows))&&(!is.na(rows))) { ## -- 20170613
      if (!is.na(rows)) { ## ++ 20170613
         g$rows <- as.integer(round(rows))
         toDefine <- toDefine+1L
      }
      if (toDefine<2)
         toDefine <- FALSE
      if ((!toDefine)&&(!is.na(g$columns))&&(!is.na(g$columns)))
         toDefine <- TRUE
      if (toDefine) {
         g$resx <- with(g,(maxx-minx)/columns)
         g$resy <- with(g,(maxy-miny)/rows)
      }
      if (zero=="node") {
         if (verbose)
            print("setbound:zero:node")
         g$minx <- round(g$minx/g$resx)*g$resx
         g$maxx <- round(g$maxx/g$resx)*g$resx
         g$miny <- round(g$miny/g$resy)*g$resy
         g$maxy <- round(g$maxy/g$resy)*g$resy
      }
   }
   ##~ if (verbose) {
      ##~ print(g)
   ##~ }
   if ((is.numeric(zoom))&&(!is.numeric(mul))&&(!is.numeric(expand))) {
      expand <- zoom
      mul <- 1/zoom
      zoom <- NA
   }
   if (!is.na(cut[1])) {
      if (length(cut)==1)
         cut <- cut*c(-1,-1,1,1)
      cut <- rep(cut,length=4)
      bbox <- with(g,c(minx,miny,maxx,maxy))+cut
   }
   if (!anyNA(expand)) {
      x0 <- (g$minx+g$maxx)/2
      y0 <- (g$miny+g$maxy)/2
      sx <- (g$maxx-g$minx)/2
      sy <- (g$maxy-g$miny)/2
      if (length(expand)==1) {
         expand <- rep(expand,length.out=2)
         s <- sqrt(sx*sy)
         if (T) {
            dx <- sx+round(s*(expand[1]-1)/g$resx*2)*g$resx/2
            dy <- sy+round(s*(expand[2]-1)/g$resy*2)*g$resy/2
         }
         else { ## deprecated
            dx <- sx+s*(expand[1]-1)
            dy <- sy+s*(expand[2]-1)
         }
      }
      else {
         dx <- (expand[1])*sx
         dy <- (expand[2])*sy
      }
     # print(with(g,c(minx,miny,maxx,maxy)))
      bbox <- c(x0-dx,y0-dy,x0+dx,y0+dy)
     # print(bbox)
   }
   if ((is.na(res))&&(!is.na(resx))&&(!is.na(resy)))
      res <- c(resx,resy)
   if (is.numeric(mul))
   {
      g$resx <- g$resx/mul
      g$resy <- g$resy/mul
   }
   else if ((!is.na(res[1]))&&(is.numeric(res[1])))
   {
      if (length(res)==2)
      {
         g$resx <- res[1]
         g$resy <- res[2]
      }
      else
         g$resx <- g$resy <- res
      if ((!is.na(bbox[1]))&&(length(bbox)==4))
         step1 <- FALSE
   }
   else
      step1 <- FALSE
   if (step1)
   {
      if (all(!is.na(c(minx,miny,maxx,maxy)))) {
         g$minx <- minx
         g$miny <- miny
         g$maxx <- maxx
         g$maxy <- maxy
         minx <- miny <- maxx <- maxy <- NA
      }
      if (is.na(tolerance)) {
         tolx <- .Machine$double.eps*max(abs(c(g$minx,g$maxx)))*mtol
         toly <- .Machine$double.eps*max(abs(c(g$miny,g$maxy)))*mtol
         if (tolx<etol)
            tolx <- etol
         if (toly<etol)
            toly <- etol
         tolerance <- min(tolx,toly)
      }
      else {
         tolx <- toly <- tolerance
      }
      c0 <- with(g,(maxx-minx)/resx)
      r0 <- with(g,(maxy-miny)/resy)
     # r0 <- with(g,((maxy-meany)-(miny-meany))/resy)
      if ((!.is.integer(r0,toly))||(!.is.integer(c0,tolx))) {
         if (verbose) {
            message("#1. 'bbox' is changed to integerity of matrix dimension")
         }
         g$minx <- floor(g$minx/g$resx)*g$resx
         g$maxx <- ceiling(g$maxx/g$resx)*g$resx
         g$miny <- floor(g$miny/g$resy)*g$resy
         g$maxy <- ceiling(g$maxy/g$resy)*g$resy
         g$columns <- with(g,(maxx-minx)/resx)
         g$rows <- with(g,(maxy-miny)/resy)
      }
      else {
         g$columns <- as.integer(round(c0))
         g$rows <- as.integer(round(r0))
      }
      if ((!.is.integer(g$columns,tolx))||(!.is.integer(g$rows,toly))) {
         print(c(dc=g$columns-round(g$columns),dr=g$rows-round(g$rows)))
         stop(paste("#1. Unable to calculate integer dim size."
                   ,"Try to change 'tolerance'"
                   ,paste0("(",format(tolerance,digits=1),")")))
      }
      g$columns <- as.integer(round(g$columns))
      g$rows <- as.integer(round(g$rows))
   }
   step2 <- FALSE
   g2 <- g
   if ((!is.na(bbox[1]))&&(length(bbox)==4))
   {
      g$minx <- unname(bbox[1])
      g$miny <- unname(bbox[2])
      g$maxx <- unname(bbox[3])
      g$maxy <- unname(bbox[4])
      step2 <- TRUE
   }
   else if (!is.na(cut[1])) {
      if (length(cut)==1)
         cut <- cut*c(-1,-1,1,1)
      cut <- rep(cut,length=4)
      g$minx <- g$minx+cut[1]
      g$miny <- g$miny+cut[2]
      g$maxx <- g$maxx+cut[3]
      g$maxy <- g$maxy+cut[4]
      step2 <- TRUE
   }
  # else ## not neccessary: initial value is FALSE
  #    step2 <- FALSE 
   if (!is.na(minx))
      g$minx <- minx
   if (!is.na(miny))
      g$miny <- miny
   if (!is.na(maxx))
      g$maxx <- maxx
   if (!is.na(maxy))
      g$maxy <- maxy
   if ((!step2)&&(any(!is.na(c(minx,miny,maxx,maxy)))))
      step2 <- TRUE
   if ((!FALSE)&&(!is.na(columns))&&(!is.na(rows))) { ## ++ 20170719
      if ((FALSE)&&(!.isPackageInUse()))
         message("Not a good idea to define cell size from image dimension")
      if ((is.na(g$columns))||(g$columns!=columns))
         g$columns <- as.integer(round(columns))
      if ((is.na(g$rows))||(g$rows!=rows))
         g$rows <- as.integer(round(rows))
      g$resx <- with(g,(maxx-minx)/columns)
      g$resy <- with(g,(maxy-miny)/rows)
   }
   if (step2)
   {
      if (zero=="node") {
         if (verbose)
            print("step2:zero=node")
         g$minx <- round(g$minx/g$resx)*g$resx
         g$maxx <- round(g$maxx/g$resx)*g$resx
         g$miny <- round(g$miny/g$resy)*g$resy
         g$maxy <- round(g$maxy/g$resy)*g$resy
      }
      else if ((T & checkZero)&&(zero=="keep")&&
               (!is.na(g2$minx))&&(!is.na(g2$maxx))&&
               (!is.na(g2$miny))&&(!is.na(g2$maxy))) {
         if (verbose)
            print("step2:zero=keep")
         if (F) {
            str(as.list(match.call()))
            message("g")
            print(g,digits=12)
            message("g2")
            print(g2,digits=12)
            q()
         }
         x <- seq(g2,"x")
         y <- seq(g2,"y")
         if (verbose) {
            comment(verbose) <- "seq"
            print(series(x))
            print(series(y))
         }
         if (!TRUE) {
            indX <- which(x>=g$minx-g$resx/2 & x<=g$maxx+g$resx/2)
            indY <- which(y>=g$miny-g$resy/2 & y<=g$maxy+g$resy/2)
            if ((length(indX))&&(length(indY))) {
               if (verbose)
                  message("matched")
              # print(range(x[indX])+c(-1,1)*g$resx/2)
              # print(range(y[indY])+c(-1,1)*g$resy/2)
               g$minx <- min(x[indX])-g$resx/2
               g$maxx <- max(x[indX])+g$resx/2
               g$miny <- min(y[indY])-g$resy/2
               g$maxy <- max(y[indY])+g$resy/2
               if (F) {
                  x <- seq(g,"x")
                  y <- seq(g,"y")
                  print(series(x))
                  print(series(y))
               }
            }
            else if (verbose)
               message("unmatched")
         }
         else { ## deprecate???
            shift <- c(0,1)[1]
            indMinX <- which(x<=g$minx-shift*g$resx/2)
            indMaxX <- which(x>=g$maxx+shift*g$resx/2)
            indMinY <- which(y<=g$miny-shift*g$resy/2)
            indMaxY <- which(y>=g$maxy+shift*g$resy/2)
            if ((length(indMinX))&&(length(indMaxX))&&(length(indMinY))&&(length(indMaxY))) {
               if (verbose) {
                  message("matched")
               }
               ##~ print(indMinX)
               ##~ print(indMaxX)
               ##~ print(indMinY)
               ##~ print(indMaxY)
               g$minx <- max(x[indMinX])-g$resx/2
               g$maxx <- min(x[indMaxX])+g$resx/2
               g$miny <- max(y[indMinY])-g$resy/2
               g$maxy <- min(y[indMaxY])+g$resy/2
               if (F) {
                  x <- seq(g,"x")
                  y <- seq(g,"y")
                  print(series(x))
                  print(series(y))
               }
            }
            else if (verbose)
               message("unmatched")
         }
      }
      c0 <- with(g,(maxx-minx)/resx)
      r0 <- with(g,(maxy-miny)/resy)
      if (is.na(tolerance)) {
         tolx <- .Machine$double.eps*max(abs(c(g$minx,g$maxx)))*mtol
         toly <- .Machine$double.eps*max(abs(c(g$miny,g$maxy)))*mtol
         if (tolx<etol)
            tolx <- etol
         if (toly<etol)
            toly <- etol
         tolerance <- min(tolx,toly)
      }
      else {
         tolx <- toly <- tolerance
      }
      if ((!.is.integer(r0,toly))||(!.is.integer(c0,tolx))) {
         if (verbose)
            message("#2. 'bbox' is changed to integerity of matrix dimension")
         g$minx <- floor(g$minx/g$resx)*g$resx
         g$maxx <- ceiling(g$maxx/g$resx)*g$resx
         g$miny <- floor(g$miny/g$resy)*g$resy
         g$maxy <- ceiling(g$maxy/g$resy)*g$resy
         g$columns <- with(g,(maxx-minx)/resx)
         g$rows <- with(g,(maxy-miny)/resy)
      }
      else {
         g$columns <- as.integer(round(c0))
         g$rows <- as.integer(round(r0))
      }
      if ((!.is.integer(g$columns,tolx))||(!.is.integer(g$rows,toly))) {
        # print(g)
        # if (verbose)
            print(c(dc=g$columns-round(g$columns),dr=g$rows-round(g$rows)
                   ,tolx=tolx,toly=toly))
         stop(paste("#2. Unable to calculate integer dim size."
                   ,"Try to change 'tolerance'"
                   ,paste0("(",format(tolerance,digits=1),")")))
      }
      g$columns <- as.integer(round(g$columns))
      g$rows <- as.integer(round(g$rows))
      if ((verbose)&&(is.character(comment(verbose)))&&(comment(verbose)=="seq")) {
         print(seq(g,"x"))
         print(seq(g,"y"))
      }
   }
  # str(list(crs=crs,crs=proj4,'g$crs'=g$crs))
   if ((is.na(proj4))&&(!is.na(crs)))
      proj4 <- crs
   if (FALSE) {
      if (is.character(proj4))
         g$crs <- proj4
      else if (is.numeric(proj4))
         g$crs <- .epsg2proj4(proj4,force=!TRUE,verbose=verbose)
   }
   else if ((!is.na(proj4))&&(!identical(g$crs,proj4))) {
      g$crs <- spatial_crs(proj4)
   }
  # else {
  #    message("skip")
  # }
   if (is.na(g$crs))
      g$crs <- ""
   if (any(border!=0))
   {
      border <- round(rep(border,length=4))
      g$minx <- with(g,minx-border[2]*resx)
      g$miny <- with(g,miny-border[1]*resy)
      g$maxx <- with(g,maxx+border[4]*resx)
      g$maxy <- with(g,maxy+border[3]*resy)
      g$columns <- with(g,(maxx-minx)/resx)
      g$rows <- with(g,(maxy-miny)/resy)
     # print("STEP3")
      if (is.na(tolerance)) {
         tolx <- .Machine$double.eps*max(abs(c(g$minx,g$maxx)))*mtol
         toly <- .Machine$double.eps*max(abs(c(g$miny,g$maxy)))*mtol
         if (tolx<etol)
            tolx <- etol
         if (toly<etol)
            toly <- etol
         tolerance <- min(tolx,toly)
      }
      else {
         tolx <- toly <- tolerance
      }
      if ((!.is.integer(g$columns,tolx))||(!.is.integer(g$rows,toly))) {
         verbose <- TRUE
         if (verbose) {
            print(g,digits=15)
         }
         print(c(dc=g$columns-round(g$columns),dr=g$rows-round(g$rows)
                ,tolx=tolx,toly=toly))
         stop(paste("#3. Unable to calculate integer dim size."
                   ,"Try to change 'tolerance'"
                   ,paste0("(",format(tolerance,digits=1),")")))
      }
      g$columns <- as.integer(round(g$columns))
      g$rows <- as.integer(round(g$rows))
   }
   g$seqx <- numeric()
   g$seqy <- numeric()
   if (!raster)
      return(invisible(g))
   session_grid(g)
   ursa_new()
}
