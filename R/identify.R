'value_xy' <- function(obj,...) .getValue(obj,.getIndex(obj,...))
'value_ll' <- function(obj,...) {
   if (!is.ursa(obj))
      return(NULL)
   arglist <- list(...)
   lon <- .getPrm(arglist,name="^lon",default=NA_real_)
   lat <- .getPrm(arglist,name="^lat",default=NA_real_)
   stopifnot(length(lon)==length(lat))
   xy <- .project(cbind(lon,lat),ursa_proj(obj),inv=FALSE)
   if (is.null(dim(xy)))
      return(NULL)
   .getValue(obj,.getIndex(obj,xy[,1],xy[,2]))
}
'value_cr' <- function(obj,...) {
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
   if (n==1)
      ind <- arglist[[1]]
   else {
      lc <- .getPrm(arglist,name="^c",default=NA_real_)
      lr <- .getPrm(arglist,name="^r",default=NA_real_)
      ind <- (lr-1L)*n+lc
   }
   .getValue(obj=obj,ind=ind)
}
'coord_xy' <- function(obj,...)
{
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
   if (n==1)
      ind <- arglist[[1]]
   else {
      x <- .getPrm(arglist,name="^x",default=NA_real_)
      y <- .getPrm(arglist,name="^y",default=NA_real_)
      ind <- .getIndex(obj,x,y)
   }
   if (is.ursa(obj))
      n <- obj$grid$columns
   else if (is.ursa(obj,"grid"))
      n <- obj$columns
   else
      n <- session_grid()$columns
   res <- matrix(NA,nrow=2,ncol=length(ind)
                ,dimnames=list(c("c","r"),ind))
   if ((!missing(ind))&&(length(ind)>0))
   {
      res["r",] <- (ind-1L)%/%n+1L
      res["c",] <- (ind-1L)%%n+1L
   }
   res
}
'coord_cr' <- function(obj,...)
{
  # if (!is.ursa(obj))
  #    return(NULL)
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
  # nc <- obj$grid$columns
   if (is.ursa(obj))
      nc <- obj$grid$columns
   else if (is.ursa(obj,"grid"))
      nc <- obj$columns
   else
      nc <- session_grid()$columns
   if (n==1) {
      ind <- arglist[[1]]
      row <- (ind-1L)%/%nc+1L
      col <- (ind-1L)%%nc+1L
   }
   else {
      col <- .getPrm(arglist,name="^c",default=NA_integer_)
      row <- .getPrm(arglist,name="^r",default=NA_integer_)
      ind <- (row-1L)*nc+col
   }
   res <- matrix(NA,nrow=2,ncol=length(ind)
                ,dimnames=list(c("x","y"),ind))
   res["x",] <- with(ursa_grid(obj),minx+(col-0.5)*resx)
   res["y",] <- with(ursa_grid(obj),miny+(rows-row+0.5)*resy)
   res
}
'.getValue' <- function(obj,ind,col,row)
{
   if (!is.ursa(obj))
      return(NULL)
   isCT <- .is.colortable(obj)
   cl <- ursa(obj,"class")
   isCT <- isCT & length(cl)
   n <- obj$grid$columns
   if ((!missing(ind))&&(length(ind)!=-11L))
   {
      row <- (ind-1L)%/%n+1L
      col <- (ind-1L)%%n+1L
   }
   else {
      if (length(col)!=length(row))
         return(NULL)
      ind <- n*(row-1L)+col
   }
  # str(obj[row,])
   if ((FALSE)&&(length(obj$value))&&(is.na(obj$value)))
   {
      print(str(obj))
      print(c(ind=ind,c=col,r=row))
   }
   nc <- length(col)
   res <- matrix(NA,ncol=nc,nrow=nband(obj),dimnames=list(bandname(obj),ind))
   for (i in seq(nc)) {
      val <- obj[,row[i]]$value[col[i],]
      if (isCT)
         res[,i] <- cl[val+1L]
      else
         res[,i] <- val
   }
  # obj$value[ind,] ## incorrect if use "open_envi" construction
  # if (.is.colortable(obj)) {
  #    res[] <- res[][ursa(res,"value")]
  # }
   res
}
'.getIndex' <- function(obj,x,y)
{
   isRaster <- is.ursa(obj)
   isGrid <- is.ursa(obj,"grid")
   if (!isRaster & !isGrid)
      return(NULL)
   if (isRaster)
      grid <- ursa(obj,"grid")
   else
      grid <- obj
  # print(c(raster=isRaster,grid=isGrid))
   if (missing(y))
   {
      if (length(x)==1)
         return(x)
      else if (length(x)==2)
      {
         y <- x[2]
         x <- x[1]
      }
      else if (ncol(x)==2)
      {
         y <- x[,2]
         x <- x[,1]
      }
      else
         stop("specify 'y'")
   }
   if (TRUE) {
      columns <- grid$columns
      rows <- grid$rows
   }
   else if ((isRaster)&&(!is.na(obj$con$samples))&&(!is.na(obj$con$lines)))
   {
      columns <- obj$con$samples
      rows <- obj$con$lines
   }
   else
   {
      columns <- grid$columns
      rows <- grid$rows
   }
   nx <- length(x)
   ny <- length(y)
   if (nx!=-11)
   {
      x2 <- with(grid,(seq(minx,maxx,resx)-0.5*resx)[-1])
      whichx <- numeric(nx)
      for (i in seq(nx))
         whichx[i] <- which.min(abs(x2-x[i]))
   }
   else
      stop("TODO#1-X")
   if (ny!=-11)
   {
      y2 <- with(grid,(seq(miny,maxy,resy)-0.5*resy)[-1])
      whichy <- numeric(ny)
      for (i in seq(ny))
         whichy[i] <- rows-which.min(abs(y2-y[i]))+1
   }
   else
      stop("TODO#1-Y")
   ind <- as.integer((whichy-1)*columns+(whichx-1)+1)
   ind
}
