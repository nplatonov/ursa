'mean.ursaRaster' <- function(x,...) .syn('local_mean',0,x,...)
'median.ursaRaster' <-  function(x,...) .syn('local_median',0,x,...)
# 'sd.ursaRaster' <-  function(x,...) .syn('local_sd',0,x,...) ## (list) object cannot be coerced to type 'double'
'local_mean' <- function(x,cover=0.5-1e-3,weight=NULL,verbose=FALSE,bandname="mean")
{
   a <- .average(x=x,cover=cover,weight=weight,verbose=verbose,sum=FALSE)
   if (nchar(bandname))
      bandname(a) <- bandname
   a
}
'local_sum' <- function(x,cover=0.5-1e-3,weight=NULL,verbose=FALSE,bandname="sum")
{
   a <- .average(x=x,cover=cover,weight=weight,verbose=verbose,sum=TRUE)
   if (nchar(bandname))
      bandname(a) <- bandname
   a
}
'local_median' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="median",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_min' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="min",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_max' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="max",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_sd' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="sd",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_var' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="var",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_length' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="length",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_any' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="any",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'local_all' <- function(x,cover=0.5-1e-3,verbose=FALSE) {
   res <- .groupSummary(x,generic="all",cover=cover,weight=NULL
                       ,verbose=verbose)
   res
}
'.average' <- function(x,cover=0.5-1e-3,weight=NULL,sum=FALSE,verbose=FALSE)
{
   fun <- "mapAlgebraLocal" #as.character(match.call())[1]
   isImage <- is.ursa(x)
   isArray <- is.array(x)
   if (cover>1)
      cover <- cover/length(x)
   if (cover==0)
      cover <- 1e-11
   if (isImage)
   {
      g0 <- session_grid()
      session_grid(x)
      if (.is.con(x$con))
         nodata <- x$con$nodata
      else
         nodata <- NA
      if (is.na(nodata)) {
        # nodata <- max(x$value,na.rm=TRUE)+1
         nodata <- -1e38 #.optimal.nodata(x$value) ## result and nodata matching
      }
      x$value[is.na(x$value)] <- nodata
      obj <- ursa_new(bandname=fun,nodata=nodata)
     # print(c(nodata=nodata))
      dimx <- dim(x$value)
      if (is.null(weight))
         weight <- rep(1,dimx[2])
      if (!sum)
         weight <- weight/sum(weight)
      if (verbose)
         .elapsedTime(paste(fun,"start",sep=":"))
      a <- .Cursa(C_makemap4
             ,x=as.numeric(x$value),bg=as.numeric(nodata)
             ,dim=as.integer(dimx),cover=as.numeric(cover)
             ,weight=weight,sum=as.integer(sum)
             ,res=numeric(dimx[1]*1L),NAOK=FALSE)
      if (verbose)
         .elapsedTime(paste(fun,"stop",sep=":"))
      if (is.na(x$con$posR[1])) {
         obj$value[] <- a$res
         class(obj$value) <- "ursaNumeric"
      }
      else
      {
         obj$con$posR <- x$con$posR
         obj$value <- a$res
         dim(obj$value) <- c(dimx[1],1)
      }
      if (abs(nodata)<1)
         obj$value[abs(obj$value-nodata)<1e-27] <- NA
      else
         obj$value[abs(obj$value/nodata-1)<1e-6] <- NA
      session_grid(g0)
      return(obj)
   }
   if (isArray)
   {
      dimx <- dim0 <- dim(x)
     # if (length(dimx)==2)
     #    return(x)
      if (length(dimx)==3)
         dimx <- c(dimx[1]*dimx[2],dimx[3])
      if (length(dimx)!=2)
         return(x)
      if (is.null(weight))
         weight <- rep(1,dimx[2])
      weight <- weight/sum(weight)
      a <- .Cursa(C_makemap4
             ,x=as.numeric(x),bg=-1e34
             ,dim=as.integer(dimx),cover=as.numeric(cover)
             ,weight=weight,sum=as.integer(sum)
             ,res=numeric(dimx[1]*1L),NAOK=TRUE)$res
     # dim(a) <- dim(x)[1:2]
      dim(a) <- head(dim0,-1)
      return(a)
   }
   stop(paste0(fun,": Non-supported class of data"))
}
'quantile.ursaRaster' <- function(x,...) .syn('local_quantile',0,x,...)
'local_quantile' <- function(x,probs=seq(0,1,0.25),type=7,cover=0.5-1e-3,verbose=FALSE) {
   if (!is.ursa(x))
      return(NULL)
   g0 <- session_grid()
   nx <- length(x)
   skip <- rep(NA,length(probs))
   if (cover>1)
      cover <- cover/length(x)
   if (cover==0)
      cover <- 1e-11
   session_grid(x)
  # res <- ursa(bandname=as.character(probs))
   qv <- apply(ursa_value(x),1,function(v) {
      if (length(na.omit(v))/nx<cover)
         return(skip)
      quantile(v,probs=probs,type=type,name=FALSE,na.rm=TRUE)
   })
   res <- as.ursa(t(qv))
  # ursa_value(res) <- t(qv)
  # display(res)
   session_grid(g0)
   res
}
