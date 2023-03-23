'local_stat' <- function(obj,time=NULL,cover=1e-6,smooth=FALSE,verbose=FALSE)
{
   fun <- 'local_stat' # as.character(match.call())[1]
   if (!is.ursa(obj))
      return(NULL)
   if (.is.con(obj$con))
      nodata <- obj$con$nodata
   else
      nodata <- NA
   res <- ursa_new(bandname=c("mean","sd","sum","min","max","n"
                             ,"slope","slopeS","RSS","ESS")
                   ,nodata=-9999) ## nodata=nodata
   myname <- bandname(obj)
   dimy <- dim(obj$value)
   np <- dimy[1]
   if (is.null(time))
      time <- seq(myname)
   if (cover>1)
      cover <- cover/length(myname)-1e-11
   if (verbose)
      .elapsedTime(paste(fun,"start",sep=":"))
   ret <- .Cursa(C_variability4
            ,y=as.numeric(obj$value),x=as.numeric(time),dim=as.integer(dimy)
            ,cover=as.numeric(cover)
            ,res=numeric(np*10L),NAOK=TRUE)$res
   if (verbose)
      .elapsedTime(paste(fun,"stop",sep=":"))
   dim(ret) <- c(np,10)
   if (is.na(obj$con$lines))
      obj$con$lines <- obj$grid$rows
   if (is.na(obj$con$samples))
      obj$con$samples <- obj$grid$columns
   if (FALSE) ## 20180220 --
      r <- if (is.na(obj$con$posR[1])) seq(obj$con$lines) else obj$con$posR
   else {
      r <- if (is.na(obj$con$indexR[1])) seq(obj$con$lines) else seq_along(obj$con$indexR)
      if (!is.na(obj$con$posR[1]))
         r <- obj$con$posR
   }
   if ((is.na(obj$con$posR[1]))&&(length(r)!=res$grid$rows))
   {
      if (verbose)
         message(paste(fun,"warning *** probably incorrect resampling",sep=":"))
      r <- seq(res$grid$rows)
   }
  ##~ # print(str(ret))
   b <- 7L
   for (i in seq(b))
      res[i,r] <- ret[,i,drop=FALSE] ##a$ret[(i-1)*np+1:np]
   res[b+2,r] <- ret[,b+2,drop=FALSE] ## SSR (introduced 20150123)
   res[b+3,r] <- ret[,b+3,drop=FALSE] ## SSE (introduced 20150123)
   if (smooth)
      res["slope"] <- focal_median(res["slope"],verbose=verbose)
   F <- ret[,b+1]
   N <- ret[,b-1]
   ind <- which(!is.na(F) & !is.na(N) & N>2)
   F[ind] <- pf(abs(F[ind]),1,N[ind]-2)*sign(F[ind])
   F[-ind] <- NA
   r2 <- (c(min(r)-1,max(r)))*obj$con$samples
   r2[1] <- r2[1]+1
  # res$value[r2[1]:r2[2],6] <- F
   res[b+1,r] <- F
   res[,r]
}
