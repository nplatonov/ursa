# 'choropleth' <- function(...) .syn('zonal_stat',1,...) ## GISTools::choropleth()
'zonal_stat' <- function(x,by,FUN,table=FALSE) {
   aggregate(x=x,by=by,FUN=FUN,table=table)#,na.rm=TRUE)
}
'aggregate.ursaRaster' <- function(x,by,FUN,table=FALSE,...)
{
   if (isList <- .is.ursa_stack(by))
   {
      clname <- sapply(by,function(x) names(ursa(x,"colortable")))
      by <- ursa_brick(by)
      src <- by
      n <- length(src)
      bn <- names(src)
      if (length(bn)!=n)
         bn <- sapply(by,bandname)
      by <- ursa_new(bandname=bn)
      for (i in seq(nband(by)))
         by[i] <- src[i]
      rm(src)
      isCategory <- length(clname)
   }
   else
   {
      isCategory <- .is.category(by)
      if (isCategory)
         clname <- names(ursa(by,"colortable"))
      n <- nband(by)
      bn <- bandname(by)
   }
   by0 <- vector("list",n)
   names(by0) <- bn
   for (i in seq_along(by0))
      by0[[i]] <- by[i]$value[,1]
   x0 <- unclass(x$value)
   z <- aggregate(x=x0,by=by0,FUN,...,simplify=TRUE)
   indZ <- grep("V\\d+",colnames(z))
   colnames(z)[indZ] <- bandname(x)
   if ((isList)&&(isCategory)) {
      for (i in seq_along(clname))
         z[,i] <- rep(clname[[i]],length=nrow(z))
   }
  # a <- na.omit(names(ursa_colortable(by)))
   if (table) { ## 'table' is agrument, is not a function
      if (!nrow(z))
         return(z)
      nameBy <- names(ursa_colortable(by))
      if ((isCategory)&&(!is.null(nameBy))&&(length(na.omit(nameBy)))) {
         ind <- z[,1]+1
         ind2 <- which(is.na(match(seq_along(nameBy),ind)))
         indc <- 2:ncol(z)
         z[ind,indc] <- z[seq(nrow(z)),indc]
         z[ind2,indc] <- rep(NA,length(indc))
         z[,1] <- nameBy
      }
      return(z)
   }
   res <- ursa_new(bandname=bandname(x),ignorevalue=ignorevalue(x))
   if (n==1)
   {
      ind <- match(by0[[1]],z[,1])
      i1 <- which(!is.na(ind))
      i2 <- c(na.omit(ind))
      for (i in seq_along(indZ))
         res$value[i1,i] <- z[i2,indZ[i]]
      return(res)
   }
   else
   {
      for (i in seq(nrow(z)))
      {
         ind <- rep(0,length(by0[[1]]))
         for (j in seq_along(by0))
            ind <- ind+as.integer(by0[[j]]==z[i,j])
         ind <- which(ind==n)
         for (j in seq(nband(res)))
            res$value[ind,j] <- z[i,indZ[j]]
      }
      return(res)
   }
   NULL
}
