'as.data.frame.ursaRaster' <- function(x,...) # (x,row.names,optional,...)
{
   arglist <- list(...)
   band <- .getPrm(arglist,name="band",default=FALSE)
   id <- .getPrm(arglist,name="id",default=FALSE)
   na.rm <- .getPrm(arglist,name="na\\.rm",default=TRUE)
   all.na <- .getPrm(arglist,name="all\\.na",default=FALSE)
   col.names <- .getPrm(arglist,name="col(\\.)*name(s)*"
                       ,class="character",default=NULL)
   .as.data.frame(obj=x,band=band,id=id,na.rm=na.rm
                            ,all.na=all.na,col.names=col.names)

}
'.as.data.frame' <- function(obj,band=FALSE,id=FALSE
                                 ,na.rm=TRUE,all.na=FALSE,col.names=NULL) {
  ## TODO: categories should be intepreted as 'factor' or 'character'
   g1 <- ursa_grid(obj)
   if (!length(g1$seqx))
      x <- with(g1,seq(minx,maxx,by=resx)[-1]-resx/2)
   else
      x <- g1$seqx
   if (!length(g1$seqy))
      y <- rev(with(g1,seq(miny,maxy,by=resy)[-1]-resy/2))
   else
      y <- rev(g1$seqy)
  # xy <- expand.grid(x=x,y=y,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
   xy <- data.frame(x=rep(x,times=length(y)),y=rep(y,each=length(x)))
   isList <- .is.ursa_stack(obj)
   isCT <- .is.colortable(obj)
   bname <- bandname(obj)
   if (!is.na(obj$con$posZ[1]))
      indZ <- obj$con$posZ
   else
      indZ <- seq_along(bname)
   if (na.rm)
   {
      if (length(indZ)>1)
      {
         if (all.na)
            indR <- which(apply(obj$value,1,function(x) all(!is.na(x))))
         else
            indR <- which(apply(obj$value,1,function(x) any(!is.na(x))))
      }
      else {
         indR <- which(!is.na(obj$value[,1]))
      }
   }
   else
      indR <- seq(obj$dim[1])#*length(indZ))
   xy <- xy[indR,]
   if (!band)
   {
      if (TRUE) {
        # res <- with(xy,data.frame(x=x,y=y))
         res <- xy
        # isCategory <- .is.category(obj)
         for (i in seq_along(indZ))
         {
            res$z <- c(obj$value[indR,i]) ## 20170202 'i' or 'indZ[i]'?
            if (isCT) {
               ct <- ursa_colortable(obj)
               aname <- names(ct)
              # acol <- unname(ct) ## TODO new column $amount_col for $amount
              # res$z <- factor(names(ct)[res$z+1L]) ## -- 20180317
               res$z <- ordered(factor(names(ct)[res$z+1L]),levels=names(ct)) ## ++ 20180317
            }
            if (length(indZ)>0) ## 20160812 changed '>1' -> '>0'
               colnames(res)[i+2] <- bname[i]
         }
      }
      else if (FALSE){
        # res <- cbind(xy,as.data.frame(obj$value[indR,indZ])) ## not quick
      }
      else {
         res <- vector("list",length(indZ))
         if (length(indZ))
            names(res) <- bname
         for (i in seq_along(indZ))
         {
            res[[i]] <- c(obj$value[indR,i])
         }
         res <- cbind(xy,res) ## not quick
      }
   }
   else
   {
      n <- length(indZ)
      nr <- nrow(xy)
      res <- data.frame(x=rep(xy$x,n),y=rep(xy$y,n)
                       ,z=NA
                       ,band=rep(bname,each=nrow(xy))
                       ,id=rep(seq(nr),n)
                       ,stringsAsFactors=TRUE)
      for (i in seq_along(indZ))
      {
         ind <- (nr*(i-1)+1):(nr*i)
         res$z[ind] <- c(obj$value[indR,i])
      }
      if (!id)
         res$id <- NULL
   }
   if (is.character(col.names))
      colnames(res) <- rep(col.names,length=ncol(res))
   attr(res,"crs") <- g1$crs[which.max(nchar(g1$crs))]
  # attr(res,"colortable") <- ursa_colortable(obj)
   res
}
# 'as_data_frame' <- function(obj) UseMethod("as_data_frame",obj)
##~ '.as_data_frame.ursaRaster' <- function(obj,band=FALSE,id=FALSE
                                                    ##~ ,na.rm=TRUE,all.na=FALSE)
##~ {
   ##~ res <- as.data.frame(obj,band=band,id=id,na.rm=na.rm,all.na=all.na)
   ##~ if (!requireNamespace("dplyr",quietly=.isPackageInUse()))
      ##~ return(res)
   ##~ dplyr::as_data_frame(res)
##~ }
##~ '.as.data.table.ursaRaster' <- function(obj,band=FALSE,id=FALSE
                                                    ##~ ,na.rm=TRUE,all.na=FALSE) {
   ##~ print("HERE data.table")
   ##~ NULL
##~ }
