'as.array.ursaRaster' <- function(x,...) {
   arglist <- list(...)
   drop <- .getPrm(arglist,name="drop",default=FALSE)
   flip <- .getPrm(arglist,name="flip",default=FALSE)
   permute <- .getPrm(arglist,name="perm",default=FALSE)
   dim <- .getPrm(arglist,name="dim",default=FALSE)
   .as.array(x,drop=drop,flip=flip,permute=permute,dim=dim)
}
'.as.array' <- function(x,drop=FALSE,flip=FALSE,permute=FALSE,dim=FALSE)
{
   if (!.is.con(x$con))
   {
      nc <- x$grid$columns
      nr <- x$grid$rows
      nb <- x$dim[2]
   }
   else
   {
      con <- x$con
      if ((length(con$indexC)==1)&&(is.na(con$indexC[1]))) ## 20170525 added len==1
      {
         if (is.na(con$samples))
            nc <- x$grid$columns
         else
            nc <- con$samples
      }
      else
         nc <- length(con$indexC)
      if ((length(con$indexR)==1)&&(is.na(con$indexR[1]))) ## 20170525 added len==1
      {
         if (is.na(con$lines))
            nr <- x$grid$rows
         else
            nr <- con$lines
      }
      else if (all(is.na(con$indexR)))
         nr <- x$grid$rows
      else
         nr <- length(con$indexR)
      if (is.na(con$indexZ[1]))
      {
         if (!is.na(con$bands))
            nb <- con$bands
         else
            nb <- x$dim[2]
      }
      else
         nb <- length(con$indexZ)
      if (!is.na(con$posC[1]))
         nc <- length(con$posC)
      if (!is.na(con$posR[1]))
         nr <- length(con$posR)
      if (!is.na(con$posZ[1]))
         nb <- length(con$posZ)
      if (is.na(nb))
         nb <- 1L
   }
   if (dim) {
      if (!permute)
         return(c(nc,nr,nb))
      else
         return(c(nr,nc,nb))
   }
   if (is.null(dim(x$value)))
      return(NULL)
   sparse <- attr(x$value,"sparse")
   if (is.null(sparse))
      val <- x$value
   else if (sparse[1]<0)
   {
      ind <- 1L:x$dim[1]
      ind[-sparse] <- 0L
      val <- array(NA,dim=c(nc*nr,nb))
      val[sparse,] <- x$value
   }
   else if (sparse[1]>0) {
      val <- array(NA,dim=c(nc*nr,nb))
      val[sparse,] <- x$value
   }
   else
      val <- x$value
   dim(val) <- c(nc,nr,nb)
   if (!flip) ## FALSE 20160201, TRUE 20160330, added '!flip' for 'write.idrisi'
      val[] <- val[,rev(seq(nr)),]
   if (permute)
      val <- aperm(val,c(2,1,3))
   if ((drop)&&(nb==1))
      val <- val[,,1]
   class(val) <- NULL
   val
}
