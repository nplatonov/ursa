'compress' <- function(obj)
{
   if (!is.ursa(obj))
      return(NULL)
   dimx <- dim(obj$value)
   if (is.null(dimx))
      return(obj)
   dimx2 <- obj$dim
   if (!is.na(obj$con$posZ[1]))
      dimx2[2] <- length(obj$con$posZ)
   if (!identical(dimx2,dimx))
      return(obj)
   if (TRUE) {
      ind <- .Cursa("makeField",as.numeric(obj$value),dim=dimx
                            ,res=integer(dimx[1]),NAOK=TRUE)$res
   }
   else
   {
      ind <- apply(obj$value,2,function(x) !all(is.na(x)))
      ind <- which(ind)
   }
   ind <- ind[ind!=0L]
   if (!length(ind))
   {
      attr(obj$value,"sparse") <- NULL ## neccessary?
      return(obj)
   }
   cl <- class(obj$value)
   obj$value <- obj$value[ind,,drop=FALSE]
   class(obj$value) <- cl
   attr(obj$value,"sparse") <- ind #if ((1)&&(n*2>obj$dim[1])) -which(ind2==0L) else ind
   obj$dim <- dim(obj$value)
   .gc()
   obj
}
'decompress' <- function(obj)
{
   if (!is.ursa(obj))
      return(NULL)
   sparse <- attr(obj$value,"sparse")
   if (is.null(sparse)) ## no compression
      return(obj)
   columns <- obj$grid$columns
   rows <- obj$grid$rows
   if (all(na.omit(sparse)<0))
      sparse <- seq(columns*rows)[sparse]
   val <- array(NA,dim=c(columns*rows,obj$dim[2]))
   cl <- class(obj$value)
   val[sparse,] <- obj$value[]
   obj$value <- val
   class(obj$value) <- cl
   attr(obj$value,"sparse") <- NULL ## neccessary?
   obj$dim <- dim(val)
   .gc()
   obj
}
