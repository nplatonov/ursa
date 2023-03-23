'.areaIncrement' <- function(x,dist=NA,mul=1,verbose=FALSE)
{
   if (!is.ursa(x))
      return(NULL)
   sparse <- attr(x$value,"sparse")
   if ((!is.null(sparse))&&(any(na.omit(sparse)!=0)))
      stop("TODO: expand compression")
   if (!is.na(x$con$posZ))
   {
      nb <- length(x$con$posZ)
      bn <- x$name[x$con$posZ]
   }
   else
   {
      nb <- x$dim[2]
      bn <- x$name
   }
   if (any(is.na(dist)))
      dist <- with(x$grid,c(resx,resy))
   else if (length(dist)==1)
      dist <- rep(dist,2)
   else if (length(dist)!=2)
      stop("unrecognized argument 'dist'")
   dimx <- with(x$grid,c(columns,rows,nb))
   x$value <- (.Cursa(C_areaIncrement,x=as.numeric(x$value),dim=as.integer(dimx)
                 ,res=as.numeric(dist),out=numeric(prod(dimx))
                 ,NAOK=TRUE)$out-1)*mul
   dim(x$value) <- with(x$grid,c(columns*rows,nb))
   x
}
