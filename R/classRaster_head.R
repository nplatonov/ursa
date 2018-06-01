'head.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
   if (m==0)
      m <- 1
   x[1:m]
}
'tail.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
   if (m==0)
      m <- 1
  # str(x)
  # print(k-(m:1)+1)
   x[k-(m:1)+1]
}
'series' <- function(x,n=3L,s=170,...)
{
   if (!is.ursa(x)) {
      if (is.null(dim(x))) {
         if (is.list(x)) {
            ind <- seq_along(x)
            ind <- unique(c(head(ind,n),tail(ind,n)))
            return(x[ind])
           # return(lapply(x[ind],series,n)) ## RECURSIVE
         }
         objAttr <- attributes(x)
         ret <- c(head(x,n),tail(x,n))
         attributes(ret) <- objAttr
         return(ret)
      }
      ind <- seq(dim(x)[1])
      ind <- sort(unique(c(head(ind,n),tail(ind,n))))
      return(x[ind,,drop=FALSE])
   }
   s2 <- with(ursa_grid(x),columns*rows*4)
   m2 <- floor(s*1024*1024/2/s2)
   if (m2<n)
      n <- m2
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
   if (m==0)
      m <- 1
   m <- sort(unique(c(1:m,k-(m:1)+1)))
   x[m]
}
