'head.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   if (FALSE) {
      m <- if (n>k) k else n
      if (m==0)
         m <- 1
      x[1:m]
   }
   x[head(seq_len(k),n)]
}
'tail.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   if (FALSE) {
      m <- if (n>k) k else n
      if (m==0)
         m <- 1
     # str(x)
     # print(k-(m:1)+1)
      x[k-(m:1)+1]
   }
   x[tail(seq_len(k),n)]
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
         if ((FALSE)&&(is.null(objAttr))) {
            dim(x) <- c(length(x),1)
            rownames(x) <- seq(nrow(x))
            colnames(x) <- "<vector>"
            ind <- seq(dim(x)[1])
            ind <- sort(unique(c(head(ind,n),tail(ind,n))))
            return(x[ind,,drop=FALSE])
         }
         ret <- c(head(x,n),tail(x,n))
         if (length(ret)>length(x))
            return(x)
         if (length(grep("names",names(objAttr)))) {
            objAttr$names <-  c(head(objAttr$names,n),tail(objAttr$names,n))
         }
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
   m <- sort(unique(c(1L:m,k-(m:1L)+1L)))
   x[m]
}
