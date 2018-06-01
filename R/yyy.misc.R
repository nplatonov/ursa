'.getBand' <- function(x,i,regexp=FALSE,new=FALSE)
{
   if (!is.ursa(x))
      return(NULL)
   if (is.numeric(i))
      return(i)
   if (!is.character(i))
      return(NULL)
   if (!is.na(x$con$posZ[1]))
      myname <- x$name[x$con$posZ]
   else
      myname <- x$name
   m <- NULL
   if (!regexp)
      m <- na.omit(match(i,myname))
   if ((!length(m))||(regexp))
   {
      m <- NULL
      for (k in i)
         m <- c(m,.grep(k,myname))
   }
   if (!length(m))
   {
      if (new)
         return(length(x$name)+seq(i))
      return(NULL)
   }
   return(m)
}
'.read.hdr' <- function(fname,resetGrid=TRUE)
{
   opW <- options(warn=2)
   x <- open_envi(fname=fname,resetGrid=resetGrid,headerOnly=TRUE)
   if (!is.na(x$con$handle))
      close(x$con$handle)
   options(opW)
   x
}
