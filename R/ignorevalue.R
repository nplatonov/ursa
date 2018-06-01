'ursa_nodata' <- function(obj) .syn('ignorevalue',0,obj)
'ursa_nodata<-' <- function(obj,value) .syn('ignorevalue<-',0,obj,value)
'ignorevalue' <- function(obj)
{
   if (!is.ursa(obj))
      return(NULL)
   if (!.is.con(obj$con))
      return(NA)
   obj$con$nodata
}
'ignorevalue<-' <- function(obj,value)
{
   if (!is.ursa(obj))
      return(NULL)
   obj$con$nodata <- value
   if (TRUE)
   {
      if (!is.null(dim(obj$value)))
         obj$value[obj$value==value] <- NA
   }
   obj
}
'.make.nodata' <- function(datatype)
{
   d <- as.integer(datatype)
   res <- if (FALSE) 0L
   else if (d==1L) 255L
   else if (d==2L) -32768L
   else if (d==3L) -999999L 
   else if (d==4L) -9999.9
   else if (d==12L) 32767L 
   else if (d==11L) -128L
   else -999999.9
   res
}
'.optimal.nodata' <- function(x) {
   if (is.data.frame(x))
      x <- as.matrix(x)
   if (is.ursa(x))
      x <- x$value
   x <- as.numeric(na.omit(c(x)))
   if (!length(x))
      return(NA_integer_)
   a <- range(x)
   b <- max(abs(a))
   res <- -b+1
   s <- ifelse(all(a>=0),1L,-1)
   if (b<90)
      res <- 99L*s
   else if (b<900)
      res <- 999L*s
   else if (b<9000)
      res <- 9999L*s
   else if (b<90000)
      res <- 99999L*s
   storage.mode(res) <- storage.mode(x)
   if ((FALSE)||(.is.integer(x))) { ## forcing to integer
      if (all(a>=0 & a<=100))
         res <- 121L
      else if (all(a>=0 & a<=250))
         res <- 255L
      else if (all(a>=0 & a<=30000))
         res <- 32767L
      else if (all(abs(a<=30000)))
         res <- -32768L
      else if (all(abs(a<=125)))
         res <- -128L
   }
   res
}
