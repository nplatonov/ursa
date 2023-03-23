'temporal_mean' <- function(obj,win=7,cover=0,verbose=FALSE)
{
   fun <- "temporal_mean" # as.character(match.call())[1]
   win <- floor(win/2)*2+1
   if (win<1)
      win <- 1
   if (cover>1)
      cover <- cover/win
   if (cover>1)
      cover <- 1
   if (cover<0)
      cover <- 0
   isArray <- FALSE
   if (!is.ursa(obj))
   {
      if (!is.array(obj))
         return (NULL)
      isArray <- TRUE
   }
   if (isArray)
   {
      dimx <- dim(obj)
      a <- .Cursa(C_timefilt4
             ,x=as.numeric(obj),dim=as.integer(dimx),win=as.integer(win)
             ,cover=as.numeric(cover)
             ,res=numeric(prod(dimx)),NAOK=TRUE)$res
      dim(a) <- dimx
      return(a)
   }
  # obj <- ursa_new(len=x$dim[2])
   dimx <- dim(obj$value)
   if (verbose)
      .elapsedTime(paste(fun,"start",sep=":"))
   obj$value[] <- .Cursa(C_timefilt4
                    ,x=as.numeric(obj$value),dim=as.integer(dimx)
                    ,win=as.integer(win),cover=as.numeric(cover)
                    ,res=numeric(prod(dimx)),NAOK=TRUE)$res
   if (verbose)
      .elapsedTime(paste(fun,"stop",sep=":"))
   obj
}
