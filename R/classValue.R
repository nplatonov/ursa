'print.ursaNumeric' <- function(x,...) {
   class(x) <- paste0(".",class(x))
   print(summary(x,...))
}
'print.ursaCategory' <- function(x,...) {
  # class(x) <- paste0(".",class(x))
   print(summary(x,...))
}
'ursa_value' <- function(obj,band) {
   if (inherits(obj,c("ursaNumeric","ursaCategory"))) {
      if (missing(band))
         return(obj)
      if (is.character(band))
         stop("unable get band index from band name")
      val <- obj[,band,drop=FALSE]
      class(val) <- class(obj)
      return(val)
   }
   if (!is.ursa(obj))
      return(NULL)
   if (missing(band))
      return(obj$value)
   if (is.character(band))
      band <- .getBand(obj,band)
   val <- obj$value[,band,drop=FALSE]
   class(val) <- class(obj$value)
   val
}
'ursa_value<-' <- function(obj,band,value) {
   if (!is.ursa(obj))
      return(NULL)
   if (missing(band)) {
      dima <- dim(obj$value[])
      if (is.null(dima)) {
         cl <- class(obj$value)
         dima <- obj$dim
         obj$value <- array(NA,dim=dima)
         class(obj$value) <- cl
      }
      if (is.ursa(value)) {
         dimb <- dim(value$value)
         ind <- rep(seq(dimb[2]),length.out=dima[2])
         obj$value <- value$value
      }
      else if (is.array(value)) {
         dimb <- dim(value)
         if (dima[1]==dimb[1]) {
            obj$value[] <- value
         }
         else if (dima[1]==prod(dimb))
            obj$value[] <- value
         else {
            if ((.is.sparse(value))&&(dima[2]==dimb[2])) {
               obj$value <- value
            }
            else
               obj$value[] <- rep(value,length=prod(dima))
         }
      }
      else if ((is.numeric(value))||((length(value)==1)&&(is.na(value)))) {
         if (.is.integer(length(value)/obj$dim[1]))
            obj$value[] <- value
         else if (length(value)==nband(obj)) {
            for (i in seq(nband(obj)))
               obj$value[,i] <- value[i]
         }
         else {
            obj$value[] <- rep(value,length=prod(dima))
         }
      }
      else {
         opW <- options(warn=0)
         warning("Optimization is required (condition 3)")
         options(opW)
         obj[] <- value
      }
   }
   else
   {
      if (is.character(band))
         band <- .getBand(obj,band)
      dima <- dim(obj$value[,band,drop=FALSE])
      if (is.array(value)) {
         if (dima[1]==dim(value)[1])
            obj$value[,band] <- value
         else if (dima[1]==prod(dim(value)))
            obj$value[,band] <- value
         else {
            opW <- options(warn=0)
            warning("Optimization is required (condition 4)")
            options(opW)
            obj[band] <- value
         }
      }
      else if (is.numeric(value)) {
         if (.is.integer(length(value)/dima[1]))
            obj$value[,band] <- value
         else if (length(value)==dima[2]) {
            for (i in seq(dima[2]))
               obj$value[,band[i]] <- value[i]
         }
         else
            obj$value[,band] <- rep(value,length=prod(dima))
      }
   }
   class(obj$value) <- if (inherits(value,"ursaCategory")) "ursaCategory" 
                       else "ursaNumeric"
   obj
}
