'ursa_colortable' <- function(x)
{
   if ((TRUE)&&(inherits(x,"ursaColorTable"))) ## was FALSE 20160127
      ct <- x
   else if (is.character(x)) {
      valid <- all(sapply(unname(x),function(col) {
         tryCatch(is.matrix(col2rgb(col)),error=function(e) FALSE)
      }))
      if (!valid)
         return(NULL)
      class(x) <- "ursaColorTable"
      return(x)
   }
   else if (inherits(x,"ursaLegend")) {
      if (length(x$name)==length(x$pt.bg)) {
         print("POINT")
         ret <- x$pt.bg
         names(ret) <- x$name
         class(ret) <- "ursaColorTable"
         return(ret)
      }
      if ((all("transparent" %in% x$fill))&&(length(x$name)==length(unique(x$border)))) {
         print("LINESTRING")
         ind <- match(x$name,names(x$border))
         if (inherits(x$border,"ursaColorTable"))
            return(x$border[ind])
         ret <- x$border[ind]
         names(ret) <- x$name
         class(ret) <- "ursaColorTable"
         return(ret)
      }
      if (length(x$name)==length(x$fill)) {
         print("POLYGON")
         ret <- x$fill
         names(ret) <- x$name
         class(ret) <- "ursaColorTable"
         return(ret)
      }
      print("unreachable")
      return(NULL)
   }
   else if (any(sapply(x,inherits,"ursaLegend"))) {
      return(lapply(x,ursa_colortable)) ## RECURSIVE!!!
   }
   else if (!is.ursa(x)) {
      ct <- x$colortable
      if (!inherits(ct,"ursaColorTable")) {
         if ((is.list(x))&&("name" %in% names(x))&&("fill" %in% names(x))) {
            ct <- x$fill
            names(ct) <- x$name
            class(ct) <- "ursaColorTable"
            return(ct)
         }
         else
            return(NULL)
      }
   }
   else 
      ct <- x$colortable
   if (!length(ct))
      return(ct)
   if (is.null(names(ct)))
      names(ct) <- as.character(seq(length(ct))-1L)
   ct
}
'[.ursaColorTable' <- function(x,i) {
   cl <- class(x)
   res <- unclass(x)[i]
   class(res) <- cl
   res
}
'ursa_colortable<-' <- function(x,value)
{
   if (!is.ursa(x))
      return(NULL)
  ## Implement? 'if (is.ursa(value,"colortable)) {x <- colorize(x,value);return(x)'
   if (is.null(value))
      value <- character(0)
   myname <- names(x$colortable)
   if ((length(myname)==length(value))&&
       (is.null(names(value)))&&
       (!inherits(value,"ursaColorTable")))
      names(value) <- myname
   class(value) <- "ursaColorTable"
   x$colortable <- value
   if ((inherits(x$con$handle,"connection"))&&(is.null(dim(x$value))))
      .write.hdr(x,clear=FALSE)
   x
}
'print.ursaColorTable' <- function(x,...)
{
   print(unclass(x))
   cn <- names(x)
   cnd <- .deintervale(cn)
  # if ((!identical(cn,as.character(cnd)))&&(length(cn)))
   if (length(cn)!=length(cnd))
      print(.deintervale(cn),quote=FALSE)
}
'.is.colortable' <- function(obj) {
   if (is.ursa(obj))
      obj <- obj$colortable
   else if (is.list(obj)) {
      if (!is.null(obj$colortable))
         obj <- obj$colortable
      else if (length(obj)==1)
         obj <- obj[[1]]
   }
   ((length(obj)>0)&&(inherits(obj,"ursaColorTable")))
}
'.is.nominal' <- function(obj) {
   if (!.is.colortable(obj))
      return (FALSE)
   ct <- ursa_colortable(obj)
   val <- .deintervale(ct)
   length(val)==length(ct)
}
'.is.interval' <- function(obj) {
   if (!.is.colortable(obj))
      return (FALSE)
   ct <- ursa_colortable(obj)
   val <- .deintervale(ct)
   length(val)!=length(ct)
}
'.is.category' <- function(obj) {
   if (is.ursa(obj))
      return(inherits(obj$value,"ursaCategory"))
   inherits(obj,"ursaCategory")
}
'.be.category' <- function(obj) {
   (.is.colortable(obj))&&(!.is.category(obj))
}
'names.ursaColorTable' <- function(x) NextMethod("names",x)
'names<-.ursaColorTable' <- function(x,value) {
   if (!is.null(value)) {
      if (length(x)==length(value)+1) {
         n0 <- value
         if (length(n0)>1)
            value <- paste0("(",n0[-length(n0)],";",n0[-1],"]")
         value <- c(paste0("<= ",n0[1]),value,paste0("> ",n0[length(n0)]))
      }
   }
   if (!FALSE)
      return(NextMethod("names<-",x))
   cl <- class(x)
   x <- unclass(x)
   names(x) <- value
   class(x) <- cl
   x
}
'ursa_colorindex' <- function(ct) {
   if ((is.list(ct))&&(!is.null(ct$index))&&(inherits(ct$colortable,"ursaColorTable")))
      return(ct$index)
   NULL
}
