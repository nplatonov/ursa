'summary.ursaRaster' <- function(object,...) {
   isCategory <- .is.category(object)
   if (isCategory) {
      class(object$value) <- paste0(".",class(object$value))
     # return(as.table(x))
      res <- NULL
      for (i in seq(object))
         res <- rbind(res,as.table(object[i]))
      z <- object$con$posZ
      rownames(res) <- if (is.na(z[1])) object$name else object$name[z]
      return(res)
   }
   class(object$value) <- paste0(".",class(object$value))
   z <- object$con$posZ
   colnames(object$value) <- if (is.na(z[1])) object$name else object$name[z]
  # print(colnames(object$value))
   summary(object$value)
}
'summary.ursaNumeric' <- function(object,...) {
   dim(object) <- NULL
   class(object) <- paste0(".",class(object))
   summary(object)
}
'summary.ursaCategory' <- function(object,...) {
  # dim(object) <- NULL
   class(object) <- NULL # paste0(".",class(object))
  # str(object)
   as.table(object)
}
