'.zzz.zzzregrid' <- function(obj,...) {
   print("regrid")
   if (missing(obj))
      obj <- session_grid()
   UseMethod("regrid")
}
'.zzz.zzzregrid.default' <- function(obj,...) {
   print(methods("regrid"))
   print(paste("'regrid' is not implemented for",paste(.sQuote(class(obj)),collapse=", ")))
   NULL
}
