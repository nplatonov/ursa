'as.table.ursaRaster' <- 'ursa_table' <- function(x,...) {
  # print("as.table.ursaRaster")
   ct <- ursa_colortable(x)
   isCT <- .is.colortable(x)
   res <- table(x$value,...)
   if (!.is.colortable(ct)) {
     # names(dimnames(res)) <- as.character(match.call())[2]
      names(dimnames(res)) <- NULL
      return(res)
   }
   ind <- match(as.numeric(names(res)),seq(length(ct))-1)
   if (any(is.na(ind))) {
      return(res)
   }
   freq <- rep(0L,length(ct))
   freq[ind] <- res
   res <- as.table(freq)
   dimnames(res) <- list(names(ct))
   res
}
#'table' <- function(...) UseMethod("table")
#'table.NULL' <- function(...) NULL
#'table.default' <- function(x,...) base::table(x,...)
#'table.ursaRaster' <- function(x,...) as.table.ursaRaster(x,...)
