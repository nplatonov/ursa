'as.list.ursaRaster' <- function(x,...) ursa_stack(x,...)
'unlist.ursaStack' <- function(x,recursive,use.names) ursa_brick(x)
'ursa_apply' <- function(obj,FUN,...) {
   res <- lapply(X=obj,FUN=FUN,...)
   if (.is.ursa_stack(res))
      class(res) <- "ursaStack"
   res
}
'ursa_stack' <- function(...) { ## 'ursa_hetero' (make syn?)
   obj <- list(...)
   if ((length(obj)==1)&&(is.ursa(obj[[1]]))) {
      obj <- obj[[1]]
      res <- vector("list",nband(obj))
     # names(res) <- bandname(obj)
      for (i in seq_along(res))
         res[[i]] <- obj[i]
      names(res) <- names(obj)
      class(res) <- "ursaStack"
      return(res)
   }
   class(obj) <- "ursaStack"
   obj
}
'ursa_brick' <- function(obj) { ## 'ursa_homo' (make syn?)
   if (is.ursa(obj))
      return(obj)
   isList <-  .is.ursa_stack(obj)
   if (!isList)
      return(NULL)
   n <- sapply(obj,nband)
   nodata <- unique(sapply(obj,ignorevalue))
   rname <- unname(unlist(lapply(obj,bandname)))
   res <- ursa_new(nband=sum(n))#,bandname=rname)
   oname <- names(obj)
   k <- 0L
   for (i in seq_along(obj)) {
      if (!n[i])
         next
      img <- .extract(obj[[i]])
      ##~ if (.is.colortable(img)) {
         ##~ print(img)
         ##~ print(ursa_colortable(img))
         ##~ img <- reclass(img)
         ##~ print(img)
      ##~ }
      nl <- nband(img)
      k2 <- k+seq(nl)
      res[k2] <- img
      if ((!is.null(oname))&&(nl==1)) {
        # bandname(res)[k2] <- oname[i]
         rname[k2] <- oname[i]
      }
      k <- k+nl
   }
   if (all(tail(duplicated(lapply(obj,ursa_colortable)),-1)))
   if (length(nodata)==1)
      ignorevalue(res) <- nodata
   bandname(res) <- rname
   if (all(tail(duplicated(lapply(obj,ursa_colortable)),-1))) {
      ct <- ursa_colortable(obj[[1]])
      if (length(ct)) {
         ursa_colortable(res) <- ct
         class(ursa_value(res)) <- "ursaCategory"
      }
   }
  # class(res) <- c(class(res),"ursaBrick") ## not necessary
   res
}
'.is.ursa_stack' <- function(obj) {
   if (is.ursa(obj))
      return(FALSE)
   if (!is.list(obj))
      return(FALSE)
   all(sapply(obj,function(x) is.ursa(x) | is.null(x)))
}
'.is.ursa_brick' <- function(obj) is.ursa(obj)
