# ?findInterval
#'levels.ursaRaster' <- function(obj) names(ursa_colortable(obj))
'reclass' <- function(obj,dst=NULL,src=NULL,sparse=FALSE,...) {
   verbose <- .getPrm(list(...),name="verb(ose)*",default=FALSE)
  # str(as.list(match.call()))
   if (is.ursa(obj)) {
      ct <- obj$colortable
      onlyCT <- FALSE
   }
   else if (inherits(obj,"ursaColorTable")) {
      ct <- obj
      onlyCT <- TRUE
   }
   else {
      print("unable recognize object for reclass")
      return(NULL)
   }
   if ((is.null(names(ct)))&&(length(ct))) {
      names(ct) <- as.character(seq(length(ct))-1L)
   }
   if (length(dst)) {
      if (.is.colortable(dst)) {
         ct <- ursa_colortable(dst)
         val <- .deintervale(ct)
         if (is.character(val))
         {
            if (length(val)==length(ct)) {## categoral
               cname <- val
               val <- seq_along(val)-1L
              # str(list(value=val,name=cname,pal=unclass(unname(ct))))
               pal <- unclass(unname(ct))
               if (anyNA(pal)) {
                  res <- colorize(obj,value=val,name=cname)
                  ursa_colortable(res) <- ct
               }
               else
                  res <- colorize(obj,value=val,name=cname,pal=unclass(unname(ct)))
            }
            else { ## interval ## not-tested
               res <- obj
               ursa_colortable(res) <- ct
               class(res$value) <- "ursaCategory"
            }
         }
         else {
           # print(c(val=length(val),ct=length(ct)))
            if (length(val)==length(ct)) ## categoral
            {
               res <- colorize(obj,value=val,pal=unclass(unname(ct)))
            }
            else ## interval
               res <- colorize(obj,breakvalue=val,pal=unclass(unname(ct)))
         }
         return(res)
      }
      else {
         if (!length(src)) {
            if (length(ct)==length(dst)) {
               src <- seq_along(ct)-1
            }
         }
         if (length(src)==length(dst)) {
           # sparse <- !TRUE ## forced to change 'sparse' argument
            if (sparse)
               obj <- compress(obj)
            isChar <- !is.numeric(dst)
            if (isChar) {
               cname <- dst
              # print(cname)
              # dst <- seq_along(dst)-1
               dst <- factor(dst,levels=unique(dst))
               cname <- levels(dst)
               ct <- rep(NA,length(cname))
               names(ct) <- cname
               dst <- as.integer(dst)-1L
               ursa_colortable(obj) <- ct
               class(obj$value) <- "ursaCategory"
            }
            else {
               class(obj$value) <- "ursaNumeric"
            }
            if (TRUE) {
               n1 <- length(na.omit(c(obj$value)))
               isInteger <- is.integer(src)
               if ((isInteger)&&(!is.integer(obj$value)))
                  obj$value[] <- as.integer(obj$value)
               val <- dst[match(c(obj$value),src)] ## faster
               n2 <- length(na.omit(c(val)))
               if ((!isInteger)&&(n1!=n2)) { ## slower
                  str(src)
                  str(dst)
                  val <- c(obj$value)
                  pb <- ursaProgressBar(min=0,max=length(src)
                                       ,title="reclass: need optimization"
                                       ,tail=TRUE)
                  for (i in seq_along(src)) {
                     ind <- .is.eq(c(obj$value),src[i])
                     if (length(ind))
                        val[ind] <- dst[i]
                     setUrsaProgressBar(pb)
                  }
                  close(pb)
                  if ((TRUE)||(verbose)) {
                     n3 <- length(na.omit(c(val)))
                     print(c('berore'=n1,'after'=n2,'corrected'=n3))
                  }
               }
               obj$value[] <- val
               rm(val)
            }
            else {
              # print(".C optimization is needed") 
            }
            if (sparse)
               return(decompress(obj))
            else
               return(obj)
         }
      }
   }
  # if ((length(ct)>0)&&(obj$category)) {
   if (length(ct)>0) {
      if ((is.ursa(obj))&&(!.is.category(obj))) { # if ((is.ursa(obj))&&(!attr(obj$value,"category")))
         return(reclass(obj,ct,verbose=verbose)) ## recursive!!!
      }
      val <- .deintervale(ct)
      if (is.character(val)) {
         if (FALSE)
            ursa_colortable(obj) <- character(0) ## 'character(0)'? 'ct'?
         else {
            class(obj$value) <- "ursaNumeric"
         }
         return(obj)
      }
      args <- as.list(match.call())
      args$obj <- quote(obj)
      args$src <- seq_along(ct)-1
      if (length(val)==length(ct)) { ## categoral
         args$dst <- as.numeric(val)
      }
      else { ## interval
         v1 <- as.numeric(val)
         if (length(v1)<4) {
            if (length(v1)==1)
               args$dst <- rep(v1,2)
            else {
               v2 <- diff(v1)
               v3 <- max(v2)
               v4 <- v1[-1]-v2/2
               args$dst <- c(head(v4,1)-v3,v4,tail(v4,1)+v3)
               rm(v2,v3,v4)
            }
            rm(v1)
         }
         else {
            x1 <- seq(v1)
            x2 <- seq(length(v1)-1)+0.5
            x12 <- sort(c(x1,x2))
            opLoess <- options(warn=-1)
            for (s in seq(0.99,0.19,by=-0.1)) {
               m1 <- try(loess(v1~x1,span=s),silent=TRUE)
               if (inherits(m1,"try-error"))
                  next
               v12 <- predict(m1,x12)
               v3 <- c(head(v12,1)-head(diff(v12),1)
                      ,predict(m1,x2)
                      ,tail(v12,1)+tail(diff(v12),1))
               if (length(unique(sign(diff(v3))))==1)
                  break
            }
            options(opLoess)
            args$dst <- v3
            rm(v1,x1,x12,m1,v12,v3)
         }
      }
      if (identical(args$src,args$dst)) {
         if (TRUE) { ## ++ 20180706
            if (inherits(obj$value,"ursaCategory"))
               class(obj$value) <- "ursaNumeric"
            else if (inherits(obj$value,"ursaNumeric"))
               class(obj$value) <- "ursaCategory"
         }
         return(obj)
      }
      if (onlyCT)
         return(args$dst)
      if (any(is.na(args$dst))) {
         if (FALSE) {
            obj$colortable <- character(0)
            class(obj$colortable) <- "ursaColorTable"
         }
         return(obj)
      }
      res <- do.call(as.character(args[1]),args[-1]) ## recursive!!!
      res$con$nodata <- .optimal.nodata(args$dst)
      return(res)
   }
   if (FALSE)
      res <- colorize(obj,...)
   else {
      arglist <- list(...)
      if (!.lgrep("tail",names(arglist)))
         arglist$tail <- 0
      res <- do.call("colorize",c(list(obj),arglist),quote=TRUE)
   }
   res$colortable[] <- rep(NA_character_,length(res$colortable))
   if ((is.character(dst))&&(length(dst)==length(res$colortable))) {
      names(res$colortable) <- dst
     # str(res$colortable)
   }
   return(res)
  # NULL
}
'.extract' <- function(obj) {
   if (!.is.category(obj)) ##(!.is.colortable(obj))
      return(obj)
   if (is.character(.deintervale(obj$colortable))) {
      class(obj$value) <- "ursaNumeric"
      return(obj)
   }
   obj <- reclass(obj)
   if (FALSE)
      ursa_colortable(obj) <- character(0)
   obj
}
