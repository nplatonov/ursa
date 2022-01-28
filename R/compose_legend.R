'compose_legend' <- function(...)
{
   if (.skipPlot(FALSE))
      return(NULL)
   nlegend <- getOption("ursaPngLayout")$legend
   arglist <- list(...)
   colorbar <- .getPrm(arglist,name="^colorbar$",default=TRUE)
   if (!as.logical(colorbar))
      return(invisible(NULL))
   units <- NULL
   aname2 <- sapply(arglist,function(x) {
      y <- names(x)
      if (is.null(y))
         return("")
      if (.is.colortable(x)) {
         return("")
      }
      if (length(y)>1) {
         y <- paste0(head(y,1),"...",tail(y,1))
      }
      y
   })
   arglist <- lapply(arglist,function(x1) {
      if (identical(c("index","colortable"),names(x1)))
         return(x1$colortable)
      if (is.ursa(x1))
         return(x1$colortable)
      if ((is.list(x1))&&(length(x1)==1)) {
         return(x1[[1]])
      }
      x1
   })
   ind <- sapply(arglist,is.ursa,"colortable")
   if (length(which(ind))) {
      obj <- vector("list",length(ind))
      myname <- names(ind)
      if (length(ind2 <- which(!nchar(myname)))) {
         myname[ind2] <- aname2[ind2]
      }
      names(obj) <- myname
      ind2 <- rep(TRUE,length(ind))
      for (i in seq_along(obj)) {
         if (ind[i])
            obj[[i]] <- ursa_colortable(arglist[[i]])
         else {
            if (!nchar(myname[i]))
               obj[[i]] <- as.character(arglist[[i]])
            else
               ind2[i] <- FALSE
              # obj[[i]] <- NULL
         }
      }
      obj <- obj[ind2]
      arglist <- arglist[!ind2]
      isList <- TRUE
     # arglist <- list(NA)
      units <- names(obj)
   }
   else {
      if (inherits(arglist[[1]],"ursaLegend"))
         arglist[[1]] <- ursa_colortable(arglist[[1]])
      if (!inherits(arglist[[1]],"ursaColorTable")) {
         arglist[[1]] <- lapply(arglist[[1]],function(x1) {
            if (identical(c("index","colortable"),names(x1)))
               return(x1$colortable)
            if ((is.list(x1))&&(length(x1)==1)) {
               if (is.ursa(x1))
                  return(x1$colortable)
               else
                  return(x1[[1]])
            }
            x1
         })
      }
      ind <- sapply(arglist[[1]],is.ursa,"colortable")
      if ((is.list(ind))&&(!length(ind)))
         return(invisible(NULL))
      if (length(which(ind))) {
         obj <- vector("list",length(ind))
         names(obj) <- names(ind)
         for (i in seq_along(obj)) {
            if (ind[i])
               obj[[i]] <- ursa_colortable(arglist[[1]][[i]])
            else
               obj[[i]] <- as.character(arglist[[1]][[i]])
         }
         isList <- TRUE
      }
      else {
         obj <- .getPrm(arglist,name="",default=NULL
                       ,class=list(c("list","ursaRaster"),"ursaRaster"))
         if (is.null(obj)) {
            obj <- .getPrm(arglist,name="",default=NULL
                          ,class=list(c("list","ursaColorTable"),"ursaColorTable")
                          ,verbose=FALSE)
            obj <- .getPrm(arglist,index=1L,class="ursaColorTable",verbose=TRUE)
            isList <- all(sapply(obj,function(x) class(x) %in% "ursaColorTable"))
            if (is.null(obj))
               return(invisible(NULL))
         }
         else {
            isList <- .is.ursa_stack(obj)
            if (isList) {
               myname <- names(obj)
               obj <- lapply(obj,ursa_colortable)
               names(obj) <- myname
            }
            else
               obj <- ursa_colortable(obj)
         }
      }
   }
  # str(obj,grid=FALSE)
   mUnits <- .getPrm(arglist,name="unit(s)*",class=c("expression","character")
                    ,default=NA_character_)
   if (((is.expression(mUnits))||((is.character(mUnits))&&(is.na(mUnits[1]))))&&
         (length(mUnits)==length(units)))
      units <- mUnits
   else if ((is.null(units))||(all(!nchar(units)))) {
      units <- .getPrm(arglist,name="unit(s)*",class=c("expression","character")
                      ,default=NA_character_)
   }
   skip <- getOption("ursaPngSkipLegend")
   if (isList) {
      if ((!is.expression(units))&&(is.na(units[1]))) {
         units <- names(obj)
         if (is.null(units)) {
           # units <- sapply(obj,names)
            units <- sapply(obj,function(x) {
               if (is.ursa(x))
                  return(names(x))
               NULL
            })
         }
      }
      if ((is.null(units))||(length(units)!=length(obj))) {
         if (is.null(units))
            units <- rep("",len=length(obj))
         else
            units <- rep(units,len=length(obj))
      }
      for (i in seq_along(obj)) {
         if (i %in% skip)
            next
         if (i>nlegend)
            break
        # legend_colorbar(obj=obj[[i]],units=units[i],...)
         arglist2 <- c(quote(obj[[i]]),arglist) ## 20180308 change 'arglist[-1]'?
        # arglist[[1]] <- quote(obj[[i]])
         if (.is.colortable(obj[[i]])) {
            arglist2[["units"]] <- units[i]
           # str(arglist2)
            do.call("legend_colorbar",arglist2)
           # arglist[["units"]] <- NULL
         }
         else {
            do.call("legend_mtext",arglist2)
           # legend_mtext(obj[[i]])
         }
      }
   }
   else {
      myname <- names(arglist)
      if (is.null(myname))
         myname <- ""
      arglist <- arglist[match(unique(myname),myname)]
     # legend_colorbar(obj,units=units,...)
      do.call("legend_colorbar",arglist)
   }
   invisible(NULL)
}
