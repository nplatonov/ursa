##~ 'panel_legend' <- function(items,pos="bottomright",x,y,cex=1.5,pch=22
                           ##~ ,box.col="transparent",bg="#FFFFFFAF"
                           ##~ ,fill=NULL,lty,lwd,angle=45,density=NULL,bty="o"
                           ##~ ,box.lwd=par("lwd"),box.lty=par("lty")
                           ##~ ,xjust=0,yjust=1,x.intersp=1,y.intersp=1
                           ##~ ,adj=c(0,0.5),text.width=NULL,text.col=par("col")
                           ##~ ,text.font=NULL,merge=TRUE,trace=FALSE
                           ##~ ,plot=TRUE,ncol=1,horiz=FALSE,title=NULL
                           ##~ ,inset=0,xpd,title.col=text.col,title.adj=0.5
                           ##~ ,seg.len=2)
'.shape_legend<-' <- function(obj,items,verbose,value) {
   res <- unlist(lapply(items,function(x) x[[value]]))
   if (!is.null(res)) {
      if (verbose) {
         str(value)
         str(res)
      }
      obj[[value]] <- res
   }
   obj
}
'panel_legend' <- function(items,...)
{
  # verbose <- !.isPackageInUse()
   verbose <- .getPrm(list(...),name="verb(ose)*",default=!.isPackageInUse())
   if (.skipPlot(TRUE))
      return(NULL)
   if (missing(items))
      items <- getOption("ursaPngLegend")
   else if (is.list(items)) {
      items <- lapply(items,function(item) {
         if ((length(item)==1)&&(is.list(item)))
            return(item[[1]]) # return(unlist(item,recursive=FALSE))
         item
      })
   }
   items <- items[!sapply(items,is.null)]
   if (!length(items))
      return(invisible(NULL))
   arglist <- head(as.list(args(legend)),-1)
   items2 <- list(list(...))
   if (verbose) {
      message("-------")
      str(items)
   }
   for (a in names(arglist)) {
      .shape_legend(arglist,items,verbose) <- a
      .shape_legend(arglist,items2,verbose) <- a
   }
   if (is.language(arglist[["lwd"]]))
      arglist[["lwd"]] <- NULL
   if (is.language(arglist[["lty"]]))
      arglist[["lty"]] <- NULL
   if (is.language(arglist[["box.lwd"]]))
      arglist[["box.lwd"]] <- 0.1
   if (is.language(arglist[["pt.cex"]]))
      arglist[["pt.cex"]] <- 1 #arglist[["cex"]]
   if (is.language(arglist[["pt.lwd"]]))
      arglist[["pt.lwd"]] <- arglist[["lwd"]]
   if (!nchar(as.character(arglist[["x"]])))
      arglist[["x"]] <- "bottomright"
   if ((!is.null(arglist[["lwd"]]))&&(all(is.na(arglist[["lwd"]]))))
      arglist[["lwd"]] <- NULL
   if ((!is.null(arglist[["lty"]]))&&(all(is.na(arglist[["lty"]]))))
      arglist[["lty"]] <- NULL
   if (all(is.na(arglist[["cex"]])))
      arglist[["cex"]] <- 1
   arglist[["merge"]] <- FALSE ## '$merge : language do.lines && has.pch'
   for (a in names(arglist)) {
      if (is.language(arglist[[a]])) {
         if (!sum(nchar(as.character(arglist[[a]]))))
            next
         if (verbose)
            message(a)
         if (isTRUE(getOption("ursaNoticeMatchCall")))
            message('panel_legend: try `mget(names(match.call())[-1])` instead of `as.list(match.call())`')
         res <- try(eval.parent(arglist[[a]]))
         if (inherits(res,"try-error")) {
            next
         }
         arglist[[a]] <- res
      }
   }
   arglist[["title.col"]] <- arglist[["text.col"]]
   arglist[["cex"]] <- arglist[["cex"]]/par()$cex
   lname <- names(items)
   iname <- c(sapply(items,function(x) x$name))
   if (is.null(lname))
      lname <- iname
   else if (length(ind <- which(!nchar(lname))))
      lname[ind] <- iname[ind]
   arglist[["legend"]] <- lname
  # arglist[["pch"]] <- unname(sapply(items,function(x) x$pch))
  # arglist[["title.cex"]] <- arglist[["cex"]]
   if (length(ind <- which(sapply(arglist,class) %in% "call"))) {
      arglist[ind] <- lapply(names(arglist)[ind],function(a) {
         arglist[[a]] <- with(arglist,eval(as.call(arglist[[a]])))
      })
   }
   if (verbose)
      str(arglist)
   ret <- do.call("legend",arglist)
   ##~ ret <- legend(x="topright",legend=c("213856","213857"),fill="transparent"
                ##~ ,col="white",border="transparent",pch=21,density=NA
                ##~ ,bty="o",bg="#0000007F",box,lwd=0.1,box.lty="solid"
                ##~ ,box.col="black",pt.bg=c('213856'="#978035FF",'213857'="#677EC9FF")
                ##~ ,pt.cex=2.5,pt.lwd=2,adj=c(0,0.5),text.col="white",merge=FALSE
                ##~ ,horiz=TRUE,title.col="white",title.adj=0.5)
   invisible(ret)
}
