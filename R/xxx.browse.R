'browse' <- '.open' <- function(...) {
   arglist <- list(...)
   if (length(ind <- grep("^ref$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      reflab <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      reflab <- NULL
   if (length(ind <- grep("^dpi$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      dpi <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      dpi <- NULL
   if (length(ind <- grep("^height$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      height <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      height <- NULL
   if (length(ind <- grep("^output$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      output <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      output <- NULL
   if (length(ind <- grep("^link$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      link <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      link <- FALSE
   if (length(ind <- .grep("verb(ose)*",names(arglist)))) {
      ind2 <- tail(ind,1)
      verbose <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      verbose <- !.isPackageInUse()
   cl <- lapply(arglist,class)
   aname <- NULL
   if ((length(cl)==1)&&(cl=="list")) {
      aname <- names(arglist)
      arglist <- arglist[[1]]
      cl <- lapply(arglist,class)
     # if (verbose)
     #    str(cl)
   }
   oname <- names(arglist)
   if (sum(nchar(oname))==0) {
      if ((!is.null(aname))&&(length(aname)==1)) {
         oname <- rep("",length(arglist))
         oname[length(oname)] <- aname
      }
      else
         oname <- NULL
   }
   if (is.null(reflab))
      reflab <- if (T) rep("",length(arglist))
                else paste(sample(letters,4,replace=TRUE),collapse="")
   if ((length(arglist)>1)&&(length(reflab)==1))
      reflab <- paste0(reflab,seq_along(arglist))
   if ((length(arglist)>1)&&(length(dpi)==1))
      dpi <- rep(dpi,length(arglist))
   if (all(nchar(reflab)>0))
      reflab <- paste0(" ",reflab)
   k <- 0L
  # return(oname)
   ret <- lapply(arglist,function(obj) {
      k <<- k+1L
      if (verbose)
         print(k)
      ret2 <- NULL
      if (is.character(obj)) {
         if (!file.exists(obj)) { ## e.g.,Rplots%02d.png
            obj <- head(dir(path=dirname(obj)
                             ,pattern=gsub("\\%(\\d+)*d","\\\\d+",obj)
                             ,full.names=TRUE),1)
         }
         if (!length(obj))
            return(ret2)
         if (length(grep("\\.(png|gif)$",basename(obj))))
            cmd <- c(paste0("```{r",reflab[k]
                           ,", echo=F"
                           ,if (!is.null(oname)) paste0(", fig.cap=",dQuote(oname[k]))
                           ,"}")
                    ,paste0("knitr::include_graphics(",dQuote(obj)
                                   ,if (!is.null(dpi)) paste0(",dpi=",dpi[k])
                                  # ,",height=",dQuote(paste0(round(4.8*96,1),"px"))
                                   ,")")
                    ,"```")
         else
            cmd <- NULL
         if (!.isKnitr()) {
            if ((verbose)&&(!is.null(cmd)))
               cat(cmd,sep="\n")
            ret2 <- browseURL(normalizePath(obj))
         }
         else {
            cmd <- knitr::knit_child(text=cmd,quiet=TRUE)
            cat(cmd,sep="\n")
            ret2 <- invisible(NULL)
         }
      }
      if (inherits(obj,"mapview"))
         obj <- methods::slot(obj,"map")
      if (inherits(obj,"htmlwidget")) {
         isUrsaCache <- !identical(normalizePath(.ursaCacheDir()),tempdir())
         if (!isUrsaCache & is.null(output))
            return(ret2)
         cond2 <- requireNamespace("htmlwidgets",quietly=.isPackageInUse())
         if (!cond2)
            return(ret2)
         ftemp <- tempfile(fileext=".rds")
         saveRDS(obj,ftemp)
         md5 <- tools::md5sum(ftemp)
         file.remove(ftemp)
         if (is.null(output))
            output <- .ursaCacheDir()
         else {
           # TODO 'if (length(grep("\\.html$",output)))...' 
            output <- normalizePath(output)
         }
         libdir <- file.path(output,"htmlwidgets")
         fname <- file.path(output,paste0("htmlwidget_",unname(md5),".html"))
         if (!file.exists(fname)) {
           # obj <- htmlwidgets::prependContent(obj,htmltools::HTML("<style>iframe {border: 3px solid magenta;}</style>"))
            htmlwidgets::saveWidget(obj,file=fname,libdir=libdir
                                   ,selfcontained=FALSE)
         }
         fname <- gsub("\\\\","/",fname)
         if (.lgrep("^(/\\w|[A-Z\\:\\w])",fname))
            fname <- paste0("file:///",fname)
        # cap <- paste0("<a href=",URLencode(fname),">",oname[k],"</a>")
         if (link) {
            cap <- paste0("^[&#128279;](",URLencode(fname),")^") ## []{style=\"opacity: 0.3\"}
           # cap <- paste0("<span style=\"color: magenta;\">[&#128279;](",URLencode(fname),")</span>") ## []{style=\"opacity: 0.3\"}
         }
         else
            cap <- ""
         if (!is.null(oname))
            cap <- paste(oname[k],cap)
         iframe <- paste0("knitr::include_url(",dQuote(fname))
         if (!is.null(height)) {
            if (is.character(height))
               iframe <- paste0(iframe,",height=",dQuote(height))
            if (is.numeric(height))
               iframe <- paste0(iframe,",height=",dQuote(height,c("","px")[1]))
         }
         iframe <- paste0(iframe,")")
         cmd <- c(paste0("```{r",reflab[k]
                        ,", echo=F"
                       # ,if (!is.null(oname)) paste0(", fig.cap=",dQuote(oname[k]))
                        ,if (T | !nchar(cap)) paste0(", fig.cap=",dQuote(cap))
                        ,paste0(", out.extra=",dQuote("style='class=\\\"reset\\\"'"))
                        ,"}")
                 ##~ ,paste0("knitr::include_url(",dQuote(fname)
                                            ##~ #,",height=",dQuote(paste0(round(4.8*96,1),"px"))
                                            ##~ ,")")
                 ,iframe
                 ,"```"
                # ,""
                # ,paste0("[link to widget](",fname,"){style=\"font-size: 75%;\"}")
                # ,paste0("[&#128279;](",URLencode(fname),"){style=\"font-size: 75%; opacity: 0.6;\" class=\"noprint\"}")
                 )
         if (!.isKnitr()) {
            if (verbose)
               cat(cmd,sep="\n")
            ret2 <- browseURL(fname)
         }
         else {
           # knitr::asis_output(cmd)
            cmd <- knitr::knit_child(text=cmd,quiet=TRUE)
            cat(cmd,sep="\n")
            ret2 <- invisible(NULL)
         }
         ##~ inventory <- .ursaCacheInventory()
         ##~ if (file.exists(inventory)) {
            ##~ was <- utils::read.table(inventory,sep=",",encoding=enc)
            ##~ colnames(was) <- c("time","stamp","visits","size","src","dst")
         ##~ }
         ##~ else
            ##~ was <- NULL
         ##~ data.frame(time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                         ##~ ,stamp=as.integer(file.mtime(fname))
                         ##~ ,visits=0L
                         ##~ ,size=file.size(fname)
                         ##~ ,src=basename(fname)
                         ##~ ,dst=basename(fname)
                         ##~ )
        # da <- rbind(was,da)
         ##~ utils::write.table(da,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                           ##~ ,file=inventory,append=TRUE,fileEncoding="UTF-8")
         if ((isUrsaCache)&&(!.ursaCacheFind(fname))) {
            da <- .ursaCacheRecord(fname)
            .ursaCacheWrite(da,append=TRUE)
         }
      }
      ret2
   })
   if (length(ret)==1)
      ret <- ret[[1]]
   invisible(ret)
}
