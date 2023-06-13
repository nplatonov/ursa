'widgetize' <- function(obj,...) {
  # if (!requireNamespace("widgetframe",quietly=.isPackageInUse()))
  #    return(browse(obj))
  # widgetframe::frameWidget(obj)
   if (is.null(obj))
      return(invisible(obj))
   if (!.isKnitr()) {
      if (is.character(obj)) {
         return(cat(obj,sep="\n"))
      }
     # cat("not knit","a\n")
      return(browse(obj,...))
   }
  # if (.isRemark())
  #    return(browse(obj,...))
   if (inherits(obj,c("datatables")))
      return(browse(obj,...))
   if (inherits(obj,c("htmlwidget"))) {
     # cat("htmlwidget 1","\n")
     # cat("knitr:",c(knitr="knitr" %in% loadedNamespaces()),"\n")
     # cat("asis:",knitr::opts_current$get("results")=="asis","\n")
     # cat("class:",class(obj),"\n")
      if ("knitr" %in% loadedNamespaces()) {
         if (!isTRUE(knitr::opts_chunk$get("customized_rendering")))
            return(obj)
         if (isTRUE(knitr::opts_current$get("results")=="asis")) {
           # cat("asis 2","\n")
            return(browse(obj,...))
         }
      }
   }
   if (!inherits(obj,c("htmlwidget","knitr_kable"))) {
     # cat("htmlwidget 3","\n")
      return(browse(obj,...))
   }
  # cat("obj 4","\n")
   obj
}
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
   if (length(ind <- grep("^width$",ignore.case=FALSE,names(arglist)))) {
      ind2 <- tail(ind,1)
      width <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      width <- NULL
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
      verbose <- FALSE # !.isPackageInUse()
   if (length(ind <- .grep("(fig\\.cap|caption)",names(arglist)))) {
      ind2 <- tail(ind,1)
      cap <- arglist[[ind2]]
      arglist[ind2] <- NULL
   }
   else
      cap <- NULL
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
   if ((is.null(oname))&&(!is.null(cap)))
      oname <- cap
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
     # if (inherits(obj,"tmap")) {
     #    if (requireNamspace("tmap"))
     #    obj <- tmap::tmap_leaflet(obj)
     # }
      if (inherits(obj,c("tmap","htmlwidget"))) {
         isUrsaCache <- !identical(normalizePath(.ursaCacheDir()),tempdir())
         if (!isUrsaCache & is.null(output))
            return(ret2)
         cond2 <- requireNamespace("htmlwidgets",quietly=.isPackageInUse())
         if (!cond2)
            return(ret2)
         ftemp <- tempfile(fileext=".rds")
         o <- obj[grep("dependencies",names(obj),invert=TRUE)]
         saveRDS(o,ftemp)
         md5 <- tools::md5sum(ftemp)
         file.remove(ftemp)
         if (is.null(output)) {
            if (!.isKnitr())
               output <- file.path(.ursaCacheDir(),"knit")
            else
               output <- file.path(knitr::opts_chunk$get("fig.path"))
         }
         else {
           # TODO 'if (length(grep("\\.html$",output)))...' 
            output <- normalizePath(output)
         }
         if (!dir.exists(output))
            dir.create(output,recursive=TRUE)
         if (!.isKnitr()) {
            libdir <- file.path(output,"site_libs")
            fname <- file.path(output,paste0("htmlwidget_",unname(md5),".html"))
         }
         else {
            libdir <- file.path("libs")
            fname <- file.path(output,paste0("widget_"
                                            ,knitr::opts_current$get("label")
                                            ,"_",unname(md5)
                                            ,".html"))
            caption <- knitr::opts_current$get("fig.cap")
            if ((is.null(oname))&&(!is.null(caption)))
               oname <- caption
         }
         if (!file.exists(fname)) {
           # obj <- htmlwidgets::prependContent(obj,htmltools::HTML("<style>iframe {border: 3px solid magenta;}</style>"))
            if (requireNamespace("widgetframe",quietly=.isPackageInUse())) {
               obj <- widgetframe::frameableWidget(obj)
            }
            if (!.isPackageInUse())
               opW <- options(warn=1)
            htmlwidgets::saveWidget(obj,file=fname,libdir=libdir
                                   ,selfcontained=FALSE
                                   ,knitrOptions=list(hello="World")
                                   )
            if (!.isPackageInUse())
               options(opW)
         }
         if (T | !.isKnitr()) {
            a <- readLines(fname,encoding="UTF-8")
            a <- grep("application/json.+data-for=\\\"htmlwidget",a,value=TRUE)
            id <- gsub("^.+visdat\\W+([0-9a-f]+)\\W+.+$","\\1",a)
            if ((nchar(id)>4)&&(nchar(id)<36)) {
               a <- gsub(id,"gggg",a)
               a <- gsub("(^.+htmlwidget\\W+)([0-9a-f]+)(\\W+.+$)","\\1hhhhh\\3",a)
               saveRDS(a,ftemp)
               md5 <- unname(tools::md5sum(ftemp))
               file.remove(ftemp)
               ename <- fname
               if (!.isKnitr())
                  fname <- file.path(dirname(fname),paste0("htmlwidget-",md5,".html"))
               else
                  fname <- file.path(dirname(fname),paste0("widget_"
                                                  ,knitr::opts_current$get("label")
                                                  ,"_",unname(md5)
                                                  ,".html"))
               file.rename(ename,fname)
            }
         }
        # oname <- "PUT YOUR CAPTION HERE"
         fname <- gsub("\\\\","/",fname)
         if ((T | !.isKnitr())&&(length(grep("^(/\\w|[A-Z\\:\\w])",fname))))
            fname <- paste0("file:///",fname)
        # cap <- paste0("<a href=",URLencode(fname),">",oname[k],"</a>")
         if (link) {
            cap <- paste0("^[&#128279;](",URLencode(fname),")^") ## []{style=\"opacity: 0.3\"}
           # cap <- paste0("<span style=\"color: magenta;\">[&#128279;](",URLencode(fname),")</span>") ## []{style=\"opacity: 0.3\"}
         }
         else
            cap <- ""
         if ((!is.null(oname))&&(nchar(oname[k])>0))
            cap <- paste(oname[k],cap)
         iframe <- paste0("knitr::include_url(",dQuote(fname))
         if (!is.null(height)) {
            if (is.character(height))
               iframe <- paste0(iframe,",height=",dQuote(height))
            if (is.numeric(height))
               iframe <- paste0(iframe,",height=",dQuote(height)) ## dQuote(height,c("","px")[1])
         }
         iframe <- paste0(iframe,")")
         cmd <- c(paste0("```{r",reflab[k]
                        ,", echo=F"
                       # ,if (!is.null(oname)) paste0(", fig.cap=",dQuote(oname[k]))
                        ,if (T | !nchar(cap)) paste0(", fig.cap=",dQuote(cap))
                        ,paste0(", out.extra=",dQuote("class=\\\'ursa-widgetize\\\'"))
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
        # print(iframe)
        # print(cmd)
        # str(knitr::opts_current$get())
         if (!.isKnitr()) {
            if (verbose)
               cat(cmd,sep="\n")
            ret2 <- browseURL(fname)
         }
         else {
            if (T) {
               if (.isRemark()) {
                  if (is.null(height))
                     height <- knitr::opts_current$get("out.height")
                  if (is.null(width))
                     width <- "100%"
                  suffix <- paste0(" style=\"height:",height,"px; width=",width,"\"")
               }
               else
                  suffix <- ""
               cmd <- c(paste0("<div class=\"framed\"",suffix,">"),cmd,"</div>")
            }
           # writeLines(cmd,"c:/tmp/cap.Rmd")
           # knitr::asis_output(cmd)
            if (!.isRemark()) {
               cmd <- knitr::knit_child(text=cmd,quiet=TRUE)
            }
            else {
               cmd <- paste0("<div class=\"figure\">\n"
                            ,"<div class=\"framed\"",suffix,">\n"
                            ,"<iframe src=",dQuote(fname)
                            ,ifelse(is.null(width),"",paste0(" width=",dQuote(width)))
                            ,ifelse(is.null(height),"",paste0(" height=",dQuote(height)))
                            ," class=",dQuote("ursa-widgetize"),">"
                            ,"</iframe>\n"
                            ,"</div>\n"
                            ,if (nchar(cap))
                                paste0("<p class=",dQuote("caption"),">"
                                      ,cap," </p>\n")
                            ,"</div>")
            }
           # print(cmd)
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
