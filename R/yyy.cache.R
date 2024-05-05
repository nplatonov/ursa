## ?fastdigest::fastdigest
'.ursaCacheDir' <- function() {
   fpath <- getOption("ursaCacheDir")
   if (!dir.exists(fpath))
      dir.create(fpath)
  # fclear <- file.path(fpath,"clear.R")
  # if (!file.exists(fclear))
  #    writeLines("ursa:::.ursaCacheDirClear(age=2,size=0.2,count=1e3)",fclear)
   fpath
}
'.ursaCacheFile' <- function(pattern="ursaCache") {
  # fpath <- getOption("ursaCacheDir")
  # if (!dir.exists(fpath))
  #    dir.create(fpath)
  # tempfile(tmpdir=fpath,pattern=pattern)
   .normalizePath(tempfile(tmpdir=.ursaCacheDir(),pattern=pattern))
}
'.ursaCacheInventory' <- function() file.path(.ursaCacheDir(),"ursaCache_inventory.txt")
'.ursaCacheVisits' <- function() file.path(.ursaCacheDir(),"ursaCache_visits.txt")
'.ursaCacheDirClear' <- function(size=getOption("ursaCacheSize")
                                ,age=getOption("ursaCacheAge")
                                ,count=10000,completely=FALSE) {
   if (!is.numeric(size))
      size <- 16
   if (!is.numeric(age))
      age <- 7
   fpath <- .ursaCacheDir()
   if (!file.exists(fpath))
      return(invisible(NULL))
   if (completely) {
      if (!dir.exists(fpath))
         return(invisible(NULL))
      file.remove(.dir(path=fpath,pattern=as.list(args(.ursaCacheFile))$pattern
                      ,full.names=TRUE))
      if (develHtmlWidgets <- TRUE) {
         file.remove(.dir(path=fpath,pattern="^htmlwidgets.+\\.html$",full.names=TRUE))
         dhw <- file.path(fpath,"htmlwidgets")
         if (dir.exists(dhw))
            unlink(dhw)
         file.remove(.dir(path=fpath,pattern="^___ursa\\d+.+$",full.names=TRUE)) ## '.*png' etc
      }
      unlink(fpath)
      return(invisible(NULL))
   }
   inventory <- .ursaCacheInventory()
   if (!file.exists(inventory)) {
      return(.ursaCacheDirClear(completely=TRUE)) ## RECURSIVE
   }
   was <- try(utils::read.table(inventory,sep=",",encoding="UTF-8"))
   if (inherits(was,"try-error")) {
      message("cache was removed completely due to damaged structure")
      return(.ursaCacheDirClear(completely=TRUE)) ## RECURSIVE
   }
   colnames(was) <- c("time","stamp","visits","size","src","dst")
   was <- was[rev(seq(nrow(was))),]
   was0 <- was
   was$src <- NULL
   was$time <- as.POSIXct(was$time,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
   t0 <- as.POSIXct(as.numeric(Sys.time()),origin="1970-01-01",tz="UTC")
   was$p1 <- unclass(difftime(t0,was$time,units="days"))
   was$p2 <- cumsum(was$size/1024)
   was$p3 <- row(was[,1,drop=FALSE])
   ind <- which(was$p1>age | was$p2>size*1024*1024 | was$p3>count)
   if (!length(ind))
      return(invisible(NULL))
   if (length(ind)==nrow(was)) {
      message("cache was removed completely")
      return(.ursaCacheDirClear(completely=TRUE)) ## RECURSIVE
   }
   dst <- file.path(fpath,was0$dst[ind])
   dst <- dst[file.exists(dst)]
   if (FALSE) {
      print(was)
      print(c(size=size,age=age,count=count))
      print(c(toRemove=dst))
      q()
   }
   file.remove(dst)
   dst <- paste0(dst,".hdr")
   dst <- dst[file.exists(dst)]
   file.remove(dst)
   was0 <- was0[-ind,]
   was0 <- was0[rev(seq(nrow(was0))),]
   ##~ utils::write.table(was0,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                     ##~ ,file=inventory)
   .ursaCacheWrite(was0,append=FALSE)
   return(invisible(NULL))
}
'.ursaCacheExpired' <- function(value) {
   if (is.character(value)) {
      if (!.lgrep("\\-",value))
         value <- paste0("-",value)
      if (!.lgrep("\\d\\s\\D",value))
         value <- gsub("(^.*\\d)(\\D.+$)","\\1 \\2",value)
      expired <- as.integer(tail(seq(Sys.time(),len=2,by=value),1))
     # expire <- as.POSIXlt(tail(seq(Sys.time(),len=2,by=value),1),tz="UTC")
      attr(expired,"cache") <- TRUE
   }
   else {
      expired <- as.integer(tail(seq(Sys.time(),len=2,by="-1 month"),1)) ## not '+'
     # expired <- as.POSIXlt(tail(seq(Sys.time(),len=2,by="-1 month"),1),tz="UTC")
      attr(expired,"cache") <- value
   }
   expired
}
'.ursaCacheDirZero' <- function() {
   list1 <- list.dirs(.ursaCacheDir())
   for (i in seq(9)) {
      list2 <- sapply(list1,function(x) {
         list3 <- dir(path=x,recursive=FALSE)
         length(list3)==0
      })
      list2 <- names(list2)[list2]
      if (!length(list2))
         break
      sapply(list2,unlink,force=TRUE,recursive=TRUE)
   }
   0L
}
'.ursaCacheDownload' <- function(src,dst,method,quiet=FALSE,cache=TRUE,mode="w"
                                ,extra=getOption("download.file.extra"),headers=NULL) {
   enc <- "UTF-8"
   inventory <- .ursaCacheInventory()
   src0 <- src
   attr(src0,"credentials") <- NULL
   if (!is.null(cred <- attr(src,"credentials"))) {
      for (cr in cred) {
         m1 <- gregexpr(paste0("\\{",cr,"\\}"),src)[[1]]
         if (any(m1<0))
            next
         m2 <- regmatches(src,m1)
         m3 <- gsub("^\\{(.+)\\}$","\\1",m2)
         if (grepl("thunderforest\\.com",src)) {
            getCr <- getOption("ThunderforestApiKey")
         }
         else if (grepl("api\\.here\\.com",src)) {
            getCr <- getOption("HEREapp")
         }
         else if (grepl("api\\.mapbox\\.com",src)) {
            getCr <- getOption("mapboxToken")
         }
         else
            getCr <- NULL
         if (is.null(getCr))
            next
         if (is.character(getCr))
            value <- getCr
         else {
            value <- getCr[[match(cr,names(getCr))]]
         }
         if ((is.character(value))&&(length(value)==1))
            regmatches(src,m1) <- value
      }
      attr(src,"credentials") <- NULL
   }
   else if (grepl("tiles\\.stadiamaps\\.com",src)) {
      getCr <- getOption("stadiamaps_api_key")
      if (!is.null(getCr)) {
         src <- paste0(src,ifelse(grepl("\\.(png|jpg)$",src),"?","&")
                      ,"api_key=",getCr)
      }
   }
   src1 <- src
   patt <- "(^http(s)*://(tile|mt)*)\\{(.+)\\}(.+$)" ## \\1 \\4 \\5
   if (.lgrep(patt,src)) {
      dom <- unlist(strsplit(gsub(patt,"\\4",src),""))
      src <- unname(sapply(sample(dom),function(x) gsub(patt,paste0("\\1",x,"\\5"),src1)))
     # dom <- unlist(strsplit(.gsub2("\\{(.+)\\}","\\1",gsub("\\{.\\}","",src)),""))
     # src <- .gsub("{.+}",sample(dom,1),src0)
     # src <- unname(sapply(sample(dom),function(x) .gsub("{.+}",x,src0)))
   }
   if (missing(dst))
      dst <- NULL
   expired <- .ursaCacheExpired(cache)
  # message("expired:")
  # str(expired)
  # str(as.POSIXct(expired,origin=.origin()))
   cache <- attr(expired,"cache")
   ##~ if (is.character(cache)) {
      ##~ if (!.lgrep("\\-",cache))
         ##~ cache <- paste0("-",cache)
      ##~ if (!.lgrep("\\d\\s\\D",cache))
         ##~ cache <- gsub("(^.*\\d)(\\D.+$)","\\1 \\2",cache)
      ##~ expired <- as.POSIXlt(tail(seq(Sys.time(),len=2,by=cache),1),tz="UTC")
      ##~ cache <- TRUE
   ##~ }
   ##~ else
      ##~ expired <- as.POSIXlt(tail(seq(Sys.time(),len=2,by="-7 days"),1),tz="UTC")
   if (cache) {
     # message("use cache")
      if (file.exists(inventory)) {
        # message("check inventory")
         was <- utils::read.table(inventory,sep=",",encoding=enc)
         colnames(was) <- c("time","stamp","visits","size","src","dst")
         if (F & is.character(dst)) {
            stop("dst")
         }
         ind <- tail(which(!is.na(match(was$src,src0))),1) ## match(src0,was$src)
         if ((length(ind))&&(!is.na(ind))) {
           # message("found in inventory")
           # str(was$stamp[ind])
           # str(as.POSIXct(was$stamp[ind],origin=.origin()))
           # t2 <- as.POSIXlt(was$time[ind],format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
            if (was$stamp[ind]>=expired) { ## t2>=expired
              # message("not expired")
               dst <- file.path(.ursaCacheDir(),was$dst[ind])
            }
         }
      }
   }
   if ((is.null(dst))||(!file.exists(dst))) {
     # message("download is required")
      if (!length(src))
         return(NULL)
      if (is.null(dst))
         dst <- if (cache) .ursaCacheFile() else tempfile()
      for (i in seq_along(src)) {
         ret <- try(download.file(url=URLencode(iconv(src[i],to="UTF-8")) 
                      ,destfile=dst,method=method,quiet=quiet,mode=mode
                      ,extra=extra,headers=headers))
         if (!inherits(ret,"try-error"))
            break
      }
      if (inherits(ret,"try-error"))
         return(ret)
      if (cache)
         ##~ utils::write.table(
            ##~ data.frame(time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                      ##~ # ,stamp=as.integer(Sys.time())
                       ##~ ,stamp=as.integer(file.mtime(dst))
                       ##~ ,visits=0L
                       ##~ ,size=file.size(dst)
                       ##~ ,src=src0,dst=basename(dst))
            ##~ ,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
            ##~ ,file=inventory,append=TRUE,fileEncoding=enc)
         .ursaCacheWrite(.ursaCacheRecord(dst,src=src0),append=TRUE)
   }
   else if (cache) {
      Fout <- file(.ursaCacheVisits(),"at")
      writeLines(basename(dst),Fout)
      close(Fout)
   }
   dst
}
'.ursaCacheRaster' <- function(src,unpack=c("none","gzip","bzip2"),reset=FALSE) {
   enc <- "UTF-8"
   unpack <- match.arg(unpack)
   finfo <- file.info(src)
   ftime <- as.integer(finfo$mtime)
   fsize <- finfo$size
   dst <- NULL # .ursaCacheFile() ## 
   inventory <- .ursaCacheInventory()
   ind <- NA
   if (file.exists(inventory)) {
      was <- utils::read.table(inventory,sep=",",encoding=enc)
      colnames(was) <- c("time","stamp","visits","size","src","dst")
      if (is.character(dst)) {
         stop("dst")
      }
      if (FALSE) {
         wasP <- was
         wasP$src <- substr(wasP$src,1,12)
         print(wasP)
      }
      if (FALSE) {
         ind1 <- match(.normalizePath(src),was$src)
         ind2 <- match(ftime,was$stamp)
         ind3 <- ind2 ## dummy for one more check
         if (!anyNA(c(ind1,ind2,ind3))&&(ind1==ind2)&&(ind2==ind3)) {
            dst <- file.path(.ursaCacheDir(),was$dst[ind1[1]])
            ind <- ind1
         }
      }
      else {
         ind1 <- which(!is.na(match(was$src,.normalizePath(src))))
         ind2 <- which(!is.na(match(was$stamp,ftime)))
         ta <- table(c(ind1,ind2))
         ta <- ta[ta==2]
         if (length(ta)) {
            ind <- as.integer(names(ta))
            dst <- file.path(.ursaCacheDir(),was$dst[ind[1]])
         }
      }
   }
   if (reset) {
      if (!is.null(dst)) {
         was <- was[-ind,]
         file.remove(dst)
         dst <- NULL
      }
      else
         reset <- FALSE
   }
   if (unpack!="none") {
      if ((!is.null(dst))&&(!envi_exists(dst)))
         dst <- NULL
      if (is.null(dst)) {
         dst <- .ursaCacheFile()
         if (unpack %in% c("gzip","bzip2")) {
            if (unpack=="gzip") {
               system2("gzip",c("-f -d -c",.dQuote(src)),stdout=dst,stderr=FALSE)
            }
            else if (unpack=="bzip2")
               system2("bzip2",c("-f -d -c",.dQuote(src)),stdout=dst,stderr=FALSE)
            if (debugExact <- F) {
               str(src)
               str(dst)
               cat("------------\n")
               str(envi_list(src,exact=TRUE))
               cat("------------\n")
               q()
            }
            if (length(listE <- envi_list(src,exact=TRUE)))
               file.copy(paste0(listE,".hdr"),paste0(dst,".hdr"),copy.date=TRUE)
         }
         ##~ da <- data.frame(time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                         ##~ ,stamp=ftime,visits=0L,size=file.size(dst)
                         ##~ ,src=.normalizePath(src),dst=basename(dst))
         da <- .ursaCacheRecord(dst,src=.normalizePath(src),ftime=ftime)
         if (reset)
            da <- rbind(was,da)
         ##~ utils::write.table(da,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                           ##~ ,file=inventory,append=!reset,fileEncoding=enc)
         .ursaCacheWrite(da,append=!reset)
      }
      else {
         Fout <- file(.ursaCacheVisits(),"at")
         writeLines(basename(dst),Fout)
         close(Fout)
      }
   }
   dst
}
'.ursaCacheRecord' <- function(dst,src=NULL,ftime=NULL) {
   if (.lgrep("^file:///",dst))
      dst <- .gsub("^file:///","",dst)
   if (is.null(src))
      src <- basename(dst)
   if (is.null(ftime))
      ftime <- file.mtime(dst)
   da <- data.frame(time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                   ,stamp=as.integer(ftime)
                   ,visits=0L
                   ,size=file.size(dst)
                   ,src=src
                   ,dst=basename(dst)
                   )
   da
}
'.ursaCacheWrite' <- function(da,append=TRUE) {
   inventory <- .ursaCacheInventory()
   utils::write.table(da,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                     ,file=inventory,append=append,fileEncoding="UTF-8")
}
'.ursaCacheRead' <- function(fname) {
   inventory <- .ursaCacheInventory()
   if (!file.exists(inventory))
      return(NULL)
   was <- utils::read.table(inventory,sep=",",encoding="UTF-8")
   stopifnot(ncol(was)==6)
   colnames(was) <- c("time","stamp","visits","size","src","dst")
   was
}
'.ursaCacheFind' <- function(loc) {
   was <- .ursaCacheRead()
   if (is.null(was))
      return(0L)
   ind <- match(loc,was$src)
   if (is.na(ind)) {
      ind <- match(basename(loc),was$dst)
   }
   if (is.na(ind))
      return(0L)
   ind
}
'.atOnceCacheRebuildAndForget' <- function() {
   a <- utils::read.table("_inventory.txt",sep=",",dec=".")
   a <- data.frame(a[,1:2],B=0,a[,3:5])
   str(a)
   utils::write.table(a,"_inventory.new",sep=",",dec=".",col.names=FALSE,row.names=FALSE)
}
