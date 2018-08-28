'.ursaCacheDir' <- function() {
   fpath <- getOption("ursaCacheDir")
   if (!dir.exists(fpath))
      dir.create(fpath)
  # fclear <- file.path(fpath,"clear.R")
  # if (!file.exists(fclear))
  #    writeLines("ursa:::.ursaCacheDirClear(age=2,size=0.2,count=1e3)",fclear)
   fpath
}
'.ursaCacheFile' <- function(pattern="cache") {
  # fpath <- getOption("ursaCacheDir")
  # if (!dir.exists(fpath))
  #    dir.create(fpath)
  # tempfile(tmpdir=fpath,pattern=pattern)
   .normalizePath(tempfile(tmpdir=.ursaCacheDir(),pattern=pattern))
}
'.ursaCacheInventory' <- function() file.path(.ursaCacheDir(),"_inventory.txt")
'.ursaCacheVisits' <- function() file.path(.ursaCacheDir(),"_visits.txt")
'.ursaCacheDirClear' <- function(size=16,count=10000,age=7,completely=FALSE) {
   fpath <- .ursaCacheDir()
   if (!file.exists(fpath))
      return(invisible(NULL))
   if (completely) {
      if (!dir.exists(fpath))
         return(invisible(NULL))
      file.remove(.dir(path=fpath,full.names=TRUE))
      unlink(fpath)
      return(invisible(NULL))
   }
   inventory <- .ursaCacheInventory()
   if (!file.exists(inventory)) {
      return(.ursaCacheDirClear(completely=TRUE)) ## RECURSIVE
   }
   was <- try(utils::read.table(inventory,sep=","))
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
   file.remove(dst)
   was0 <- was0[-ind,]
   was0 <- was0[rev(seq(nrow(was0))),]
   utils::write.table(was0,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                     ,file=inventory)
   return(invisible(NULL))
}
'.ursaCacheDownload' <- function(src,dst,method,quiet=FALSE,cache=TRUE,mode="w") {
   enc <- "UTF-8"
   inventory <- .ursaCacheInventory()
   src0 <- src
   if (.lgrep("\\{..+}",src)) {
      dom <- unlist(strsplit(.gsub2("\\{(.+)\\}","\\1",gsub("\\{.\\}","",src)),""))
     # src <- .gsub("{.+}",sample(dom,1),src0)
      src <- unname(sapply(sample(dom),function(x) .gsub("{.+}",x,src0)))
   }
   if (missing(dst))
      dst <- NULL
   if (cache) {
      if (file.exists(inventory)) {
         was <- utils::read.table(inventory,sep=",",encoding=enc)
         colnames(was) <- c("time","stamp","visits","size","src","dst")
         if (is.character(dst)) {
            stop("dst")
         }
         ind <- match(src0,was$src)
         if (!is.na(ind)) {
            dst <- file.path(.ursaCacheDir(),was$dst[ind[1]])
         }
      }
   }
   if ((is.null(dst))||(!file.exists(dst))) {
      if (is.null(dst))
         dst <- if (cache) .ursaCacheFile() else tempfile()
      for (i in seq_along(src)) {
         ret <- try(download.file(url=URLencode(iconv(src[i],to="UTF-8")) 
                      ,destfile=dst,method=method,quiet=quiet,mode=mode))
         if (!inherits(ret,"try-error"))
            break
      }
      if (inherits(ret,"try-error"))
         return(ret)
      if (cache)
         utils::write.table(data.frame(time=format(Sys.time()
                                                ,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                                      ,stamp=as.integer(Sys.time())
                                      ,visits=0L
                                      ,size=file.size(dst)
                                      ,src=src0,dst=basename(dst))
                           ,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                           ,file=inventory,append=TRUE,fileEncoding=enc)
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
      if (is.null(dst)) {
         dst <- .ursaCacheFile()
         if (unpack %in% c("gzip","bzip2")) {
            if (unpack=="gzip")
               system2("gzip",c("-f -d -c",.dQuote(src)),stdout=dst,stderr=FALSE)
            else if (unpack=="bzip2")
               system2("bzip2",c("-f -d -c",.dQuote(src)),stdout=dst,stderr=FALSE)
            if (length(listE <- envi_list(src,exact=TRUE)))
               file.copy(paste0(listE,".hdr"),paste0(dst,".hdr"),copy.date=TRUE)
         }
         da <- data.frame(time=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                         ,stamp=ftime,visits=0L,size=file.size(dst)
                         ,src=.normalizePath(src),dst=basename(dst))
         if (reset)
            da <- rbind(was,da)
         utils::write.table(da,quote=TRUE,col.names=FALSE,row.name=FALSE,sep=","
                           ,file=inventory,append=!reset,fileEncoding=enc)
      }
      else {
         Fout <- file(.ursaCacheVisits(),"at")
         writeLines(basename(dst),Fout)
         close(Fout)
      }
   }
   dst
}
'.atOnceCacheRebuild' <- function() {
   a <- utils::read.table("_inventory.txt",sep=",",dec=".")
   a <- data.frame(a[,1:2],B=0,a[,3:5])
   str(a)
   utils::write.table(a,"_inventory.new",sep=",",dec=".",col.names=FALSE,row.names=FALSE)
}
