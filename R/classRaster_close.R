'close_envi' <- function(...) close(...)
'close.ursaRaster' <- function(...)
{
   args <- list(...)
   for (i in seq(along=args))
   {
      con <- args[[i]]$con
     # print(class(con))
      if (!.is.con(con))
         next
      if (inherits(con$handle,"connection")) {
         if ((F)&&(con$compress %in% c(0L,3L))&&(con$connection=="file")) {
            str(args[[i]])
            str(con)
         }
         if (any(con$fname %in% showConnections()[,"description"]))
            close(con$handle)
         con$handle <- NA
         if ((con$driver=="EGDAL")&&(length(con$fname)==2)) {
            with(con,.envi2gdal(src=fname[2],dst=fname[1],datatype=datatype,bands=bands))
            envi_remove(con$fname[2])
            con$compress <- 0L
         }
         if (con$compress==-1L)
            file.remove(con$fname)
         else if (con$compress==-2L)
         {
            if (is.null(fname <- attr(con$fname,"source")))
               fname <- .gsub("\\.unpacked(.*)~$",".envi",con$fname)
            else
               fname <- gsub("gz$","",fname)
            file.rename(con$fname,fname)
            if (file.exists(ftmp <- paste0(fname,".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.bin","",fname),".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.envi","",fname),".gz")))
               file.remove(ftmp)
            system(paste("gzip","-f -Sgz",dQuote(fname)))
         }
         else if (con$compress==1L)
         {
           # .elapsedTime("CLOSE 1")
            if (file.exists(ftmp <- paste0(con$fname,".gz")))
               file.remove(ftmp)
            if (nchar(Sys.which("gzip"))) {
               for (i in seq(10)) {
                  break
                  s <- file.size(con$fname)
                  print(c(i=i,s=s))
                  if (s)
                     break
                  Sys.sleep(7)
               }
               a <- system(paste("gzip","-f -Sgz",dQuote(con$fname))) ##keep
              # str(a)
            }
           # .elapsedTime("PASSED")
           # src <- paste0(con$fname,"gz")
           # dst <- file.path(dirname(src),.gsub("\\.bin",".gz",basename(con$fname)))
           # file.rename(src,dst)
         }
         else if (con$compress==3L) { ## compress==0 and at least one 'replace'
            if (dirname(con$fname)==.ursaCacheDir()) {
               was <- .ursaCacheRead()
               if (!is.null(was)) {
                  dst <- was$src[match(basename(con$fname),was$dst)]
                  if (file.exists(dst)) {
                     if ((.lgrep("(envi|bin|\\.)gz$",dst))&&
                                                  (nchar(Sys.which("gzip")))) {
                        system2("gzip",c("-f -9 -c -n",.dQuote(con$fname))
                               ,stdout=dst,stderr=FALSE)
                     }
                     else if ((.lgrep("\\.bz2$",dst))&&
                                                  (nchar(Sys.which("bzip2")))) {
                        system2("bzip2",c("-f -9 -c -n",.dQuote(con$fname))
                               ,stdout=dst,stderr=FALSE)
                     }
                  }
               }
            }
           # print(.ursaCasheLoc())
           # a <- .ursaCacheRead()
           # ind <- match(con$fname,)
           # str(a)
         }
      }
      else if (inherits(con$handle,"GDALTransientDataset")) {
         bname <- args[[i]]$name
         .rgdal_close_Transient(con,bname)
      }
      else if (inherits(con$handle,"GDALReadOnlyDataset")) {
        # print(class(con$handle))
         .rgdal_close_ReadOnly(con)
         con$handle <- NA
      }
   }
   invisible(NULL)
}
'.reopen' <- function(con) ## not called everywhere
{
   close(con$handle)
   open(con$handle)
   if (con$offset)
   {
      if (con$seek)
         seek(con$handle,where=con$offset,origin="start")
      else
         readBin(con$handle,raw(),n=con$offset)
   }
}
