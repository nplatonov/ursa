'envi_exists' <- function(pattern=".+",path=".",all.files=FALSE,full.names=TRUE
                         ,recursive=FALSE,ignore.case=TRUE,exact=FALSE)
{
   length(envi_list(pattern=pattern,path=path,all.files=all.files
                   ,full.names=full.names,recursive=recursive
                   ,ignore.case=ignore.case,exact=exact))
}
'envi_copy' <- function(src,dst,overwrite=TRUE)
{
   patlist <- "(\\.(hdr|gz|bz2|xz|bin|bingz|envi|envigz|img|dat))*$"
   srcdir <- dirname(src)
   srcname <- basename(src)
   n <- nchar(dst)
   if (substr(dst,n,n)=="/")
      dst <- substr(dst,1,n-1)
   if (missing(dst))
   {
      dstdir <- "."
      dstname <- srcname
   }
   else if ((file.exists(dst))&&(file.info(dst)$isdir))
   {
      dstdir <- dst
      dstname <- srcname
   }
   else
   {
      dstdir <- dirname(dst)
      dstname <- basename(dst)
   }
   srcname <- paste0("^",srcname)
   list1 <- .dir(path=srcdir
      ,pattern=paste0(srcname,patlist)
      ,full.names=TRUE)
   if (overwrite)
   {
      list2 <- .dir(path=dstdir
         ,pattern=paste0(dstname,patlist)
         ,full.names=TRUE)
      if (length(list2))
         file.remove(list2)
   }
   list2 <- file.path(dstdir,.gsub(srcname,dstname,basename(list1)))
   for (i in seq_along(list1))
      file.copy(list1[i],list2[i],overwrite=overwrite,copy.date=TRUE)
   invisible(0L)
}
'envi_remove' <- function(pattern=".+",path=".",all.files=FALSE,full.names=recursive
                         ,recursive=FALSE,ignore.case=TRUE,verbose=FALSE)
{
   list2 <- NULL
   for (a in pattern)
   {
      list2 <- c(list2,.delete.envi.each(pattern=a,path=path
                                        ,all.files=all.files
                                        ,full.names=full.names
                                        ,recursive=recursive
                                        ,ignore.case=ignore.case
                                        ,verbose=verbose))
   }
   invisible(list2)
}
'.delete.envi.each' <- function(pattern=".+",path=".",all.files=FALSE
                               ,full.names=FALSE,recursive=FALSE
                               ,ignore.case=!FALSE,verbose=FALSE)
{
   list1 <- envi_list(pattern=pattern,path=path,all.files=all.files
                     ,full.names=full.names,recursive=recursive
                     ,ignore.case=ignore.case)
   if (verbose)
      print(list1)
   for (a in list1)
   {
      toRemove <- FALSE
      for (ext in c("envi","envigz","bin","","bingz","img","dat","gz","bz2","xz"))
      {
         b <- sprintf("%s.%s",a,ext)
         if (verbose)
            print(data.frame(file=b,exists=file.exists(b)))
         if (!file.exists(b))
            next
         if ((ext=="bin")&&(file.exists(sprintf("%s.idr",a))))
            next
         file.remove(b)
         toRemove <- TRUE
      }
      if (!toRemove)
         next
      file.remove(sprintf("%s.hdr",a))
      aux.xml <- .dir(path=dirname(a)
                     ,pattern=paste0("^",basename(a),".*\\.aux\\.xml$")
                     ,full.names=TRUE)
      if (verbose)
         print(aux.xml)
      if (length(aux.xml))
         file.remove(aux.xml)
   }
   list1
}
'envi_rename' <- function(src,dst,overwrite=TRUE)
{
   srcdir <- dirname(src)
   srcname <- basename(src)
   if (missing(dst))
   {
      dstdir <- "."
      dstname <- srcname
   }
   else if ((file.exists(dst))&&(file.info(dst)$isdir))
   {
      dstdir <- dst
      dstname <- srcname
   }
   else
   {
      dstdir <- dirname(dst)
      dstname <- basename(dst)
   }
   srcname <- paste0("^",srcname)
   list1 <- .dir(path=srcdir
                    ,pattern=paste0(srcname,"(\\.(hdr|gz|bzip2|xz|envi|envigz|bin|bingz|img|dat))*$")
                    ,full.names=TRUE)
   list2 <- file.path(dstdir,.gsub(srcname,dstname,basename(list1)))
   file.rename(list1,list2)
}
'envi_list' <- function(pattern=".+",path=".",all.files=FALSE,full.names=recursive
                       ,recursive=FALSE,ignore.case=TRUE,exact=FALSE)
{
   '.noESRI' <- function(elist) {
      fpath <- ifelse(full.names | length(grep("/",elist)),elist,file.path(path,elist))
      a <- elist[sapply(fpath,function(f) readLines(paste0(f,".hdr"),1)=="ENVI")]
      elist[sapply(fpath,function(f) readLines(paste0(f,".hdr"),1)=="ENVI")]
   }
   if (!nchar(pattern))
      return(character())
   if (.lgrep("(/|\\\\)",pattern))
   {
      isDir <- file.info(pattern)$isdir
      if ((!is.na(isDir))&&(isDir)) {
         path <- pattern
         pattern <- ".+"
         full.names <- TRUE
      }
      else {
         if (file.exists(dirname(pattern))) {
            path <- dirname(pattern)
            pattern <- basename(pattern)
            recursive <- FALSE
            full.names <- TRUE
         }
      }
     # print(c(path=path,pattern=pattern))
   }
   if ((substr(pattern,1,1))=="+")
      pattern <- paste0("\\\\",pattern)
   patt1a <- patt1 <- .gsub("\\.hdr$","",pattern)
   patt1 <- .gsub("(\\.)$","",patt1)
   patt1a <- patt1
   patt2 <- .gsub("\\.(bin|envi|img|dat|gz|bz2|xz|bingz|envigz)$","",patt1)
   if (exact) {
      patt1 <- paste0("^",patt1,"$")
      patt2 <- paste0("^",patt2,"$")
   }
   list1 <- dir(path=path,pattern="\\.hdr$",all.files=all.files
               ,full.names=full.names,recursive=recursive
               ,ignore.case=ignore.case)
   list2 <- .gsub("\\.hdr$","",list1)
   if (!length(list2))
      return(character())
   ind <- try(.grep(patt1,basename(list2)),silent=TRUE)
   if (inherits(ind,"try-error"))
      ind <- integer()
   if (length(ind)) {
      return(.noESRI(list2[ind]))
   }
   ind <- try(.grep(patt2,basename(list2)))
   if (inherits(ind,"try-error"))
      ind <- integer()
   if (length(ind))
      return(.noESRI(list2[ind]))
   if (.lgrep("\\.(tif|tiff|png|bmp|shp|sqlite|geojson|json|gpkg|kml|mif)$",patt2)) {
      return(character()) ## if exist TIF and HDR, then HDR is not associated with TIF
   }
   patt2a <- .gsub("(\\..+)$","",patt2)
   ind <- try(.grep(patt2a,basename(list2))) ## truncate binary file extension
   if (inherits(ind,"try-error"))
      ind <- integer()
   if (length(ind)==1L) ## only exact matching!
      return(.noESRI(list2[ind]))
   ind <- .grep(patt2,basename(list2))
   if ((0)&&(file.exists(patt1a))&&((file.exists(paste0(patt1a,".hdr")))||
                               (file.exists(.gsub("^.+(\\..+)$",".hdr",patt1a)))))
      return(patt1a)
   if (!exact)
      return(character())
   list1 <- dir(path=getOption("ursaRequisite")
               ,pattern="\\.hdr$",all.files=all.files
               ,full.names=TRUE,recursive=FALSE
               ,ignore.case=ignore.case)
   list2 <- .gsub("\\.hdr$","",list1)
   ind <- .grep(patt1,basename(list2))
   if (length(ind))
      return(.noESRI(list2[ind]))
  # ind <- .grep(patt2,basename(list2))
  # if (length(ind))
  #    return(list2[ind])
   character()
}
