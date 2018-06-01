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
      if (inherits(con$handle,"connection"))
      {
         close(con$handle)
         con$handle <- NA
         if (con$compress==-1L)
            file.remove(con$fname)
         else if (con$compress==-2L)
         {
            fname <- .gsub("\\.unpacked(.*)~$",".envi",con$fname)
            file.rename(con$fname,fname)
            if (file.exists(ftmp <- paste0(fname,".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.bin","",fname),".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.envi","",fname),".gz")))
               file.remove(ftmp)
            system(paste("gzip","-f -Sgz",fname))
         }
         else if (con$compress==1L)
         {
            if (file.exists(ftmp <- paste0(con$fname,".gz")))
               file.remove(ftmp)
            if (nchar(Sys.which("gzip")))
               system(paste("gzip","-f -Sgz",con$fname)) ##keep
           # src <- paste0(con$fname,"gz")
           # dst <- file.path(dirname(src),.gsub("\\.bin",".gz",basename(con$fname)))
           # file.rename(src,dst)
         }
      }
      else if (inherits(con$handle,"GDALTransientDataset")) {
         dr <- rgdal::getDriverName(rgdal::getDriver(con$handle))
         op <- NULL
         if (dr=="GTiff")
            op=c("COMPRESS=DEFLATE","PREDICTOR=2","TILED=NO"
                ,paste0("INTERLEAVE=",switch(con$interleave,bil="PIXEL","BAND")))
         else if (dr=="HFA") {
            op=c("COMPRESSED=YES")
         }
         else if (dr=="ENVI") {
           # print(con$interleave)
            op <- paste0("INTERLEAVE=",toupper(con$interleave))
         }
         rgdal::saveDataset(con$handle,con$fname,options=op)
        # rgdal::closeDataset(con$handle)
         rgdal::GDAL.close(con$handle)
         con$handle <- NA
         bname <- args[[i]]$name
        # if (FALSE) {
         standardname <- paste("Band",seq_along(bname))
         if ((TRUE)&&(!is.na(bname[1]))&&(!identical(standardname,bname))) {
            metafile <- paste0(con$fname,".aux.xml")
            if (!is.na(con$posZ[1]))
               bname <- bname[con$posZ]
            added3 <- rep("",length(bname))
               for (i in seq_along(bname))
                  added3[i] <- paste0("    <MDI key=",.dQuote(paste0("Band_",i))
                                    ,">",bname[i],"</MDI>")
            added2 <- c("  <Metadata>",added3,"  </Metadata>")
            added1 <- c("<PAMDataset>",added2,"</PAMDataset>")
            if (!file.exists(metafile)) {
               Fmeta <- file(metafile,"wt")
               writeLines(added1,Fmeta)
               close(Fmeta)
            }
            else {
               meta <- readLines(metafile)
              # i1 <- .grep("<Metadata>",meta)
               ##~ i2 <- .grep("</Metadata>",meta)
               ##~ i2 <- i2[i2>i1][1]
               i3 <- .grep("</PAMDataset>",meta)
               metaBefore <- meta[1:(i3-1)]
               metaAfter <- meta[i3:length(meta)]
               writeLines(c(metaBefore,added2,metaAfter),metafile)
              # op <- options(warn=0)
              # warning("Band names was not written. TODO insert lines to *.aux.xml")
              # options(op)
            }
         }
      }
      else if (inherits(con$handle,"GDALReadOnlyDataset")) {
        # print(class(con$handle))
         rgdal::GDAL.close(con$handle)
        # rgdal::closeDataset(con$handle)
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
