'.prepare.con' <- function(x,...)
{
   arglist <- list(...)
   if (missing(x))
      x <- NA
   if ((!missing(x))&&(!is.ursa(x)))
   {
      arglist$temporal <- x
      ind <- .grep("^temporal$",names(arglist))
      names(arglist)[ind] <- ""
      x <- .raster.skeleton()
      x$grid <- session_grid()
      if (.is.grid(arglist$temporal))
         x$grid <- arglist$temporal
   }
   isCT <- FALSE
   ind <- .grep("^(colortable|category)",names(arglist))
   if (length(ind))
   {
      ct <- arglist[[ind]]
      if (is.logical(ct)) {
         if (ct)
            ct <- ursa_colortable(x)
         else {
            ct <- character(0)
            ursa_colortable(x) <- ct
         }
      }
      if ((is.character(ct))&&(length(ct))) {
         isCT <- TRUE
         n <- length(ct)
         arglist$datatype <- ifelse(n<255,1L,ifelse(n<65535,12L,13L))
         arglist$nodata <- n
         class(x$value) <- "ursaCategory"
         x$colortable <- ct
         class(x$colortable) <- "ursaColorTable"
      }
   }
   simple <- attr(x,"copyright")
   simple <- ((is.character(simple))&&(nchar(simple)>0))
   if ((simple)) {
      if ((!FALSE)||("ursa" %in% loadedNamespaces())) {
         g1 <- ursa(x,"grid")
         ursa(x,"grid") <- regrid(g1,setbound=c(0,0,g1$columns,g1$rows),crs="")
      }
   }
   x$con <- .create.con(x,arglist)
   if (!.is.con(x$con)) {
      cat("Unable to create connection\n")
      return(NULL)
   }
   ind <- .grep("^(band|layer)*name",names(arglist))
   if (length(ind))
      x$name <- arglist[[ind[1]]]
   if ((0)&&(length(ind)))
      cat(sprintf("x$name=%s\n",x$name))
   if (any(is.na(x$dim)))
      x$dim <- as.integer(with(x$con,c(lines*samples,bands)))
   if ((x$dim[2]!=x$con$bands)&&((is.na(x$value))||(x$dim[2]!=dim(x$value)[2])))
      x$dim[2] <- x$con$bands
   if (length(.grep("(band|layer|names)",names(arglist))))
   {
      if (x$dim[2]!=x$con$bands)
         x$dim[2] <- x$con$bands
   }
   else
   {
      if (x$con$bands!=x$dim[2])
         x$con$bands <- x$dim[2]
   }
   if ((TRUE)&&(!is.na(x$name[1]))&&(!is.na(x$con$posZ))) ## FALSE before 29may2010
      NULL # x$name <- x$name #[x$con$posZ]
   else if ((is.na(x$name[1]))||(length(x$name)!=x$dim[2])) {
      x$name <- sprintf(sprintf("%s%%0%dd"
                               ,ifelse(is.na(x$name[1]),"Band ",x$name[1])
                               ,nchar(length(1:x$con$bands))),1:x$con$bands)
     # x$name <- character()
   }
   if (x$con$driver=="ENVI")
      .write.hdr(x)
   else if (x$con$driver=="GDAL") {
      rgdal::GDALcall(x$con$handle,"SetProject",x$grid$crs)
      rgdal::GDALcall(x$con$handle,"SetGeoTransform"
                                  ,with(x$grid,c(minx,resx,0,maxy,0,-resy)))
      nodata <- ursa_nodata(x)
      ct <- ursa_colortable(x)
      isCT <- (nband(x)==1)&&(length(ct)>0)
      hasColor <- (isCT)&&(all(!is.na(ct)))
      hasNames <- (isCT)&&(all(!is.na(names(ct))))
     # print(c(isCT=isCT,hasColor=hasColor,hasNames=hasNames))
      if (any(!is.na(nodata),isCT,hasColor,hasNames)) {
         for (i in seq(nband(x))) {
            bset <- methods::new("GDALRasterBand",x$con$handle,i)
            if (!is.na(nodata))
               rgdal::GDALcall(bset,"SetNoDataValue",nodata)
            if (hasColor)
               rgdal::GDALcall(bset,"SetRasterColorTable",as.character(ct))
            if (hasNames)
               rgdal::GDALcall(bset,"SetCategoryNames",names(ct))
           # if (isCT)
           #    rgdal::putRasterData(dset,as.array(colorize(obj[i]),flip=TRUE,drop=TRUE),band=i)
           # else
           #    rgdal::putRasterData(dset,as.array(obj[i],flip=TRUE,drop=TRUE),band=i)
         }
      }
   }
   x$value <- NA
  # class(x$value) <- ifelse(isCT,"ursaCategory","ursaNumeric")
   class(x$value) <- ifelse(.is.colortable(x),"ursaCategory","ursaNumeric")
   x
}
'.create.con' <- function(x,arglist)
{
   if (.is.con(x$con))
   {
      if (is.null(arglist$datatype))
      {
         if ((TRUE)&&(!is.null(dim(x$value))))
         {
            nodata <- if (is.null(arglist$nodata)) x$con$nodata else arglist$nodata
            if ((is.na(nodata))&&(anyNA(x$value)))
               arglist$nodata <- nodata <- .optimal.nodata(x$value)
            arglist$datatype <- .optimal.datatype(x$value,nodata)
            if (!is.na(nodata)) {
               if ((arglist$datatype==2)&&((nodata>32767)||(nodata<(-32768))))
                  arglist$nodata <- -32768L
               else if ((arglist$datatype==12)&&((nodata>65535)||(nodata<0)))
                  arglist$nodata <- 65535L
               else if ((arglist$datatype==1)&&((nodata>255)||(nodata<0)))
                  arglist$nodata <- 255L
               else if ((arglist$datatype==11)&&((nodata>127)||(nodata<(-128))))
                  arglist$nodata <- -128L
            }
         }
         else
            arglist$datatype <- x$con$datatype ## release: only these line
      }
      if (is.null(arglist$nodata))
         arglist$nodata <- x$con$nodata
      if (is.null(arglist$connection))
         arglist$connection <- "file"
   }
   else if (!is.null(dim(x$value)))
   {
      if (is.null(arglist$datatype))
         arglist$datatype <- .optimal.datatype(x$value,x$con$nodata)
      if (is.null(arglist$nodata)) {
         arglist$nodata <- .make.nodata(arglist$datatype)
      }
   }
   obj <- .raster.skeleton()
   if (is.null(x))
   {
      obj$grid <- session_grid()
     # obj$con <- envi=.con.skeleton()
      obj$con <- .con.skeleton()
      obj$con$driver <- arglist$implement ## added 20170124
     # obj$con$driver <- driver ## removed 20170124
   }
   else
   {
      class(x) <- "ursaRaster (no generic)"
      for (i in seq(along=x))
      {
         myname <- names(x)[i]
         if (myname!="value")
            obj[[myname]] <- x[[myname]]
      }
   }
   con <- .make.con(obj,arglist)
   con
}
'.make.con' <- function(obj,arglist)
{
   if (!is.ursa(obj))
      return(NULL)
   if (missing(arglist))
      arglist <- list()
   grid <- obj$grid
   if (!.is.con(obj$con))
      con <- .con.skeleton()
   else
      con <- obj$con
   ignore <- NULL #obj$nodata
   fname <- NULL
   datatype <- NULL
   byteorder <- NULL
   bands <- NULL
   bandnames <- NULL
   connectionName <- c("file","bzfile","gzfile","xzfile")
   connection <- NULL
   interleaveName <- c("bsq","bil","bip")
   interleave <- NULL
   implementName <- c("ENVI","GDAL")
   implement <- NULL
   driver <- NULL
   proj <- NULL
   compressed <- NULL
   wkt <- NULL
   ext <- NULL
   for (i in seq(along=arglist))
   {
      var <- arglist[[i]]
      if (is.null(var))
         next
      name <- names(arglist[i])
      if ((is.null(fname))&&(is.character(var))&&
          ((!nchar(name))||(length(.grep("fname",name)))))
      {
         fname <- var
         next
      }
      if ((is.null(connection))&&(is.character(var))&&
          ((!length(name))||(length(.grep("con",name)))))
      {
         if (length(.grep("^gz",var)))
            connection <- "gzfile"
         else if (length(.grep("^bz",var)))
            connection <- "bzfile"
         else if (length(.grep("^xz",var)))
            connection <- "xzfile"
         else if (length(.grep("^file",var)))
            connection <- "file"
         if (!is.null(connection))
            next
         
      }
      if ((is.null(interleave))&&(is.character(var))&&
          ((!length(name))||(length(.grep("interleave",name)))))
      {
         ind <- .grep(var,interleaveName)
         if (length(ind))
         {
            interleave <- interleaveName[ind]
            next
         }
      }
      if ((is.null(implement))&&(is.character(var))&&
          ((!length(name))||(length(.grep("implement",name)))))
      {
         ind <- .grep(var,implementName)
         if (length(ind))
         {
            implement <- implementName[ind]
            next
         }
      }
      if ((is.null(datatype))&&
              ((!length(name))||(length(.grep("datatype",name)))))
      {
         if (is.numeric(var))
         {
            if (as.integer(var) %in% c(1,2,3,4,5,12,13,11))
            {
               datatype <- var
               next
            }
         }
         else if (is.character(var))
         {
            datatype <- switch(var,byte=1L,integer=2L,real=4L,float=4L
                                  ,Byte=1L,UInt8=1L,Int8=11
                                  ,Int16=2L,UInt16=12,UInt32=13,Int32=3
                                  ,Float32=4L,Float64=5L
                              ,NULL)
            next
         }
      }
      if ((is.null(ignore))&&(!is.ursa(var))&&((is.numeric(var))||(anyNA(var)))&&
          (length(name))&&(length(.grep("(^bg$|nodata|ignore)",name))))
      {
         ignore <- var
         next
      }
      if ((is.null(byteorder))&&(is.numeric(var))&&
          ((length(name))&&(length(.grep("byteorder",name)))))
      {
         byteorder <- as.integer(var)
         if (byteorder)
            byteorder <- 1L
         next
      }
     # if ((is.null(bands))&&(is.numeric(var))&&((length(name))
     #     &&((length(.grep(name,"bands")))||(length(.grep(name,"nband")))||
     #        (length(.grep(name,"layers")))||(length(.grep(name,"nlayer"))))))
      if ((is.null(bands))&&(is.numeric(var))&&(length(name))&&
          (length(.grep("(layers|bands|nlayer|nband|length)",name))))
      {
         bands <- as.integer(var)
         next
      }
     # if ((is.null(bandnames))&&(is.character(var))&&((length(name))
     #     &&((length(.grep(name,"bandname")))||
     #        (length(.grep(name,"layername")))||
     #        (length(.grep(name,"name"))))))
      if ((is.null(bandnames))&&(is.character(var))&&(length(name))&&
          (length(.grep("(bandname|layername|names)",name))))
      {
         bandnames <- var
         next
      }
      if ((is.null(driver))&&(is.character(var))&&(length(name))&&
          (length(.grep("driver",name))))
      {
         driver <- var
         next
      }
      if ((is.null(compressed))&&((is.integer(var) || is.logical(var)))&&
          ((length(name))&&(length(.grep("compress",name)))))
      {
         compressed <- as.logical(var)
         next
      }
      if ((is.null(wkt))&&((is.integer(var) || is.logical(var)))&&
          ((length(name))&&(length(.grep("wkt",name)))))
      {
         wkt <- as.logical(var)
         next
      }
      if ((is.null(ext))&&(is.character(var))&&
          ((length(name))&&(length(.grep("ext",name)))))
      {
         ext <- var
         next
      }
   }
   if ((is.null(bands))&&(length(bandnames)))
      bands <- length(bandnames)
   if (is.null(fname))
   {
      fname <- .maketmp()
      message(paste("Filename is assigned automatically:",fname))
   }
   myDir <- .dirname(fname)
   if (!file.exists(myDir)) {
      dir.create(myDir,recursive=TRUE)
   }
   con$fname <- file.path(chartr("\\","/",normalizePath(.dirname(fname)))
                          ,.basename(fname))
   if (!is.null(connection))
      con$connection <- connection
   else
      connection <- "file"
   if (is.na(con$connection))
      con$connection <- connection
   if (!is.null(datatype))
      con$datatype <- datatype
   else
      datatype <- 4L
   if (is.na(con$datatype))
      con$datatype <- datatype
   if (!is.null(ignore))
      con$nodata <- ignore
   else
      ignore <- .make.nodata(datatype)
   if (is.na(con$nodata))
      con$nodata <- ignore
   if (!is.null(interleave))
      con$interleave <- interleave
   else
      interleave <- interleaveName[1]
   if (is.na(con$interleave))
      con$interleave <- interleave
   if (!is.null(implement))
      con$driver <- implement
   else
      implement <- implementName[1]
   if (is.na(con$driver))
      con$driver <- implement
   if (!is.null(byteorder))
      con$byteorder <- byteorder
   else
      byteorder <- 0L
   if (is.na(con$byteorder))
      con$byteorder <- byteorder
   if ((con$driver=="ENVI")&&(TRUE)) ## forced to compress
   {
      if (!(is.null(compressed))) {
         if (!is.na(compressed))
            con$compress <- as.integer(compressed)
         else
            con$compress <- 1L
      }
      else
         con$compress <- 1L
   }
   else
   {
      if (!is.null(compressed))
         con$compress <- as.integer(compressed)
      else
         compressed <- 0L
      if (is.na(con$compress))
         con$compress <- compressed
      con$compress <- abs(con$compress)
   }
   con$compress <- abs(con$compress)
   if (!is.null(wkt))
      con$wkt <- as.logical(wkt)
   else
      wkt <- FALSE
   if (is.na(con$wkt))
      con$wkt <- wkt
   if (is.null(ext))
      ext <- switch(con$driver,ENVI=".envi","")
   else
   {
      ext <- if (nchar(ext)) paste0(".",ext) else ""
      con$compress=0L
   }
   if (is.na(con$wkt))
      con$wkt <- wkt
   if (!is.null(bands))
      con$bands <- bands
   else if (length(obj$value))
      bands <- obj$dim[2]
   else
      bands <- 1L
   if (is.na(con$bands))
      con$bands <- bands
   con$mode <- if (con$datatype %in% c(1L,2L,3L,11L,12L,13L)) "integer" 
               else "numeric"
   con$sizeof <- if (con$datatype %in% c(1L,11L)) 1L 
               else if (con$datatype %in% c(2L,12L)) 2L
               else if (con$datatype %in% c(3L,13L,4L)) 4L else 8L
   con$signed <- if (con$datatype %in% c(1L,12L,13L)) FALSE else TRUE
   con$endian <- "little"
   con$swap <- as.integer((.Platform$endian=="big")&(!con$byteorder)|
                           (.Platform$endian=="little")&(con$byteorder))
  # endian != .Platform$endian
   if (FALSE)
      NULL 
   else if (length(.grep("\\.gz$",con$fname)))
      con$connection <- "gzfile"
   else if (length(.grep("\\.bz2$",con$fname)))
      con$connection <- "bzfile"
   else if (length(.grep("\\.xz$",con$fname)))
      con$connection <- "xzfile"
   else if (length(.grep("\\.(envi|bin|img|dat)$",con$fname)))
   {
      con$connection <- "file"
      con$compress <- 0L
   }
   ##~ else if (length(.grep("\\.bin$",con$fname)))
   ##~ {
      ##~ con$connection <- "file"
      ##~ con$compress <- 0L
   ##~ }
   ##~ else if (length(.grep("\\.envi$",con$fname)))
   ##~ {
      ##~ con$connection <- "file"
      ##~ con$compress <- 0L
   ##~ }
   ##~ else if (length(.grep("\\.img$",con$fname)))
   ##~ {
      ##~ con$connection <- "file"
      ##~ con$compress <- 0L
   ##~ }
   else if (length(.grep("\\.$",con$fname)))
   {
      con$fname <- substr(con$fname,1,nchar(con$fname)-1L)
      con$connection <- "file"
      con$compress <- 0L
   }
   else
      con$fname <- sprintf("%s%s",con$fname,switch(con$connection
                       ,file=ext,gzfile=".gz",bzfile=".bz2",xzfile=".xz",""))
   if (!(con$connection %in% c("file")))
      con$compress <- 0L
   con$lines <- grid$rows
   con$samples <- grid$columns
  # con$offset <- as.integer(5*4+8*8+sum(nchar(obj$name))+length(obj$name)+
  #                          nchar(obj$grid$crs)+1+
  #                          4*obj$dim[2]+con$sizeof)
   if ((length(con$offset)>1)||(is.na(con$offset)))
      con$offset <- 0L
   if (is.na(con$bands))
   {
      if (!is.null(bandnames))
         con$bands <- length(bandnames)
      else
         con$bands <- 1L
   }
   if ((!is.na(obj$dim[2]))&&(is.null(bands)))
      con$bands <- obj$dim[2]
   con$interleave <- with(con,switch(interleave,spatial="bsq",temporal="bil"
                           ,interleave))
   if (con$driver=="ENVI") {
      con$handle <- do.call(con$connection,list(con$fname,"w+b"))
      cl <- class(con$handle)
      if (("bzfile" %in% cl)||("xzfile" %in% cl)||("gzfile" %in% cl)) ## gz?
         con$seek <- FALSE
      else
         con$seek <- TRUE
      rm(cl)
   }
   else if (con$driver=="GDAL") {
      if (.lgrep("\\.tif(f)*$",fname))
         driver <- "GTiff"
      else if (.lgrep("\\.img$",fname))
         driver <- "HFA" # https://gdal.org/frmt_hfa.html
      else if (.lgrep("\\.png$",fname))
         driver <- "PNG"
      else if (.lgrep("\\.jp(e)*g$",fname))
         driver <- "JPEG"
      else if (.lgrep("\\.bmp$",fname))
         driver <- "BMP"
     # else if (.lgrep("\\.sdat$",fname))
     #    driver <- "SAGA"
      if (is.null(driver))
         driver <- "ENVI"
      dtName <- switch(as.character(datatype)
                      ,'1'="Byte",'2'="Int16",'4'="Float32"
                      ,'11'="Int8",'12'="UInt16",'13'="UInt32",'3'="Int32"
                      ,'5'="Float64",stop("cannot recognize datatype"))
      nb <- if (is.na(con$posZ[1])) con$bands else length(con$posZ)
      try(con$handle <- methods::new("GDALTransientDataset"
                                ,methods::new("GDALDriver",driver)
                                ,con$lines,con$samples,nb,dtName))
      if (!inherits(con$handle,"GDALTransientDataset"))
         return(NA)
      con$seek <- FALSE
   }
   if ("file" %in% class(con$handle))
   {
      if (file.exists(oldpack <- paste0(con$fname,".gz")))
         file.remove(oldpack)
      if (file.exists(oldpack <- paste0(con$fname,".bz2")))
         file.remove(oldpack)
      if (file.exists(oldpack <- paste0(con$fname,".xz")))
         file.remove(oldpack)
   }
   if (file.exists(aux <- paste0(con$fname,".aux.xml")))
      file.remove(aux)
   if (con$driver=="ENVI") {
      if (((con$interleave %in% c("bil","bsq"))&&(con$seek))||
         (con$connection %in% c("gzfile"))) #&&(!is.matrix(obj$value)))
      {
         nb <- ifelse(is.na(con$posZ[1]),con$bands,length(con$posZ))
         seek(con,origin="start"
             ,where=with(con,(lines*samples*nb-1)*sizeof+offset),rw="w")
         val <- 0L
         storage.mode(val) <- con$mode
         with(con,writeBin(val,size=sizeof,endian=endian,handle))
      }
   }
   con$indexC <- NA
   con$indexR <- NA
   if ((!is.na(con$posZ[1]))&&(!is.na(con$indexZ[1])))
      con$posZ <- con$indexZ[con$posZ]
   con$indexZ <- NA
   storage.mode(con$nodata) <- con$mode
   storage.mode(con$datatype) <- "integer"
  # con$driver <- "ENVI"
   con$swap <- as.integer((.Platform$endian=="big")&(!con$byteorder)|
                           (.Platform$endian=="little")&(con$byteorder))
   con
}
'.optimal.datatype' <- function(x,nodata=NULL)
{
   if (is.ursa(x))
      x <- x$value
   if (TRUE)
   {
      isInt <- is.integer(x)
      n <- as.integer(prod(dim(x)))+as.integer(!is.null(nodata))
      if (isInt)
         res <- .Cursa("optimalDatatypeInt",x=c(as.integer(x),as.integer(nodata))
                  ,n=n,res=integer(1),NAOK=TRUE)$res
      else
      {
         res <- .Cursa("optimalDatatypeDouble",x=c(as.numeric(x),as.numeric(nodata))
                  ,n=n,res=integer(1),NAOK=TRUE)$res
      }
     # print(summary(x));q()
      return(res)
   }
  # isInteger <- is.integer(x)
   dimx <- dim(x)
   lend <- length(dimx)
   if (!(length(dimx) %in% c(2,3)))
      return(NULL)
  # if (!isInteger)
  #    datatype <- 4
   bands <- if (lend==3) dimx[3] else if (lend==2) dimx[1]
   if (is.na(bands))
      return(NULL)
   datatypeout <- rep(NA_integer_,bands)
   tmp <- if (lend==3) array(NA,dim=dimx[-lend]) else array(NA,dim=dimx[2])
   for (i in seq(bands))
   {
      tmp[] <- if (lend==3) x[,,i] else if (lend==2) x[i,]
      tmp[is.na(tmp)] <- 0
      if ((all(tmp %in% 0:255))||(all(tmp %in% -128:127)))
         datatypeout[i] <- 1L
      else if (all(tmp %in% -32768:32767))
         datatypeout[i] <- 2L
      else if (all(tmp %in% 0:65536))
         datatypeout[i] <- 12L
      else
         datatypeout[i] <- 4L
   }
   rm(tmp)
   datatype <- unique(datatypeout)
   if (length(datatype)>1)
   {
      if (4L %in% datatype)
         datatype <- 4L
      else if ((1L %in% datatype)&&(2L %in% datatype)&&(!(12L %in% datatype)))
         datatype <- 2L
      else if ((1L %in% datatype)&&(12L %in% datatype)&&(!(2L %in% datatype)))
         datatype <- 12L
      else
         datatype <- 4L
   }
   datatype
}
'.write.hdr' <- function(x,clear=TRUE)
{
   if (!is.ursa(x))
      return(NULL)
   grid <- x$grid
   con <- x$con
   if (!.is.grid(grid))
      return(NULL)
   if (!.is.con(con))
      return(NULL)
   if (is.na(con$interleave))
      con$interleave <- "bsq"
   if (is.na(con$samples))
      con$samples <- grid$columns
   if (is.na(con$lines))
      con$lines <- grid$rows
   if (is.na(con$bands))
      con$bands <- 1L
   if (is.na(con$byteorder))
      con$byteorder <- 0L
   if (is.na(con$datatype))
      con$datatype <- 1L
   fname <- .gsub("\\.(bin|bingz|envi|envigz|img|dat|gz|bz2|xz|unpacked(.*)~)$","",con$fname)
   if (is.na(fname))
   {
      fname <- x$name[1]
      con$fname <- fname
   }
   myname <- paste(fname,".hdr",sep="")
   if (clear)
   {
      list1 <- .dir(path=dirname(fname)
         ,pattern=sprintf("^%s($|\\.(envi|envigz|bin|bingz|dat|img|gz|bz2|xz|.*aux\\.xml)$)"
                                       ,basename(fname)),full.names=TRUE)
      if (length(ind <- .grep(sprintf("^%s$",basename(con$fname)),basename(list1))))
         list1 <- list1[-ind]
      file.remove(list1[which(!file.info(list1)$isdir)])
   }
   Fout <- file(myname,"wt")
   writeLines(sprintf("%s","ENVI"),Fout)
   writeLines(sprintf("description = {%s}",con$fname),Fout)
   writeLines(sprintf("samples = %d",con$samples),Fout)
   writeLines(sprintf("lines   = %d",con$lines),Fout)
   if (is.na(con$posZ[1]))
      writeLines(sprintf("bands   = %d",con$bands),Fout)
   else
      writeLines(sprintf("bands   = %d",length(con$posZ)),Fout)
   writeLines(sprintf("header offset = %d",0),Fout)
   writeLines(sprintf("file type = %s","ENVI Standard"),Fout)
   writeLines(sprintf("data type = %d",con$datatype),Fout)
   writeLines(sprintf("interleave = %s",con$interleave),Fout)
   if (FALSE)
      writeLines(sprintf("sensor type = %s","Unknown"),Fout)
   writeLines(sprintf("byte order = %d",con$byteorder),Fout)
   if ((length(unique(con$nodata))==1)&&(!is.na(con$nodata)))
      writeLines(sprintf("data ignore value = %s",con$nodata[1]),Fout)
   projection_name <- "ursa package"
   projection_ellipse <- "unknown"
   projection_units <- "units=meters"
   projection_info <- ""
   proj4 <- grid$crs[1]
   p <- NULL
   if (nchar(proj4))
   {
      pr <- unlist(strsplit(proj4,"\\s+"))
      projection_ellipse <- "unknown"
      projection_units <- "units=meters"
      projection_name <- "R"
      semi <- NULL
      if (FALSE)
         semi <- c("6378137","6356752.3") ## low-precision
      if (.lgrep("\\+datum=WGS84",pr))
         semi <- c("6378137","6356752.314245179")
      else if (.lgrep("\\+ellps=WGS84",pr))
         semi <- c("6378137","6356752.314245179")
      else if (.lgrep("\\+ellps=krass",pr))
         semi <- c("6378245","6356863.018773047")
      else
         semi <- c(.gsub2("\\s\\+a=(\\S+)\\s","\\1",proj4)
                  ,.gsub2("\\s\\+b=(\\S+)\\s","\\1",proj4))
      if (.lgrep("\\+proj=laea",pr))
      {
         p <- c(p,"11",semi)
         p <- c(p,.gsub2("\\s\\+lat_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
         projection_name <- "Lambert Azimuthal Equal Area"
      }
      else if (.lgrep("\\+proj=stere",pr))
      {
         if (.lgrep("\\+lat_ts",proj4)) {
            p <- c(p,"31",semi)
            p <- c(p,.gsub2("\\s\\+lat_ts=(\\S+)\\s","\\1",proj4))
            p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)(\\s|$)","\\1",proj4))
            if(.lgrep("\\s\\+x_0=(\\S+)(\\s|$)",proj4))
               p <- c(p,.gsub2("\\s\\+x_0=(\\S+)(\\s|$)","\\1",proj4))
            else
               p <- c(p,"0")
            if(.lgrep("\\s\\+y_0=(\\S+)(\\s|$)",proj4))
               p <- c(p,.gsub2("\\s\\+y_0=(\\S+)(\\s|$)","\\1",proj4))
            else
               p <- c(p,"0")
            projection_name <- "Polar Stereographic"
         }
         else {
            p <- c(p,"7",semi)
            p <- c(p,.gsub2("\\s\\+lat_0=(\\S+)(\\s|$)","\\1",proj4))
            p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)(\\s|$)","\\1",proj4))
            if(.lgrep("\\s\\+x_0=(\\S+)(\\s|$)",proj4))
               p <- c(p,.gsub2("\\s\\+x_0=(\\S+)(\\s|$)","\\1",proj4))
            else
               p <- c(p,"0")
            if(.lgrep("\\s\\+y_0=(\\S+)(\\s|$)",proj4))
               p <- c(p,.gsub2("\\s\\+y_0=(\\S+)(\\s|$)","\\1",proj4))
            else
               p <- c(p,"0")
            projection_name <- "Stereographic (ellipsoid)"
         }
      }
      else if (.lgrep("\\+proj=tmerc",pr))
      {
         p <- c(p,"3",semi)
         p <- c(p,.gsub2("\\s\\+lat_ts=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
         projection_name <- "Transverse Mercator"
      }
      else if (.lgrep("\\+proj=lcc",pr))
      {
         p <- c(p,"4",semi)
         p <- c(p,.gsub2("\\s\\+lat_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lat_1=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lat_2=(\\S+)\\s","\\1",proj4))
         projection_name <- "Lambert Conformal Conic"
      }
      else if (.lgrep("\\+proj=aea",pr))
      {
         p <- c(p,"9",semi)
         p <- c(p,.gsub2("\\s\\+lat_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lat_1=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lat_2=(\\S+)\\s","\\1",proj4))
         projection_name <- "Albers Conical Equal Area"
      }
      else if (.lgrep("\\+proj=zzzcea",pr))
      {
         p <- c(p,"99",semi)
         p <- c(p,.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+lat_ts=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
         p <- c(p,.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
         projection_name <- "Equal Area Cylindrical"
      }
      
      if ((FALSE)||(!nchar(projection_name)))
      {
         print(pr)
         print(projection_info)
         stop("NEW PROJECTION")
      }
      if (!is.null(p))
      {
         p <- c(p,projection_name,projection_units)
         projection_info <- paste0("{",paste(p,collapse=", "),"}")
      }
   }
   tempx <- with(grid,minx+0*0.5*resx)
   tempy <- with(grid,grid$maxy-0*0.5*resy)
   map_info <- c(projection_name,1,1,tempx,tempy,grid$resx,grid$resy
                    ,projection_ellipse,projection_units)
   map_info <- paste0("{",paste(map_info,collapse=", "),"}")
   writeLines(sprintf("map info = %s",map_info),Fout)
   if ((TRUE)&&(nchar(Sys.which("gdalsrsinfo")))) ## forced 'gdalsrsinfo'/showWKT 
      projection_info <- "" 
   if (projection_info!="")
      writeLines(sprintf("projection info = %s",projection_info),Fout)
   wkt <- NULL
   if ((TRUE)&&(!is.null(p))) ## help QGIS to recognize EPSG
   {
      .epsg <- NA
      if ((p[1]=="31")&&
          (as.numeric(p[2])==6378137)&&(as.numeric(p[3])==6356752.314245179)&&
          (as.numeric(p[4])==70)&&(as.numeric(p[5])==-45)) {
         .epsg <- "3408"
         wkt <- paste0("PROJCS[\"WGS_84_NSIDC_Sea_Ice_Polar_Stereographic_North\""
                    ,",GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\""
                    ,",SPHEROID[\"WGS_1984\",6378137,298.257223563]]"
                    ,",PRIMEM[\"Greenwich\",0]"
                    ,",UNIT[\"Degree\",0.017453292519943295]]"
                    ,",PROJECTION[\"Stereographic_North_Pole\"]"
                    ,",PARAMETER[\"standard_parallel_1\",70]"
                    ,",PARAMETER[\"central_meridian\",-45]"
                    ,",PARAMETER[\"false_easting\",0]"
                    ,",PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]}")
      }
      else if ((p[1]=="36")&&
               (as.numeric(p[2])==6378137)&&
               (as.numeric(p[3])==6356752.314245179)&&
               (as.numeric(p[4])==90))
      {
         if (as.numeric(p[5])==180)
         {
            .epsg <- "3571"
            wkt <- paste0("PROJCS[\"WGS_84_North_Pole_LAEA_Bering_Sea\""
                     ,",GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\""
                     ,",SPHEROID[\"WGS_1984\",6378137,298.257223563]]"
                     ,",PRIMEM[\"Greenwich\",0]"
                     ,",UNIT[\"Degree\",0.017453292519943295]]"
                     ,",PROJECTION[\"Lambert_Azimuthal_Equal_Area\"]"
                     ,",PARAMETER[\"latitude_of_origin\",90]"
                     ,",PARAMETER[\"central_meridian\",180]"
                     ,",PARAMETER[\"false_easting\",0]"
                     ,",PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]")
         }
         else if (as.numeric(p[5])==-150) {
            .epsg <- "3572"
            wkt <- paste0("PROJCS[\"WGS_84_North_Pole_LAEA_Alaska\""
                     ,",GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\""
                     ,",SPHEROID[\"WGS_1984\",6378137,298.257223563]]"
                     ,",PRIMEM[\"Greenwich\",0]"
                     ,",UNIT[\"Degree\",0.017453292519943295]]"
                     ,",PROJECTION[\"Lambert_Azimuthal_Equal_Area\"]"
                     ,",PARAMETER[\"latitude_of_origin\",90]"
                     ,",PARAMETER[\"central_meridian\",-150]"
                     ,",PARAMETER[\"false_easting\",0]"
                     ,",PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]")
         }
         else if (as.numeric(p[5])==90) {
            .epsg <- "3576"
            wkt <- paste0("PROJCS[\"WGS_84_North_Pole_LAEA_Russia\""
                     ,",GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\""
                     ,",SPHEROID[\"WGS_1984\",6378137,298.257223563]]"
                     ,",PRIMEM[\"Greenwich\",0]"
                     ,",UNIT[\"Degree\",0.017453292519943295]]"
                     ,",PROJECTION[\"Lambert_Azimuthal_Equal_Area\"]"
                     ,",PARAMETER[\"latitude_of_origin\",90]"
                     ,",PARAMETER[\"central_meridian\",90]"
                     ,",PARAMETER[\"false_easting\",0]"
                     ,",PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]")
         }
      }
     # print(.epsg)
      rm(.epsg)
   }
   if ((is.null(wkt))&&(projection_info=="")&&(nchar(proj4)))
   {
      lverbose <- FALSE
      if (lverbose)
         .elapsedTime("proj4 -> wkt start")
     # loadNamespace("sf") ## for development
     # (!("package:rgdal" %in% search()))) {
      if ((nchar(Sys.which("gdalsrsinfo")))&&
          (!(any(c("rgdal","sf") %in% loadedNamespaces())))) {
         if (lverbose)
            message("'gdalsrsinfo' engine (write)")
         if (FALSE) 
            wkt <- system2("gdalsrsinfo",c("-o wkt",.dQuote(proj4))
                   ,stdout=TRUE)
         else {
            wktout <- .maketmp(ext=".wkt~")
           # shell(paste("gdalsrsinfo -o wkt",paste0("\"",proj4,"\""),"1>",wktout))
           # 20170319 dQuote() returns non-symmetrical quotes in interactive() 
            system2("gdalsrsinfo",c("-o wkt_esri",.dQuote(proj4))
                   ,stdout=wktout,stderr=FALSE)
            wkt <- readLines(wktout,warn=FALSE)
            file.remove(wktout)
         }
      }
      else if (!("sf" %in% loadedNamespaces())) {
         if (lverbose)
            message("'rgdal' engine")
         if (!.try(wkt <- rgdal::showWKT(proj4,morphToESRI=TRUE)))
            wkt <- NULL
      }
      else { ## 'sf' in namespace; 'OGC_WKT' ONLY. 
         if (lverbose)
            message("'sf' engine")
         if (!.try(wkt <- {
            if (utils::packageVersion("sf")<"0.9")
               ret <- sf::st_as_text(sf::st_crs(proj4),EWKT=TRUE)
            else
               ret <- sf::st_crs(proj4)$Wkt
            ret
         }))
        # if (!.try(wkt <- sf::st_as_text(sf::st_crs(proj4),EWKT=TRUE)))
        #    wkt <- NULL
        # print(proj4)
        # message(wkt)
         if (!TRUE) { ## 20191216 patch for EXTENSION["PROJ4","+proj=......."]
            wkt <- gsub(",EXTENSION\\[\"PROJ4\".+\\]","]",wkt)
         }
      }
      if (lverbose)
         .elapsedTime("proj4 -> wkt finish")
   }
   if (!is.null(wkt)) {
      if (length(wkt))
         wkt <- paste(gsub("(^\\s+|\\s+$)","",wkt),collapse="")
      writeLines(sprintf("coordinate system string = {%s}",wkt),Fout)
   }
   if ((is.character(x$name))&&(sum(nchar(x$name))>0))
   {
      if (.lgrep(",",x$name)) {
         metaname <- paste0(.gsub("\\.(gz|bz2|xz)$","",x$con$fname),".aux.xml")
         Fmeta <- file(metaname,"wt")
         writeLines("<PAMDataset>",Fmeta)
         writeLines("  <Metadata>",Fmeta)
         for (i in seq_along(x$name))
            writeLines(paste0("    <MDI key=",.dQuote(paste0("Band_",i))
                             ,">",x$name[i],"</MDI>"),Fmeta)
         writeLines("  </Metadata>",Fmeta)
         writeLines("</PAMDataset>",Fmeta)
         close(Fmeta)
      }
      else {
         if (all(!is.na(x$con$posZ)))
            bandnames <- paste(x$name[x$con$posZ],sep="",collapse=",\n\t")
         else
            bandnames <- paste(x$name,sep="",collapse=",\n\t")
         writeLines(sprintf("band names = {\n\t%s}",bandnames),Fout)
      }
   }
   if (length(x$colortable))
   {
      ct <- x$colortable
      writeLines(sprintf("classes = %d",length(ct)),Fout)
      isCol <- TRUE
      if (is.numeric(ct))
         val <- as.numeric(ct)
      else {
         val <- as.character(ct)
         if (all(is.na(val)))
            isCol <- FALSE
         else
            val <- c(col2rgb(as.character(val)))
      }
      if (isCol) {
         val <- paste(val,sep="",collapse=",")
         writeLines(sprintf("class lookup = {%s}",val),Fout)
      }
      if (!is.null(names(ct))) {
         if (TRUE) ## if '\t' not recoginizing
         {
            val <- .gsub(",",";",names(x$colortable))
            val <- paste(val,sep="",collapse=",")
            writeLines(sprintf("class names = {%s}",val),Fout)
         }
         else
         {
            val <- paste(names(x$colortable),sep="",collapse=",\n\t")
            writeLines(sprintf("class names = {\n\t%s}",val),Fout)
         }
      }
   }
   close(Fout)
   invisible(chartr("\\","/",normalizePath(myname)))
}
