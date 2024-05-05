'open_envi' <- function(fname,resetGrid=FALSE,headerOnly=FALSE
                       ,decompress=!headerOnly,cache=0L,...)
{
   ##~ str(nodata);q()
   if (resetGrid)
      session_grid(NULL)
   obj <- .raster.skeleton()
   con <- .con.skeleton()
   grid <- .grid.skeleton()
   if (missing(fname))
      stop("filename is missing")
   fname <- gsub("\\.$","",fname)
   wname <- fname
   fname <- envi_list(wname,exact=TRUE)
   if (!length(fname))
      return(NULL)
   dname <- unique(dirname(fname))
   if (identical(wname,dname)) ## 20220130
      fname <- envi_list(file.path(dirname(wname),paste0("^",basename(wname),"$"))
                        ,exact=TRUE)
  # str(list(wname=wname,dname=dname,fname=fname))
   if ((length(fname)!=1)&&(dirname(wname)=="."))
      fname <- envi_list(path=getOption("ursaRequisite")
                        ,pattern=basename(wname),exact=TRUE,full.names=TRUE)
   if (length(fname)!=1)
   {
      arglist <- list(...)
      if (length(list(...))==0) {
         warning(paste("ENVI header",.sQuote(wname),"not found. Terminated."))
         return(NULL)
      }
      op <- options(warn=1)
      warning(paste("ENVI header",.sQuote(wname),"not found."
                   ,"Trying to interpret additional arguments for creating new."))
      on.exit(options(op))
      return(create_envi(wname,...)) ## 'return(grid)' before 2012-09-07
   }
   myname <- paste0(fname,".hdr")
   if (!file.exists(myname))
      myname <- .gsub("^(.+)(\\..+)$","\\1.hdr",fname)
   f1 <- readLines(myname,warn=FALSE)
   fields = c("samples","lines","bands","data type","header offset"
             ,"interleave","byte order","band names"
             ,"map info","projection info","data ignore value"
             ,"classes","class lookup","class names","coordinate system string")
   f3 <- paste(f1,collapse=" ")
   f3 <- .gsub("\\s+"," ",f3)
   f3 <- .gsub(",\\s",",",f3)
   f3 <- .gsub("\\s,",",",f3)
   f3 <- .gsub("{\\s","{",f3)
   f3 <- .gsub("\\s}","}",f3)
   f3 <- .gsub("\\s=","=",f3)
   f3 <- .gsub("=\\s","=",f3)
   mylen <- nchar(f3)
   map <- NULL
   cl <- list(n=0,val=NULL,name=NULL)
   wkt <- ""
   p <- character()
   for (name in tolower(sample(fields)))
   {
      ind1 <- regexpr(name,f3,ignore.case=TRUE,perl=TRUE,fixed=FALSE)
      if (ind1<0)
         next
      len <- attr(ind1,"match")
      f4 <- substr(f3,ind1+len,mylen)
      ind2 <- substr(f4,2,2)
      if (ind2=="{")
      {
         ind3 <- regexpr("(}|$)",f4,ignore.case=TRUE,perl=TRUE,fixed=FALSE)
         f5 <- substr(f4,3,ind3-1)
      }
      else
      {
         ind3 <- regexpr("(\\s|$)",f4,ignore.case=TRUE,perl=TRUE,fixed=FALSE)
         f5 <- substr(f4,2,ind3-1)
      }
      if (name=="samples")
         con$samples <- as.integer(f5)
      else if (name=="lines")
         con$lines <- as.integer(f5)
      else if (name=="bands")
         con$bands <- as.integer(f5)
      else if (name=="data type")
         con$datatype <- as.integer(f5)
      else if (name=="header offset")
         con$offset <- as.integer(f5)
      else if (name=="interleave")
         con$interleave <- f5
      else if (name=="byte order")
         con$byteorder <- as.integer(f5)
      else if (name=="band names")
      {
         obj$name <- unlist(strsplit(f5,","))
      }
      else if (name=="classes")
         cl$n <- as.integer(f5)
      else if (name=="class lookup")
         cl$val <- as.numeric(unlist(strsplit(f5,",")))
      else if (name=="class names")
      {
         f6 <- .grep(name,f1,value=TRUE)
         f6 <- .gsub("^.+\\{(.+)\\}","\\1",f5) ## gsub(,,f5) or gsub(,,f6) ?
         cl$name <- unlist(strsplit(f6,",",perl=TRUE))
      }
      else if (name=="coordinate system string")
         wkt <- f5
      else if (name=="map info")
      {
         map <- unlist(strsplit(f5,","))
         if (map[1]=="Geographic Lat/Lon")
            grid$crs <- c("4326",.crsWGS84())[-1]
         else if (map[1]=="UTM")
            grid$crs <- paste("+proj=utm",paste0("+zone=",map[8])
                             ,paste0("+datum="
                                    ,switch(map[10]
                                           ,'North America 1927'="NAD27"
                                           ,'WGS-84'="WGS84"
                                           ,"WGS84"))
                             ,"+units=m +no_defs")
         op <- options(warn=-1)
         map <- na.omit(as.numeric(map))
         options(op)
      }
      else if (name=="projection info")
         p <- strsplit(f5,",")[[1]]
      else if (name=="data ignore value")
         con$nodata <- as.numeric(f5)
   }
   if (is.na(con$byteorder))
      con$byteorder <- 0L
   if (with(cl,((n>0)&&(length(val) %in% c(0,n,3*n))&&(length(name)%in% c(0L,n)))))
   {
      if (length(cl$val)==3*cl$n)
         val <- rgb(matrix(cl$val,ncol=3,byrow=TRUE)/255)
      else if (!length(cl$val))
         val <- rep(NA_character_,cl$n)
      else
         val <- cl$val
      if ((FALSE)&&(all(nchar(val)==7)))
         val <- paste0(val,"FF")
      names(val) <- cl$name
      class(val) <- "ursaColorTable"
      obj$colortable <- val
      rm(val)
      class(obj$value) <- "ursaCategory"
   }
   else {
      class(obj$value) <- "ursaNumeric" ## by default
   }
   rm(cl)
   con$indexZ <- seq(con$bands)
   con$sizeof <- with(con,ifelse(datatype %in% c(1,11),1L
                           ,ifelse(datatype %in% c(2,12),2L
                           ,ifelse(datatype %in% c(3,13,4),4L
                           ,ifelse(datatype %in% c(5),8L
                           ,stop(datatype))))))
   con$signed <- with(con,if (datatype %in% c(1,12,13)) FALSE else TRUE)
   con$mode <- with(con,if (datatype %in% c(1,2,3,11,12,13)) "integer" else "double")
   con$endian <- if (ifelse(.Platform$endian=="big",1,0)==con$byteorder |
                    con$byteorder<0) .Platform$endian 
                 else "swap"
   con$swap <- as.integer((.Platform$endian=="big")&(!con$byteorder)|
                           (.Platform$endian=="little")&(con$byteorder))
   con$fname <- file.path(chartr("\\","/",normalizePath(dirname(fname)))
                          ,basename(fname))
   if (con$mode=="integer")
      con$nodata <- as.integer(con$nodata)
   if (((.crsForceProj4())||(!nchar(wkt)))&&(length(p)))
   {
      proj4 <- NULL
      pr <- NULL
      if (p[8]=="WGS-84")
         ellps <- "+ellps=WGS84"
      else if (p[8]=="Hughes")
         ellps <- ""
      if (p[1] %in% c("36","11"))
      {
         pr <- paste("+proj=laea",paste0("+lat_0=",p[4]),paste0("+lon_0=",p[5])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7])
                    ,paste0("+a=",p[2]),paste0("+b=",p[3]),"+units=m","+no_defs")
         if ((as.numeric(p[2])==6371228)&&(as.numeric(p[3])==6371228)&&
             (as.numeric(p[4])==90)&&(as.numeric(p[5])==0))
            proj4 <- "3408"
         else if ((as.numeric(p[2])==6378137)&&(as.numeric(p[3])==6356752.314245179)&&
             (as.numeric(p[4])==90)&&(as.numeric(p[5])==180))
            proj4 <- "3571"
         else if ((as.numeric(p[2])==6378137)&&(as.numeric(p[3])==6356752.314245179)&&
             (as.numeric(p[4])==90)&&(as.numeric(p[5])==-150))
            proj4 <- "3572"
         else if ((as.numeric(p[2])==6378137)&&(as.numeric(p[3])==6356752.314245179)&&
             (as.numeric(p[4])==90)&&(as.numeric(p[5])==90))
            proj4 <- "3576"
      }
      else if (p[1]=="31")
      {
         pr <- paste("+proj=stere",paste0("+lat_0=",90*sign(as.numeric(p[4])))
                    ,paste0("+lat_ts=",p[4]),paste0("+lon_0=",p[5]),"+k=1"
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7]))
         if (p[8]=="WGS-84")
         {
            if ((as.numeric(p[4])==70)&&(as.numeric(p[5])==-45))
               proj4 <- "3413"
            pr <- paste(pr,"+ellps=WGS84")
         }
         else
         {
            if ((as.numeric(p[2])==6378273)&&(as.numeric(p[3])==6356889.449)&&
                (as.numeric(p[4])==70)&&(as.numeric(p[5])==-45))
               proj4 <- "3411"
            pr <- paste(pr,paste0("+a=",p[2]),paste0("+b=",p[3]))
         }
         pr <- paste(pr,"+units=m","+no_defs")
      }
      else if (p[1]=="20")
      {
         pr <- "+proj=merc"
         proj4 <- "3388"
      }
      else if (p[1]=="4")
      {
        # proj4 <- "LCC Caspy"
         pr <- paste("+proj=lcc",paste0("+lat_1=",p[8]),paste0("+lat_2=",p[9])
                    ,paste0("+lat_0=",p[4]),paste0("+lon_0=",p[5])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7])
                    ,paste0("+a=",p[2]),paste0("+b=",p[3]),"+units=m","+no_defs")
      }
      else if (p[1]=="7")
      {
         pr <- paste("+proj=stere",paste0("+lat_0=",90*sign(as.numeric(p[4])))
                    ,paste0("+lon_0=",p[5]),paste0("+k=",p[8])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7]))
         if (p[10]=="WGS-84")
            pr <- paste(pr,"+ellps=WGS84")
         else
            pr <- paste(pr,paste0("+a=",p[2]),paste0("+b=",p[3]))
         pr <- paste(pr,"+units=m","+no_defs")
      }
      else if (p[1]=="3")
      {
        # proj4 <- "Transverse mercator"
         pr <- paste("+proj=tmerc",paste0("+lat_0=",p[4]),paste0("+lon_0=",p[5])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7])
                    ,paste0("+a=",p[2]),paste0("+b=",p[3]),"+units=m","+no_defs")
      }
      else if (p[1]=="9")
      {
         pr <- paste("+proj=aea",paste0("+lat_1=",p[8]),paste0("+lat_2=",p[9])
                    ,paste0("+lat_0=",p[4]),paste0("+lon_0=",p[5])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7])
                    ,paste0("+a=",p[2]),paste0("+b=",p[3]),"+units=m","+no_defs")
      }
      else if (p[1]=="99")
      {
         pr <- paste("+proj=cea",paste0("+lon_0=",p[4]),paste0("+lat_ts=",p[5])
                    ,paste0("+x_0=",p[6]),paste0("+y_0=",p[7])
                    ,paste0("+a=",p[2]),paste0("+b=",p[3]),"+units=m","+no_defs")
      }
      else
         proj4 <- "*unknown*"
      proj4 <- pr
      grid$crs <- proj4
   }
   grid$rows <- con$lines
   grid$columns <- con$samples
   if (!is.null(map))
   {
     # print(map)
      grid$minx <- map[3]-(map[1]-1)*map[5]
      grid$maxx <- grid$minx+grid$columns*map[5]
      grid$maxy <- map[4]+(map[2]-1)*map[6]
      grid$miny <- grid$maxy-grid$rows*map[6]
      grid$resx <- map[5]
      grid$resy <- map[6]
     # print(grid)
   }
   else
   {
      grid$minx <- 0
      grid$maxx <- grid$columns
      grid$miny <- 0
      grid$maxy <- grid$rows
      grid$resx <- 1
      grid$resy <- 1
   }
   grid2 <- getOption("ursaSessionGrid")
   if (!.is.grid(grid2))
   {
      if (nchar(grid$crs))
         session_grid(grid)
      con$indexC <- seq(grid$columns)
      con$indexR <- seq(grid$rows)
   }
   else if (TRUE)
   {
      x2 <- with(grid2,(seq(minx,maxx,by=resx)-0.5*resx))[-1]
      y2 <- with(grid2,(seq(miny,maxy,by=resy)-0.5*resy))[-1]
      x1 <- with(grid,(seq(minx,maxx,by=resx)-0.5*resx))[-1]
      y1 <- with(grid,(seq(miny,maxy,by=resy)-0.5*resy))[-1]
      tolx <- .Machine$double.eps*max(abs(c(grid$minx,grid$maxx)))*1e1
      toly <- .Machine$double.eps*max(abs(c(grid$miny,grid$maxy)))*1e1
      tolerance <- c(tolx,toly)
      dig <- -as.integer(.gsub(".+e\\-(\\d+)$","\\1"
                              ,format(signif(tolerance,1),sci=TRUE)))
      tolerance <- 10^dig
      if ((length(x1)==length(x2))&&(all(abs(x1-x2)<1e-12)))
         con$indexC <- seq(grid$columns)
      else {
        # con$indexC <- match(x2,x1) ## release before 2011-02-19
         for (i in (seq(6)-1)) {
            tol <- tolerance[1]*10^i
            if (TRUE) ## reliable but 5x slower
               a <- .is.near(x2,x1)
            else
               a <- match(round(x2/tol),round(x1/tol))
           # if (anyNA(a))
           #    next
            ind <- diff(which(!is.na(a)))
            if (!length(ind))
               next
            if (all(ind==1))
               break
         }
         if (i==5)
            con$indexC <- match(y2,y1)
         else
            con$indexC <- a
      }
      if ((length(y1)==length(y2))&&(all(abs(y1-y2)<1e-12)))
         con$indexR <- seq(grid$rows)
      else {
        # con$indexR <- rev(grid$rows+1-match(y2r,y1r)) ## release before 2011-02-19
         for (i in (seq(6)-1)) {
            tol <- tolerance[2]*10^i
            if (TRUE) ## reliable but 5x slower
               a <- .is.near(y2,y1)
            else
               a <- match(round(y2/tol),round(y1/tol))
            ind <- diff(which(!is.na(a)))
            if (!length(ind))
               next
            if (all(ind==1))
               break
         }
         if (i==5)
            con$indexR <- rev(grid$rows+1-match(y2,y1))
         else
            con$indexR <- rev(grid$rows+1-a)
      }
      grid <- grid2
   }
   else if (FALSE) ## introduced 2012-08-16
   {
      x1 <- with(grid,(seq(minx,maxx,by=resx)-0.5*resx))[-1]
      y1 <- with(grid,(seq(miny,maxy,by=resy)-0.5*resy))[-1]
      con$indexC <- with(grid2,which(x1>=minx & x1<=maxx))
      con$indexR <- with(grid2,rev(grid$rows+1-which(y1>=miny & y1<=maxy)))
      grid <- grid2
   }
   fname.envi <- paste(fname,".envi",sep="")
   fname.bin <- paste(fname,".bin",sep="")
   fname.img <- paste(fname,".img",sep="")
   fname.dat <- paste(fname,".dat",sep="")
   fname.gz  <- paste(fname,".gz",sep="")
   fname.bingz  <- paste(fname,".bingz",sep="")
   fname.envigz  <- paste(fname,".envigz",sep="")
   fname.xz <- paste(fname,".xz",sep="")
   fname.bz <- paste(fname,".bz2",sep="")
   fname.aux <- NA
   if (is.character(cache))
      cache <- 1L
   if (FALSE)
      NULL
   else if ((file.exists(fname))&&(!file.info(fname)$isdir))
   {
      con$connection <- "file"
      con$fname <- fname
      fname.aux <- paste0(fname,".aux.xml")
   }
   else if ((file.exists(fname.envi))&&(!file.info(fname.envi)$isdir))
   {
      con$connection <- "file"
      con$fname <- fname.envi
      fname.aux <- paste0(fname.envi,".aux.xml")
   }
   else if ((file.exists(fname.bin))&&(!file.info(fname.bin)$isdir))
   {
      con$connection <- "file"
      con$fname <- fname.bin
      fname.aux <- paste0(fname.bin,".aux.xml")
   }
   else if ((file.exists(fname.img))&&(!file.info(fname.img)$isdir))
   {
      con$connection <- "file"
      con$fname <- fname.img
      fname.aux <- paste0(fname.img,".aux.xml")
   }
   else if ((file.exists(fname.dat))&&(!file.info(fname.dat)$isdir))
   {
      con$connection <- "file"
      con$fname <- fname.dat
      fname.aux <- paste0(fname.dat,".aux.xml")
   }
   else if ((file.exists(fname.gz))&&(!file.info(fname.gz)$isdir))
   {
      verbose <- Sys.Date()<=as.Date("2024-04-20") & !.isPackageInUse()
      solved <- FALSE
      if (nchar(Sys.which("gzip"))) {
         if (cache) {
            if (verbose)
               message("trying cache")
            con$fname <- .ursaCacheRaster(fname.gz
                              ,ifelse(decompress,"gzip","gzip"),reset=cache!=1)
            solved <- !is.null(con$fname)
         }
         else if (decompress) {
            if (verbose)
               message("local unpack")
           # con$fname <- paste0(fname,".unpacked",.maketmp(),"~")
            fbase <- .maketmp()
            con$fname <- file.path(dirname(fbase)
                                  ,paste0(basename(fname)
                                         ,".unpacked",basename(fbase),"~"))
           # print(con$fname)
           # q()
            system2("gzip",c("-f -d -c",.dQuote(fname.bz)),stdout=con$fname,stderr=FALSE)
            solved <- !is.null(con$fname)
         }
         if (solved) {
            con$connection <- "file"
            con$compress <- ifelse(cache,0L,-1L)
         }
      }
      if ((!solved)&&(!decompress)) {
         if (verbose)
            message("internal ungzip")
         con$connection <- "gzfile"
         con$fname <- fname.gz
         solved <- TRUE
      }
      if (!solved)
         stop("Unable to open gzipped file")
      fname.aux <- paste0(fname,".aux.xml")
   }
   else if ((file.exists(fname.envigz))&&(!file.info(fname.envigz)$isdir))
   {
      verbose <- Sys.Date()<=as.Date("2023-05-29") & !.isPackageInUse()
      solved <- FALSE
      if (nchar(Sys.which("gzip"))) {
         if (cache) {
            if (verbose)
               message("trying cache")
            srcname <- con$fname
            con$fname <- .ursaCacheRaster(fname.envigz
                              ,ifelse(decompress,"gzip","gzip"),reset=cache!=1)
            solved <- !is.null(con$fname)
            if (F & solved)
               attr(con$fname,"aux") <- c(fname.envigz,"gzfile","read")
         }
         else if (decompress) {
            if (verbose)
               message("local unpack")
           # con$fname <- paste0(fname,".unpacked",.maketmp(),"~")
            fbase <- .maketmp()
            con$fname <- file.path(dirname(fbase)
                                  ,paste0(basename(fname)
                                         ,".unpacked",basename(fbase),"~"))
            attr(con$fname,"source") <- fname.envigz
            system2("gzip",c("-f -d -c",.dQuote(fname.envigz)),stdout=con$fname,stderr=FALSE)
            solved <- !is.null(con$fname)
         }
         if (solved) {
            con$connection <- "file"
            con$compress <- ifelse(cache,0L,-1L)
         }
      }
      if ((!solved)&&(!decompress)) {
         if (verbose)
            message("internal ungzip")
         con$connection <- "gzfile"
         con$fname <- fname.envigz
         solved <- TRUE
      }
      if (!solved)
         stop("Unable to open gzipped file")
      fname.aux <- paste0(fname.envi,".aux.xml")
   }
   else if ((file.exists(fname.bz))&&(!file.info(fname.bz)$isdir)) {
      verbose <- Sys.Date()<=as.Date("2020-04-20") & !.isPackageInUse()
      solved <- FALSE
      if (nchar(Sys.which("bzip2"))) {
         if (cache) {
            if (verbose)
               message("trying cache")
            con$fname <- .ursaCacheRaster(fname.bz
                              ,ifelse(decompress,"bzip2","bzip2"),reset=cache!=1)
            solved <- !is.null(con$fname)
         }
         else if (decompress) {
            if (verbose)
               message("local unpack")
           # con$fname <- paste0(fname,".unpacked",.maketmp(),"~")
            fbase <- .maketmp()
            con$fname <- file.path(dirname(fbase)
                                  ,paste0(basename(fname)
                                         ,".unpacked",basename(fbase),"~"))
           # print(con$fname)
           # q()
            system2("bzip2",c("-f -d -c",.dQuote(fname.bz)),stdout=con$fname,stderr=FALSE)
            solved <- !is.null(con$fname)
         }
         if (solved) {
            con$connection <- "file"
            con$compress <- ifelse(cache,0L,-1L)
         }
      }
      if ((!solved)&&(!decompress)) {
         if (verbose)
            message("internal un-bzip2")
         con$connection <- "bzfile"
         con$fname <- fname.bz
         solved <- TRUE
      }
      if (!solved)
         stop("Unable to open bzip2-ped file")
      fname.aux <- paste0(fname,".aux.xml")
   }
   else if ((file.exists(fname.bingz))&&(!file.info(fname.bingz)$isdir))
   {
      if (!decompress)
      {
         con$connection <- "gzfile"
         con$fname <- fname.bingz
      }
      else
      {
         con$connection <- "file"
        # con$fname <- paste0(fname,".unpacked~")
        # con$fname <- paste0(fname,".unpacked",basename(.maketmp()),"~")
         fbase <- .maketmp()
         con$fname <- file.path(dirname(fbase)
                               ,paste0(basename(fname)
                                      ,".unpacked",basename(fbase),"~"))
         if (FALSE) {
            system(paste("gzip -f -d -k -Sgz",fname.bingz))
            file.rename(fname.bin,con$fname)
         }
         else ## without "-k" key
            system2("gzip",c("-f -d -c",.dQuote(fname.bingz)),stdout=con$fname,stderr=FALSE)
         con$compress <- -1L
      }
      fname.aux <- paste0(fname.bin,".aux.xml")
   }
   else if ((file.exists(fname.xz))&&(!file.info(fname.xz)$isdir))
   {
      if (!decompress)
      {
         con$connection <- "xzfile"
         con$fname <- fname.xz
      }
      else
      {
         con$connection <- "file"
        # con$fname <- paste0(fname,".unpacked~")
        # con$fname <- paste0(fname,".unpacked",basename(.maketmp()),"~")
         fbase <- .maketmp()
         con$fname <- file.path(dirname(fbase)
                               ,paste0(basename(fname)
                                      ,".unpacked",basename(fbase),"~"))
         if (FALSE) ## should be obsolete
            shell(paste("xz -d -c",fname.xz,"1>",con$fname))
         else {
            system(paste("xz -f -d -k",fname.bz))
            file.rename(fname,con$fname)
         }
         con$compress <- -1L
      }
      fname.aux <- paste0(fname,".aux.xml")
   }
   else {
      dpath <- dirname(fname)
      a <- .dir(path=dpath
              ,pattern=paste0("^",basename(fname),"(\\..+)$")
              ,recursive=FALSE,full.names=FALSE)
      a <- .grep("\\.(hdr|png|tif)$",a,invert=TRUE,value=TRUE)
      if (length(a)==1) {
         con$connection <- "file"
         con$fname <- file.path(dpath,a)
      }
      else if (!headerOnly) {
         opW <- options(warn=0)
         warning("Unable to recognize ENVI binary file name. Data values are skipped")
         options(opW)
      }
   }
   metadata <- if ((!is.na(fname.aux)&&(file.exists(fname.aux))))
      readLines(fname.aux,warn=FALSE) else NULL
  # md <- xml2::as_list(xml2::read_xml(fname.aux))
   if (!is.na(con$connection)) {
      con$handle <- try(do.call(con$connection,list(con$fname,"r+b")),silent=TRUE)
      if (inherits(con$handle,"try-error")) ## read-only
         con$handle <- do.call(con$connection,list(con$fname,"rb"))
   }
   if (("bzfile" %in% class(con$handle))||("xzfile" %in% class(con$handle)))
      con$seek <- FALSE
   else
      con$seek <- TRUE
   obj$dim <- c(grid$rows*grid$columns,con$bands)
   if (!is.null(metadata)) {
      patt <- "<MDI key=\"Band_(\\d+)\">(.+)</MDI>"
      bname1 <- .grep(patt,metadata,value=TRUE)
      if (length(bname1)) {
         bname2 <- .gsub2(patt,"\\2",bname1)
         if (!all(bname2==bname1)) {
            bseq2 <- as.integer(.gsub2(patt,"\\1",bname1))
            obj$name[bseq2] <- bname2
         }
      }
   }
   if (is.na(obj$name[1])) {
      obj$name <- sprintf(sprintf("%s %%0%dd","Band"
                                 ,nchar(length(1:obj$dim[2]))),1:obj$dim[2])
     # obj$name <- character()
   }
   if (!is.null(metadata))
   {
      a <- .grep("NoDataValue",metadata,value=TRUE)
      if (length(a)) {
         con$nodata <- as.numeric(.gsub2("<NoDataValue.*>(.+)</NoDataValue>"
                                          ,"\\1",a[1]))
         if (is.infinite(con$nodata)) ## "-1.79769313486232Ee308"
            con$nodata <- sign(con$nodata)*1.7976931348623e+308
      }
   }
   res <- NULL
   if (!is.null(metadata)) {
      ind1 <- grep("<PAMRasterBand",metadata)
      ind2 <- grep("</PAMRasterBand",metadata)
      if ((length(ind1))&&(length(ind1)==length(ind2))) {
         res <- vector("list",length(ind1))
        # names(res) <- rep("___",length(ind1))
         names(res) <- as.character(seq(length(ind1)))
         patt1 <- "^.*<MDI key=\"(.+)\">(.+)</MDI>.*$"
         patt2 <- "^.*<Description>(.+)</Description>.*$"
         for (i in seq_along(ind1)) {
            md <- metadata[ind1[i]:ind2[i]]
            if (length(ind4 <- grep(patt2,md)))
               desc <- gsub(patt2,"\\1",md[ind4])
            else
               desc <- character()
            ord <- as.integer(gsub("^.*band=\"(\\d+)\".*$","\\1",md[1]))
            ind3 <- grep(patt1,md)
            if (length(ind3)) {
               name <- gsub(patt1,"\\1",md[ind3])
               value <- as.list(gsub(patt1,"\\2",md[ind3]))
               names(value) <- name
               if (length(desc))
                  names(res)[ord] <- desc
               res[[ord]] <- value
            }
         }
        # str(head(res,3))
        # q()
      }
   }
   res2 <- NULL
   ind1 <- grep("<PAMDataset",metadata)
   ind2 <- grep("</PAMDataset",metadata)
   if ((length(ind1)==1)&&(length(ind2)==1)) {
      patt1 <- "^.*<MDI key=\"(.+)\">(.+)</MDI>.*$"
      md <- metadata[seq(ind1+1L,ind2-1L)]
      ind3 <- grep(patt1,md)
      if (length(ind3)) {
         name <- gsub(patt1,"\\1",md[ind3])
         value <- as.list(gsub(patt1,"\\2",md[ind3]))
         names(value) <- name
         res2 <- list(value)
         names(res2) <- "Dataset metadata (unstructured)"
        # if (length(desc))
        #    names(res)[ord] <- desc
        # res[[ord]] <- value
      }
   }
   res <- c(res2,res)
   if (!is.null(res)) {
      attr(obj,"metadata") <- res
   }
   if (is.null(grid$crs))
      grid$crs <- ""
   if ((forceWKT <- TRUE)&&(!nchar(grid$crs))&&(nchar(wkt))) {
      grid$crs <- .ursaCRS(wkt)
      session_grid(grid)
   }
   if ((!nchar(grid$crs))&&(nchar(wkt)))
   {
      lverbose <- !FALSE
      if (lverbose)
         .elapsedTime("wkt -> proj4 start")
     # (!("package:rgdal" %in% search()))) { 
      isSF <- ("sf" %in% loadedNamespaces())&&(utils::packageVersion("sf")<"99990.9")
      isSP <- "sp" %in% loadedNamespaces()
      if ((FALSE)&&(nchar(Sys.which("gdalsrsinfo")))&&(!isSF)&&(!isSP)) {
         if (lverbose)
            message("'gdalsrsinfo' engine (read)")
         if (FALSE) ## slow
            grid$crs <- .gsub("\'","",system2("gdalsrsinfo"
                                               ,c("-o proj4",wkt),stdout=TRUE,stderr=FALSE))
         else {
            tmp <- .maketmp()
            wktin <- paste0(tmp,".prj~")
            writeLines(wkt,wktin)
            wktout <- paste0(tmp,".wkt~")
            system2("gdalsrsinfo",c("-o proj4",wktin),stdout=wktout,stderr=FALSE)
            grid$crs <- .gsub("\'","",readLines(wktout,warn=FALSE))
            file.remove(wktout)
            file.remove(wktin)
            grid$crs <- grid$crs[nchar(grid$crs)>0]
         }
      }
      else if ((!isSF)&&(isSP)) {
         if (lverbose)
            message("showP4() in 'rgdal'")
         .try(grid$crs <- .rgdal_showP4(wkt))
        # .try(grid$crs <- attr(GDALinfo(con$fname,returnStats=FALSE)
        #                   ,"projection")) ## GDALinfo() from 'rgdal'
      }
      else  {
         if (lverbose)
            message("sf::st_crs")
         opW <- options(warn=ifelse(.isPackageInUse(),-1,1))
         if (utils::packageVersion("sf")<"0.9")
            .try(grid$crs <- sf::st_crs(wkt=wkt)$proj4string)
         else
            .try(grid$crs <- sf::st_crs(wkt)$proj4string)
         options(opW)
        # res <- sf::st_crs(wkt)$proj4string
        # message(res)
         if ((is.na(grid$crs))||(!nchar(grid$crs))) {
            if (nchar(Sys.which("gdalsrsinfo"))) {
              ## FAILED for prj with 'wkt_esri' spec 
               if (lverbose)
                  message("      sf::st_crs --> gdalsrsinfo")
               tmp <- .maketmp()
               wktin <- paste0(tmp,".prj~")
               writeLines(wkt,wktin)
               wktout <- paste0(tmp,".wkt~")
               system2("gdalsrsinfo",c("-o proj4",wktin),stdout=wktout,stderr=FALSE)
               grid$crs <- .gsub("\'","",readLines(wktout,warn=FALSE))
               file.remove(wktout)
               file.remove(wktin)
               grid$crs <- grid$crs[nchar(grid$crs)>0]
            }
            else {
               message("      sf::st_crs -> showP4() in 'rgdal'")
               .try(grid$crs <- .rgdal_showP4(wkt))
            }
         }
        # stop("This is ureacheable branch! TODO for `sf`>0.8")
      }
      if (lverbose)
         .elapsedTime("wkt -> proj4 finish")
      session_grid(grid)
   }
   con$driver <- "ENVI"
   grid$crs <- .ursaCRS(grid$crs)
   obj$grid <- grid
   obj$con <- con
   arglist <- list(...)
   nodata <- .getPrm(arglist,name="^(nodata|ignore(value)*|bg)$"
                    ,class=c("integer","numeric"),default=NA,verbose=FALSE)
   if (!is.na(nodata))
      obj$con$nodata <- nodata
   if (!.lgrep("(layer|band)*name",names(arglist))) {
      return(obj)
   }
   ln1 <- obj$name
   ln2 <- arglist[[.grep("(layer|band)*name",names(arglist))]]
   if (identical(ln1,ln2))
      return(obj)
   close(obj) ## rebuild bands; e.g. after update to monthly sst
   if (length(unique(ln1))!=length(ln1)) {
      stop("Band names are not unique")
   }
   m12 <- c(na.omit(match(ln1,ln2)))
   m21 <- c(na.omit(match(ln2,ln1)))
   if (length(m12)!=length(m21)) { #  (any(is.na(m12))) {
     # close(obj)
      stop("Inconsistence of band names")
   }
   ftemp <- .maketmp()
   envi_rename(fname,ftemp)
   src <- open_envi(ftemp,cache=TRUE) ## RECURSUVE
   dst <- create_envi(wname,...)
   srcname <- names(src)
   dstname <- names(dst)
   cb <- chunk_band(src)
   from <- head(m21,1)
   to <- tail(m21,1)
   for (i in chunk_band(src)) {
      j <- i[i>=from & i<=to]
      if (!length(j))
         next
      k <- match(j,m21)
      if (anyNA(k)) {
         j <- j[which(!is.na(k))]
         k <- c(na.omit(k))
      }
      if (!length(j))
         next
      dst[k] <- src[j]
   }
   close(src)
   envi_remove(ftemp)
   rm(src)
   dst
}
