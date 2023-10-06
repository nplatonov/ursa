'.open_vapour' <- function(fname,engine="vapour",verbose=FALSE) {
   if (!is.character(fname))
      return(NULL)
   engine <- match.arg(engine)
   requireNamespace("vapour",quietly=.isPackageInUse())
   opW <- options(warn=0-!verbose,show.error.messages=TRUE) ## to prevent 'GeoTransform values not available'
   on.exit(options(opW))
   a <- vapour::vapour_raster_info(fname)
   g1 <- regrid()
   g1$rows <- a$dimension[2]
   g1$columns <- a$dimension[1]
   ##~ nl <- as.integer(a1[3])
   g1$minx <- a$extent[1]
   g1$maxx <- a$extent[2]
   g1$miny <- a$extent[3]
   g1$maxy <- a$extent[4]
  # g1$resx <- a$geotransform[2]
  # g1$resy <- -a$geotransform[4]
   g1$resx <- with(g1,(maxx-minx)/columns)
   g1$resy <- with(g1,(maxy-miny)/rows)
   g1$crs <- a$projstring
  # comment(g1$crs) <- a$projection
  # if (is.na(g1$crs))
  #    g1$crs <- ""
  # str(g1)
  # session_grid(g1)
   res <- .raster.skeleton()
  # res$dim <- as.integer(c(prod(a$dimension),a$bands))
   res$dim <- c(prod(a$dimension),a$bands) |> as.integer()
   con <- .con.skeleton()
   con$driver <- "VAPOUR"
   con$samples <- g1$columns
   con$lines <- g1$rows
   con$bands <- a$bands
   con$indexC <- seq(g1$columns)
   con$indexR <- seq(g1$rows)
   con$indexZ <- seq_len(a$bands)
   con$seek <- FALSE
   con$fname <- fname
   con$handle <- fname ## methods::new("GDALReadOnlyDataset",fname)
   con$datatype <- switch(a$datatype,byte=1L,integer=2L,real=4L,float=4L
                                    ,Byte=1L,UInt8=1L,Int8=11
                                    ,Int16=2L,UInt16=12,UInt32=13,Int32=3
                                    ,Float32=4L,Float64=5L
                                    ,NA_integer_)
   res$con <- con
   if (!is.null(a$nodata_value))
      ignorevalue(res) <- ifelse(con$datatype %in% c(1L,11L,2L,12L,3L,13L)
                                ,as.integer(a$nodata_value),a$nodata_value)
   vrt <- vapour::vapour_vrt(fname)
   vrt <- strsplit(vrt,split="\n")[[1]]
   if (length(ind <- grep("CategoryName",vrt))) {
      patt <- "\\s*<Category>(.+)</Category>$"
      ca <- grep(patt,vrt[seq(ind[1],ind[2])],value=TRUE)
      ca <- gsub(patt,"\\1",ca)
   }
   else
      ca <- NULL
   if (length(ind <- grep("ColorTable",vrt))) {
      patt <- "\\s*<Entry c1=\"(\\d+)\" c2=\"(\\d+)\" c3=\"(\\d+)\" c4=\"(\\d+)\"\\s*/>"
      ct <- vrt[seq(ind[1]+1L,ind[2]-1L)]
      ct <- data.frame(c1=as.integer(gsub(patt,"\\1",ct))
                ,c2=as.integer(gsub(patt,"\\2",ct))
                ,c3=as.integer(gsub(patt,"\\3",ct))
                ,c4=as.integer(gsub(patt,"\\4",ct))
                )
      ct <- apply(ct,1,function(x) {
         rgb(x[1],x[2],x[3],x[4],maxColorValue=255)
      })
   }
   else
      ct <- NULL
   if (!is.null(ct)) {
      if (!is.null(ca)) {
         ct <- ct[seq(length(ca))]
      }
      names(ct) <- ca
   }
   else if (!is.null(ca)) {
      ct <- rep(NA,length(ca))
      names(ct) <- ca
   }
   else
      ct <- character()
   class(ct) <- "ursaColorTable"
   patt <- "^\\s*<MDI key=\"Band_(\\d+)\">(.+)</MDI>$"
   b <- vrt[grep(patt,vrt)]
   if (!length(b))
      bname <- paste("Band",seq_along(con$bands))
   else
      bname <- gsub(patt,"\\2",b)[as.integer(gsub(patt,"\\1",b))]
   ursa_grid(res) <- g1
   names(res) <- bname
   ursa_colortable(res) <- ct
  # if (!is.null(a$nodata_value))
  #    ignorevalue(res) <- a$nodata_value
   class(res$value) <- ifelse(length(ct),"ursaCategory","ursaNumeric")
   res
}
'.read_vapour' <- function(fname,resetGrid=TRUE,band=NULL
                       ,engine=c("vapour"),verbose=FALSE,...) { ## ,...
   if (engine=="vapour") {
     # .elapsedTime("vapour -- step1")
      if (F) {
         ri <- vapour::vapour_raster_info(fname)
         str(ri)
        # .elapsedTime("vapour -- step5")
         q()
      }
      a <- .open_vapour(fname,engine=engine,verbose=verbose)
     # .elapsedTime("vapour -- step2")
      if (inDetail <- TRUE) {
        # .elapsedTime("vapour -- step3")
         b <- vapour::gdal_raster_data(fname,bands=seq(a))
        # print(is.raw(b[[1]]))
        # .elapsedTime("vapour -- step4")
        # if (ri$datatype %in% c("Byte","Int32","UInt32","Int64"))
         if (a$con$datatype %in% c(1L,2L,3L,11L,12L,13L))
            ursa_value(a) <- as.integer(do.call(cbind,b))
         else
            ursa_value(a) <- do.call(cbind,b)
        # .elapsedTime("vapour -- step5")
      }
      else
         ursa_value(a) <- vapour::gdal_raster_data(fname,bands=seq(a)) |> do.call(cbind,args=_)
      if (resetGrid)
         session_grid(a)
      return(a)
   }
   NULL
}
