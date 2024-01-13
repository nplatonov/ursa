'ursa_new' <- function(...)
{
   arglist <- list(...)
   value <- .getPrm(arglist,name="(^$|^value)"
                   ,class=list("numeric","matrix","array","ursaRaster")
                   ,coerce=FALSE,default=NA,verbose=FALSE)
   isUrsa <- inherits(value,"ursaRaster")
   if (isUrsa) {
      result <- value
      value <- as.array(value)
   }
   else
      result <- .raster.skeleton()
   nd <- length(dim(value))
   isMatrix <- if (nd==2) TRUE else FALSE
   isArray <- if (nd==3) TRUE else FALSE
   bands <- .getPrm(arglist,name="(len(gth)*|nband|nlayer|bands|layers)"
                   ,default=1L)
   nodata <- .getPrm(arglist,name="(bg|background|nodata|ignore(value)*)"
                    ,default=NA_real_)
   bname <- .getPrm(arglist,name="(band|layer)*name(s*)",class="character"
                   ,default=NULL)
   if (length(bname))
      bands <- length(bname)
   datatype <- .getPrm(arglist,name="datatype",default=NA_integer_)
   colorTable <- .getPrm(arglist,name="(colortable|category)"
                        ,class="ursaColorTable",default=NULL)
   permute <- .getPrm(arglist,name="perm(ute)*",default=FALSE) ## -- 20170720 TRUE
   flip <- .getPrm(arglist,name="flip",default=FALSE) ## -- 20170720 TRUE
   crs <- .getPrm(arglist,name="ref",class=list("ursaGrid","character")
                 ,default=NULL)
   verbose <- .getPrm(arglist,name="verbose",default=FALSE)
   if (verbose) {
      str(list(value=value,isUrsa=isUrsa,isArray=isArray,isMarix=isMatrix
              ,nband=bands,bandname=bname,datatype=datatype
              ,colorTable=colorTable,permute=permute,flip=flip))
   }
   if (isUrsa)
      grid <- ursa_grid(result)
   else
      grid <- getOption("ursaSessionGrid") ## grid <- session_grid()
   if ((is.null(grid))||(!.is.grid(grid)))
   {
      if (.is.grid(crs))
         session_grid(crs)
      else if (is.character(crs))
         session_grid(ursa_grid(crs))
   }
   grid <- session_grid()
   sp <- with(grid,columns*rows)
   if (is.array(value)) ## # if ((is.null(grid))&&(is.array(value)))
   {
      dimb <- dima <- dim(value)
      if (length(dima)==2) {
         if ((TRUE)&&(dima[1]!=with(grid,columns*rows))) {## added 20160201
            value <- value[,rev(seq(dima[2])),drop=FALSE]
         }
         if (permute) {
            value <- t(value)
            dima <- rev(dima)
         }
         dima <- c(dima,1L)
        # dim(value) <- dima ## added 20170129
      }
      else if (length(dima)==3) {
         if (permute) {
            ind <- c(2,1,3)
            value <- aperm(value,ind)
            dima <- dim(value)
         }
      }
      if (!((sp==dima[1])||(sp==dima[2]))) {
         if ((grid$columns!=dima[1])||(grid$rows!=dima[2]))
         {
            grid <- .grid.skeleton()
            grid$columns <- dima[1]
            grid$rows <- dima[2]
            grid$resx <- 1
            grid$resy <- 1
            grid$minx <- 0
            grid$maxx <- as.numeric(dima[1])
            grid$miny <- 0
            grid$maxy <- as.numeric(dima[2])
            grid$crs <- ""
            session_grid(grid)
         }
        # str(grid)
      }
      if (flip) {
        # dimb <- dim(value)
         if (length(dimb)==2) {
            value <- value[,rev(seq(dima[2])),drop=FALSE]
         }
         else if (length(dimb)==3)
            value <- value[,rev(seq(dima[2])),,drop=FALSE]
      }
   }
   if (!isUrsa)
      result$grid <- grid
   if ((is.matrix(value))&&((sp==dim(value)[1]))) {
      result$value <- value
      dimnames(result$value) <- NULL
      result$dim <- dim(result$value)
      bands <- result$dim[2]
      if ((is.null(bname))&&(length(colnames(value))==bands))
         bname <- colnames(value)
   }
   else if ((is.matrix(value))&&((sp==dim(value)[2]))) {
      result$value <- t(value)
      dimnames(result$value) <- NULL
      result$dim <- dim(result$value)
      bands <- result$dim[2]
      if ((is.null(bname))&&(length(rownames(value))==bands))
         bname <- rownames(value)
   }
   else if (is.array(value))
   {
      if (length(dim(value))==2) {
         dim(value) <- c(prod(dim(value)),1L)
      }
      else {
         value <- value[,rev(seq(dima[2])),,drop=FALSE] ## added 20160330
         dim(value) <- c(prod(dim(value)[1:2]),dim(value)[3])
      }
      result$value <- value
      result$dim <- dim(value) #with(result$grid,c(columns*rows,dim(value)[2]))
      bands <- result$dim[2]
   }
   else if ((length(value)==1)&&(is.logical(value))&&(!is.na(value))&&(!value))
   {
      result$value <- as.numeric(NA)
      result$dim <- with(result$grid,c(columns*rows,bands))
   }
   else
   {
      if (is.null(dim(value))) {
         nb <- length(value)/with(result$grid,columns*rows)
         if ((bands==1)&&((length(value)>1)&&(!.is.integer(nb)))) { ## 12 months
            opW <- options(warn=1)
            warning("How many bands do you mean? Please specify argument 'bands='.")
            opW <- options(opW)
            if (length(value)<12)
               bands <- length(value)
         }
         if (.is.integer(nb))
            bands <- as.integer(round(nb))
      }
      ##~ if (is.null(dim(value))) {
         ##~ if ((bands==1)&&(length(value)<=12)) ## 12 months
            ##~ bands <- length(value)
      ##~ }
      dim1 <- with(result$grid,c(rows,columns,bands))
      ##~ dim1[3] <- bands
     # dim3 <- as.integer(c(prod(dim1[1:2]),dim1[3]))
      dim3 <- c(dim1[1]*dim1[2],dim1[3])
      if ((is.null(dim(value)))&&(length(value)==bands)) {
         if ((bands==1)&&(length(value)==1)) {
            result$value <- array(value,dim=dim3)
         }
         else
            result$value <- array(rep(value,each=dim3[1]),dim=dim3)
      }
      else {
         result$value <- array(value,dim=dim3)
      }
      result$dim <- dim3
   }
   if (isUrsa)
      return(result)
  # result$name <- sprintf(sprintf("%s%%0%dd","tmp"
  #                          ,nchar(length(1:x$con$bands))),1:x$con$bands)
   result$con <- .con.skeleton()
   if (!is.na(nodata))
      result$con$nodata <- nodata
   if (is.numeric(datatype))
      result$con$datatype <- as.integer(datatype)
   else if (is.character(datatype)) {
      result$con$datatype <- switch(var,byte=1L,integer=2L,real=4L,float=4L
                                       ,Byte=1L,UInt8=1L,Int8=11
                                       ,Int16=2L,UInt16=12,UInt32=13,Int32=3
                                       ,Float32=4L,Float64=5L
                                       ,NA_integer_)
   }
   ##~ if ((is.null(bname))||(length(bname)!=result$dim[2]))
   if (is.null(bname)) {
      result$name <- sprintf(sprintf("%s%%0%dd"
                                    ,"Band ",nchar(bands)),seq_len(bands))
   }
   else {
      bname <- rep(bname,length=bands)
      result$name <- bname
   }
   if (!is.null(colorTable))
   {
      result$colortable <- colorTable
      class(result$colortable) <- "ursaColorTable"
      result$con$nodata <- length(colorTable)
      class(result$value) <- "ursaCategory"
   }
   else {
      ursa_colortable(result) <- character()
      class(result$value) <- "ursaNumeric" ## not quick
     # attr(result$value,"class") <- c(attr(result$value,"class"),"ursaNumeric")
     # attr(result$value,"class") <- "ursaNumeric" ## not quick too
     # print(class(result$value))
   }
   result
}
