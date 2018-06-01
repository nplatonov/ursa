'panel_plot' <- function(obj,...)
{
   if (.skipPlot(TRUE))
      return(NULL)
   geoType <- ""
   isSP <- FALSE
   isSF <- FALSE
   arglist <- as.list(match.call())
   isLang <- is.language(arglist[["obj"]])
   if (isLang)
      oname <- as.character(arglist[["obj"]])
   else
      oname <- "*undetermed*"
  # str(lapply(arglist,class))
   arglist2 <- list(...) ## remove dupe of 'add=TRUE'
   ct <- NULL
   if (inherits(obj,c("raster"))) {
      ret <- with(session_grid()
                 ,rasterImage(as.raster(obj),minx,miny,maxx,maxy,...))
   }
   else if (is.character(obj)) {
      if (.lgrep("\\.(shp(\\.zip)*|(geojson|sqlite|gpkg)(\\.(gz|bz2))*)$",obj)) {
         if (FALSE) { ## 20171216 deprecated
            op <- options(warn=0)
            requireNamespace("rgdal",quietly=.isPackageInUse())
            a <- .shp.read(obj)
           # a <- spTransform(a,session_grid()$proj4)
           # ret <- .panel_plot(a,add=TRUE,...)
            ret <- sp::plot(a,add=TRUE,...)
            options(op)
         }
         else {
            ret <- panel_plot(spatialize(obj),...) ## RECURSIVE
         }
      }
      else if ((length(obj)==1)&&(envi_exists(obj))) {
         ret <- panel_raster(read_envi(obj),...)
      }
      else if ((length(obj)==1)&&(file.exists(obj))) {
         ret <- panel_raster(read_gdal(obj),...)
      }
      else if ((getOption("ursaPngScale")==1)&&
             (.lgrep("\\+proj=merc",session_proj4())>0)&&
             ((TRUE)||(.lgrep(obj,.tileService())))) {
         ret <- panel_raster(obj,...)
      }
      else
         return(invisible(NULL))
   }
   else if (is.ursa(obj)) {
      ct <- panel_raster(obj,...)
   }
   else if (is_spatial(obj)) {
      oprj <- spatial_proj4(obj)
      sprj <- session_proj4()
      if (nchar(sprj)>2) {
         oprj2 <- .gsub("\\+wktext\\s","",oprj)
         sprj2 <- .gsub("\\+wktext\\s","",sprj)
         oprj2 <- .gsub("(^\\s|\\s$)","",oprj2)
         sprj2 <- .gsub("(^\\s|\\s$)","",sprj2)
         if (!identical(oprj2,sprj2)) {
            obj <- spatial_transform(obj,sprj)
         }
      }
      geoType <- spatial_geotype(obj)
      onlyGeometry <- is.null(spatial_data(obj))
      if ((FALSE)&&(onlyGeometry)) {
        # ret <- plot(obj,add=TRUE,...)
         ret <- .tryE(do.call(".panel_plot",c(obj,add=TRUE)))
      }
      else if (TRUE) { ## is advanced condition required here?
        # ret <- plot(sf::st_geometry(obj),...)
        # opE <- options(show.error.messages=TRUE)
        # ret <- .tryE(.panel_plot(sf::st_geometry(obj),add=TRUE,...))
        # ret <- .tryE(.panel_plot(obj,add=TRUE,...))
        # message("===========")
        # str(arglist2)
        # message("===========")
         if ((TRUE)&&(.lgrep("^col",names(arglist2)))) {
           # stop("I")
            if (is.numeric(col)) {
               ct <- colorize(col)
               col <- ct$colortable[ct$index]
            }
            else if ((is.character(arglist2$col))&&(length(spatial_fields(obj)))) {
               if (!anyNA(match(arglist2$col,spatial_fields(obj)))) {
                 # stop("A")
                  ct <- colorize(spatial_data(obj,subset=col,drop=TRUE))
                  col <- ct$colortable[ct$index]
               }
               else if (length(spatial_fields(obj))==-1) {
                 # stop("B")
                  if (.is.colortable(arglist2$col)) {
                    # stop("B1")
                     val <- spatial_data(obj,drop=TRUE)
                     ind <- match(names(arglist2$col),val)
                     if ((!anyNA(ind))&&(all(diff(ind)==1))) {
                       # stop("B1a")
                        col <- unname(unclass(arglist2$col))
                        ct <- arglist2$col
                       # ct <- colorize(val,pal=arglist2$col)
                     }
                     else {
                       # stop("B1b")
                        if (length(val)==length(arglist2$col)) {
                           ct <- arglist2$col
                           col <- unname(unclass(arglist2$col))
                        }
                        else {
                           ct <- colorize(val,colortable=arglist2$col)
                           col <- ct$colortable[ct$index]
                        }
                     }
                  }
                  else {
                    # stop("B2")
                     col <- arglist2$col
                  }
               }
               else if (is.character(col)) {
                 # stop("C")
                  col <- arglist2$col
               }
               else {
                 # stop("D")
                  col <- NULL
               }
            }
            else
               col <- arglist2$col
            if (!is.null(col))
               arglist2$col <- unclass(col)
         }
         else if (length(spatial_fields(obj))==1) {
           # stop("II")
           # ct <- colorize(spatial_data(obj)[[1]])
            ct <- do.call("colorize",c(list(spatial_data(obj)[[1]]),arglist2))
            arglist2$col <- ct$colortable[ct$index]
         }
         else
            arglist2$col <- ifelse(is_spatial_lines(obj),"black","transparent")
         if (.lgrep("point",geoType)) {
            if (!.lgrep("pch",names(arglist2))) {
               arglist2$pch <- 21
            }
            if (!.lgrep("bg",names(arglist2))) {
               arglist2$bg <- arglist2$col
               if ((.lgrep("pch",names(arglist2)))&&
                   (arglist2$pch %in% c(21,22,23,24,25))) ## ?pch
                  arglist2$col <- "black"
                 # arglist2$bg <- "transparent"
            }
            if (!.lgrep("cex",names(arglist2))) {
               arglist2$cex <- 1.2
            }
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 0.5
            }
         }
         else if (.lgrep("lines",geoType)) {
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 1 ## 20180309 was 'lwd <- 3'
            }
            if (length(ind <- .grep("^border",names(arglist2)))) {
               arglist2$col <- arglist2[[ind]]
            }
         }
         if (.lgrep("polygon",geoType)) {
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 0.1
            }
           # str(arglist2)
           # str(ct)
           # q()
         }
        # str(c(obj=list(spatial_geometry(obj)),add=TRUE,arglist2))
         ret <- .tryE(do.call(".panel_plot"
                     ,c(obj=list(spatial_geometry(obj)),add=TRUE,arglist2)))
         arglist2$type <- spatial_geotype(obj)
        # options(opE)
      }
      isSF <- .isSF(obj)
      isSP <- .isSP(obj)
   }
   else {
      ret <- try(.panel_plot(obj,add=TRUE,...))
      if (inherits(ret,"try-error")) {
         opW <- options(warn=1)
         warning(paste("Unable to call 'plot' method for class"
                      ,.sQuote(class(obj))
                      ,"\nIt seems that package 'methods' is required."))
         options(opW)
         ret <- NULL
      }
   }
   aname <- names(arglist2)
  # str(arglist2)
   if (is_spatial_points(obj)) {
      ret <- list(name=oname
                 ,pt.col=arglist2$col
                 ,pch=arglist2$pch
                 ,pt.cex=arglist2$cex
                 ,pt.lwd=arglist2$lwd
                 ,pt.bg=arglist2$bg
                 ,border="transparent"
                 )
   }
   else {
      ret <- list(name=oname,type="default"
                 ,col="transparent",border="transparent",lty=1,lwd=1,pch=0,cex=1
                 ,fill="transparent",bg="transparent",density=NULL,angle=45)
      rname <- names(ret)
      if (.lgrep("polygon",geoType)) { # 20171215 -- 'if (geoType %in% c("POLYGON","MULTIPOLYGON"))'
         ret$pch <- 22
         ret$cex <- 3
      }
      for (i in seq_along(rname)) {
         if (is.na(j <- match(rname[i],aname)))
            next
         ret[[i]] <- arglist2[[j]]
      }
      if ((TRUE)&&(.lgrep("line",geoType))) {
         if (!is.list(ret$col)) {
            .col <- ret$col
            ret$col <- ret$border
            ret$border <- .col
         }
      }
      if ((TRUE)&&(.lgrep("point",geoType))&&(!is.null(ct))) {
         ret$col <- ct$colortable
      }
      if ((ret$bg!="transpareny")&&(ret$border=="transparent")) {
         ret$fill <- ret$bg
      }
   }
   if (nchar(geoType)) {
      opR <- getOption("ursaPngLegend")
      options(ursaPngLegend=if (is.null(opR)) list(ret) else c(opR,list(ret)))
   }
   if (!is.null(ct))
      return(invisible(ct))
   invisible(ret)
}
'panel_box' <- function(...){
   if (.skipPlot(FALSE))
      return(NULL)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   if (!length(list(...))) {
      box(lwd=0.5,col=ifelse(bg<128,"#FFFFFF7F","#0000007F"))
   }
   else
      box(...)
}
'panel_lines' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   lines(...)
}
'panel_points' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   points(...)
}
'panel_text' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   text(...)
}
'panel_polygon' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   polygon(...)
}
'panel_abline' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   abline(...)
}
'panel_segments' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   segments(...)
}
'.zzz.panel_plot.--20171115' <- function(obj,...){
   if (.skipPlot(TRUE))
      return(NULL)
   if (is.null(obj))
      return(obj)
   plot(obj,...)
}
'.panel_plot' <- function(obj,...) {
   if (.skipPlot(TRUE))
      return(NULL)
   if (is.null(obj))
      return(obj)
   arglist <- list(...)
  # str(arglist)
   arglist <- lapply(arglist,function(x1) {
      if (identical(c("index","colortable"),names(x1)))
         return(unclass(unname(x1$colortable))[x1$index])
      x1
   })
   pkg <- attr(class(obj),"package")
   opW <- NULL
   if (is.character(pkg)) {
      if (pkg=="sp")
         plot <- sp::plot ## unerror "cannot coerce type 'S4' to vector of type 'double'"
         opW <- options(warn=-1) ## release 'warn=-1': no warnings
   }
  # message("-----------------")
  # str(arglist)
  # message("-----------------")
   if ((.isSF(obj))&&(.lgrep("(dens|angl)",names(arglist)))) {
     # arglist$add <- NULL
      message("'sf' cannot deal with fill patterns")
      arglist$add <- NULL
      ret <- lapply(obj,function(x1) {
         do.call("polygon",c(unclass(x1),arglist))
      })
   }
   else
      ret <- do.call("plot",c(list(obj),arglist))
   if (!is.null(opW))
      options(opW)
   ret
}
