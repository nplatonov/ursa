'segmentize' <- function(obj,by=NULL,connect=c("united","consequent")) {
   connect <- match.arg(connect)
  # if (!is.null(by))
  #    connect <- "united"
   if ((!is.null(by))&&(connect=="consequent")) {
      ret <- do.call(spatial_bind,by(obj,by,segmentize,connect=connect)) ## RECURSIVE
      ##~ for (a in spatial_fields(obj)) {
         ##~ byvalue <- obj[[a]]
         ##~ str(a)
         ##~ str(byvalue)
         ##~ if (is.integer(byvalue))
            ##~ ret[[a]] <- as.integer(ret[[a]])
         ##~ else if (is.numeric(byvalue))
            ##~ ret[[a]] <- as.numeric(ret[[a]])
         ##~ else if (inherits(byvalue,"Date"))
            ##~ ret[[a]] <- as.Date(ret[[a]])
      ##~ }
      return(ret)
   }
   if ((TRUE)&&(T & is.null(by))&&(spatial_geotype(obj) %in% c("MULTIPOINT"))&&
       (spatial_count(obj)>1)) {
      ret <- lapply(seq_len(spatial_count(obj)),\(j) {
         res <- segmentize(obj[j,],by=spatial_data(obj[j,]),connect=connect) ## RECURSIVE
      })
      ret <- do.call(spatial_bind,ret)
      return(ret)
   }
   if (is_spatial_lines(obj)) {
      crd <- sapply(seq(1,2),function (x) basename(tempfile(pattern="")))
      if (is.null(by)) {
         by <- rep(0L,spatial_count(obj))
      }
      index <- by(obj,by,rownames)
      conseq <- is.list(index)
      if (!conseq)
         da <- spatial_data(obj)
      a <- lapply(index,function(x) {
         if (is.null(x))
            return(NULL)
         ind <- match(x,rownames(spatial_data(obj)))
         xy <- do.call(rbind,spatial_coordinates(obj[ind,]))
         colnames(xy) <- crd
         if (!conseq) {
            return(cbind(xy,da[rep(ind,nrow(xy)),,drop=FALSE]))
         }
         ind2 <- c(1L,which(seq_len(nrow(xy)) %% 2 ==0))
        # ind2 <- which(!duplicated(xy))
         da <- spatial_data(obj[c(ind[1L],ind),])
         cname <- colnames(da)
         if (length(ind3 <- grep("\\.1$",cname))>0) {
            for (i in ind3) {
               da[1,gsub("\\.1$","",cname[i])] <- da[1,i]
            }
            da[,ind3] <- NULL
         }
         cbind(xy[ind2,],da)
      })
      ret <- spatialize(do.call(rbind,a),coords=crd
                     ,crs=spatial_crs(obj),engine=ifelse(.isSF(obj),"sf","sp"))
      rownames(ret) <- NULL
      return(ret)
   }
   if (!is_spatial_points(obj))
      return(NULL)
   xy <- unname(spatial_coordinates(obj))
   if (is.list(xy)) {
      if (length(xy)==1)
         xy <- xy[[1]]
      else {
         str(xy)
         stop("It seems that MULTI<geometry>, which is unsupported")
      }
   }
   if (connect=="united") {
      if (!is.null(by)) {
         ##~ if ((length(by)==1)&&(by %in% spatial_fields(obj))) {
            ##~ dname <- by
            ##~ by <- obj[[by]]
         ##~ }
         ##~ else
            ##~ dname <- NULL
        # q()
         if (!.isSF(obj))
            crd <- dimnames(obj@coords)[[2]]
         res <- by(obj,by,simplify=FALSE,function(x) {
            if (!.isSF(x)) {
              # sp::coordinates(x) <- c("coords.x1","coords.x2")
               sp::coordinates(x) <- crd
            }
            ret <- segmentize(x,connect=connect) ## RECURSIVE
            ret
         })
         ind <- which(!sapply(res,is.null))
         da <- attr(res,"dimnames")
         if (!is.data.frame(da)) {
            da <- do.call(expand.grid,list(da,stringsAsFactors=FALSE))[ind,,drop=FALSE]
            attr(da,"out.attrs") <- NULL
         }
         if (ncol(da)==1) {
            byname <- as.list(match.call())[["by"]]
            byvalue <- eval(byname,envir=parent.frame())
            if (is.list(byvalue)) {
               dname <- names(byvalue)
               byvalue <- byvalue[[1]]
            }
            else {
               list1 <- as.list(match.call())
               dname <- gsub(paste0(as.character(list1["obj"]),"\\$"),""
                            ,as.character(list1["by"]))
               by <- data.frame(array(by,dim=c(length(by),1),dimnames=list(NULL,dname))
                               ,check.names=FALSE)
            }
            colnames(da) <- dname
           # if (is.null(names(by)))
           #    names(by) <- byname
         }
         for (a in names(by)) {
            byvalue <- by[[a]]
            if (is.integer(byvalue))
               da[[a]] <- as.integer(da[[a]])
            else if (is.numeric(byvalue))
               da[[a]] <- as.numeric(da[[a]])
            else if (inherits(byvalue,"Date"))
               da[[a]] <- as.Date(da[[a]])
         }
         res <- do.call(spatial_bind,res[ind])
         spatial_data(res) <- da
         if (.isSP(obj))
            sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
      }
      else if (.isSF(obj)) {
         if (nrow(xy)==1)
            res <- sf::st_sfc(sf::st_linestring(xy[integer(),]),crs=spatial_crs(obj))
         else
            res <- sf::st_sfc(sf::st_linestring(xy),crs=spatial_crs(obj))
      }
      else if (.isSP(obj)) {
         res <- sp::Lines(sp::Line(xy),1L)
         res <- sp::SpatialLines(list(res))
         sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
      }
      else
         res <- NULL
      return(res)
   }
   ind <- tail(seq_len(nrow(xy)),-1)
   n <- length(ind)
   res <- vector("list",n)
   if (.isSF(obj)) {
      for (i in seq_along(ind)) {
         res[[i]] <- sf::st_linestring(xy[ind[i]+c(-1,0),])
      }
      res <- sf::st_sfc(res,crs=spatial_crs(obj))
   }
   else if (.isSP(obj)) {
      for (i in seq_along(ind)) {
         res[[i]] <- sp::Lines(sp::Line(xy[ind[i]+c(-1,0),]),ind[i])
      }
      res <- sp::SpatialLines(res)
      sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
   }
   else
      return(NULL)
   if (F)
      spatial_data(res) <- tail(spatial_data(obj),-1)
   else {
      da <- spatial_data(obj)
      if (nrow(da)==1)
         spatial_data(res) <- da
      else {
         da1 <- head(da,-1)
         da2 <- tail(da,-1)
         ind <- rep(NA,ncol(da)) 
         for (i in seq_len(ncol(da1))) {
            ind[i] <- identical(da1[,i],da2[,i])
         }
         da1[,which(ind)] <- NULL
         spatial_data(res) <- data.frame(cbind(da2,da1))
      }
   }
   res
}
