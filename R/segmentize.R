'segmentize' <- function(obj,by=NULL,connect=c("consequent","united")) {
   if (!is.null(by))
      connect <- "united"
   connect <- match.arg(connect)
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
            segmentize(x,connect=connect) ## RECURSIVE
         })
         ind <- which(!sapply(res,is.null))
         if (smartRenaming <- FALSE) {
            da <- by(spatial_data(obj),by,function(x) x)
            str(da)
            list1 <- as.list(match.call())
            str(list1)
            dname <- gsub(paste0(as.character(list1["obj"]),"\\$"),""
                         ,as.character(list1["by"]))
            str(dname)
            q()
         }
         da <- attr(res,"dimnames")
         if (!is.data.frame(da))
            da <- do.call(expand.grid,da)
         res <- do.call(spatial_bind,res[ind])
         spatial_data(res) <- da
         if (.isSP(obj))
            sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
      }
      else if (.isSF(obj)) {
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
     # str(res)
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
      da1 <- head(da,-1)
      da2 <- tail(da,-1)
      ind <- rep(NA,ncol(da)) 
      for (i in seq_len(ncol(da1))) {
         ind[i] <- identical(da1[,i],da2[,i])
      }
      da1[,which(ind)] <- NULL
      spatial_data(res) <- data.frame(cbind(da2,da1))
   }
   res
}
