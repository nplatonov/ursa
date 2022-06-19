'spatial_levelsplit' <- function(obj,sep=" - ") {
   ind <- order(spatial_area(obj),decreasing=TRUE)
   res <- vector("list",length(ind-1))
   n1 <- ncol(spatial_data(obj))
   indCol1 <- seq(1L,n1)
   indCol2 <- seq(n1+1L,2L*n1)
   aname <- spatial_colnames(obj)
   res[[1]] <- obj[tail(ind,1),]
   da <- spatial_data(obj)
   if (length(dtype <- which(sapply(da,inherits,c("integer","numeric")))))
      for (i in dtype)
        spatial_data(res[[1]])[,i] <- paste("0",spatial_data(res[[1]])[,i],sep=sep)
   for (i in tail(ind,-1)) {
      res2 <- spatial_difference(spatial_geometry(obj[i+1,]),spatial_geometry(obj[i,]))
      da2 <- apply(da[c(i,i+1),,drop=FALSE],2,function(x) {
         y <- unique(x)
         if (length(y)==1)
            return(y)
         paste(unique(x),collapse=sep)
      })
      if (!is.list(da2))
         da2 <- lapply(da2,function(x) x)
      spatial_data(res2) <- as.data.frame(da2)
      res[[i+1]] <- res2
   }
   do.call(spatial_bind,res)
}
