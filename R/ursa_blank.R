'band_blank' <- function(obj,verbose=FALSE)
{
   if (!is.ursa(obj))
      return(NULL)
   z <- obj$con$posZ
   nb <- if (!is.na(z[1])) length(z) else obj$dim[2]
   res <- rep(FALSE,nb)
   if (is.matrix(obj$value))
   {
      for (i in seq_along(res)) {
         r <- unique(obj$value[,i])
         res[i] <- (length(r)==1)&&(r==0 | is.na(r))
      }
   }
   else
   {
      cb <- chunk_band(obj)
      if (pr <- verbose & length(cb)>1)
         pb <- ursaProgressBar(min=0,max=length(cb))
      for (i in cb) {
         res[i] <- apply(obj[i]$value,2,function(z) {#(all(is.na(z)))||(all(z==0)))
            r <- unique(z)
            (length(r)==1)&&(r==0 | is.na(r))
         })
         if (pr)
            setUrsaProgressBar(pb)
      }
      if (pr)
         close(pb)
   }
   res
}
'ursa_blank' <- function(obj) if (is.ursa(obj)) all(band_blank(obj)) else NULL
'.which.blank' <- function(obj) if (is.ursa(obj)) which(band_blank(obj)) else NULL
