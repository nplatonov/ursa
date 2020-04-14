'band_blank' <- function(obj,ref=c("any","0","NA"),verbose=FALSE)
{
   if (!is.ursa(obj))
      return(NULL)
   arglist <- eval(as.list(args("band_blank"))$ref)
   ref <- match.arg(as.character(ref),arglist)
  # ref <- match.arg(as.character(ref))
   z <- obj$con$posZ
   nb <- if (!is.na(z[1])) length(z) else obj$dim[2]
   res <- rep(FALSE,nb)
   if (is.matrix(obj$value))
   {
      for (i in seq_along(res)) {
         r <- unique(obj$value[,i])
         res[i] <- switch(ref
                         ,'0'=(length(r)==1)&&(r==0)
                         ,'NA'=(length(r)==1)&&(is.na(r))
                         ,(length(r)==1)&&(r==0 | is.na(r))
                         )
      }
   }
   else
   {
      cb <- chunk_band(obj)
      if (pr <- verbose & length(cb)>1)
         pb <- ursaProgressBar(min=0,max=length(cb),tail=TRUE)
      for (i in cb) {
         res[i] <- apply(obj[i]$value,2,function(z) {#(all(is.na(z)))||(all(z==0)))
            r <- unique(z)
            switch(ref
                  ,'0'=(length(r)==1)&&(r==0)
                  ,'NA'=(length(r)==1)&&(is.na(r))
                  ,(length(r)==1)&&(r==0 | is.na(r))
                  )
         })
         if (pr)
            setUrsaProgressBar(pb)
      }
      if (pr)
         close(pb)
   }
   res
}
'ursa_blank' <- function(obj,ref) {
   if (!is.ursa(obj))
      return(NULL)
   if (missing(ref))
      ref <- "any"
   all(band_blank(obj,ref))
}
'.which.blank' <- function(obj,ref) if (is.ursa(obj)) which(band_blank(obj,ref)) else NULL
