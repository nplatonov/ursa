'.con.skeleton' <- function()
{
   obj <- list(driver=NA_character_
              ,samples=NA_integer_,lines=NA_integer_,bands=NA_integer_
              ,datatype=NA_integer_,interleave=NA_character_,byteorder=NA_integer_
              ,endian=NA_character_,swap=NA_integer_,signed=NA,scale=NA_real_
              ,offset=NA_integer_,wkt=FALSE,nodata=NA_real_,mode="raw"
              ,sizeof=NA_integer_,indexC=NA_integer_,indexR=NA_integer_,indexZ=NA_integer_
              ,posC=NA_integer_,posR=NA_integer_,posZ=NA_integer_
              ,fname=NA_character_,connection=NA_character_
              ,compress=0L,seek=NA,handle=NA)
   class(obj) <- c("ursaConnection")
   obj
}
'.is.con' <- function(obj) inherits(obj,"ursaConnection")
'.ursa_connection'<- function(x) {
   if (.is.con(x))
      return(x)
   if (is.ursa(x))
      return(x$con)
   return(NULL)
}
'print.ursaConnection' <- function(x,...) str(x,...)
'seek.ursaConnection' <- function(con,where=NA,origin="start",rw="",...)
{
   if ((1)||(con$seek))
      return(seek(con$handle,where=round(where),origin=origin,rw=rw,...))
   stop("Reopenning is needed here, but it seems that connection doesn't support seek")
   F <- con$handle
   if (isOpen(F))
   {
      close(F)
      con$handle <- with(con,do.call(connection,list(fname,"rb")))
   }
  # print(showConnections(TRUE))
   readBin(con$handle,raw(),n=as.integer(where))
   where
}
