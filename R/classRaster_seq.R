'.seq.ursaRaster' <- function(obj,axis=c("z","x","y","c","r","lines","samples")) {
   fun <- match.arg(axis)
   NULL
}
'.seq.ursaGrid' <- function(obj,axis=c("z","x","y","c","r")) {
   fun <- match.arg(axis)
   NULL
}
'seq.ursaRaster' <- function(...) {
   arglist <- list(...)
   obj <- arglist[[1]]
   fun <- .getPrm(arglist[-1],valid=c("z","x","y","c","r","lines","samples"))
   if (fun=="z")
      return(seq(nband(obj)))
   g <- ursa_grid(obj)
   if (fun=="x") {
     # return(with(g,seq(minx,maxx,by=resx)[-1]+resx/2))
      return(with(g,seq(minx,maxx,len=columns+1L)[-1]-resx/2))
   }
   if (fun=="y") {
     # return(with(g,seq(miny,maxy,by=resy)[-1]+resy/2))
      return(with(g,seq(miny,maxy,len=rows+1L)[-1]-resy/2))
   }
   if (fun %in% c("c","samples")) {
     # return(with(g,seq_along(seq(minx,maxx,by=resx)[-1]+resx/2)))
      return(with(g,seq_along(seq(minx,maxx,len=columns+1L)[-1]-resx/2)))
   }
   if (fun %in% c("r","lines")) {
     # return(with(g,seq_along(seq(miny,maxy,by=resy)[-1]+resy/2)))
      return(with(g,seq_along(seq(miny,maxy,len=rows+1L)[-1]-resy/2)))
   }
}
'seq.ursaGrid' <- function(...) {
   arglist <- list(...)
   obj <- arglist[[1]]
   fun <- .getPrm(arglist[-1],valid=c("z","x","y","c","r","lines","samples"))
   if (fun=="z")
      return(1L)
   if (fun=="x") {
     # return(with(obj,seq(minx,maxx,by=resx)[-1]+resx/2))
      return(with(obj,seq(minx,maxx,len=columns+1L)[-1]-resx/2))
   }
   if (fun=="y") {
     # return(with(obj,seq(miny,maxy,by=resy)[-1]+resy/2))
      return(with(obj,seq(miny,maxy,len=rows+1L)[-1]-resy/2))
   }
   if (fun %in% c("c","samples")) {
     # return(with(g,seq_along(seq(minx,maxx,by=resx)[-1]+resx/2)))
      return(with(obj,seq_along(seq(minx,maxx,len=columns+1L)[-1]-resx/2)))
   }
   if (fun %in% c("r","lines")) {
     # return(with(g,seq_along(seq(miny,maxy,by=resy)[-1]+resy/2)))
      return(with(obj,seq_along(seq(miny,maxy,len=rows+1L)[-1]-resy/2)))
   }
}
'ursa_seqx' <- function(obj)
{
   if (missing(obj))
      obj <- session_grid()
   if (is.ursa(obj))
      obj <- ursa_grid(obj)
   if (!.is.grid(obj))
      return(NULL)
  # with(obj,seq(minx,maxx,by=resx)[-1]+resx/2)
   with(obj,seq(minx,maxx,len=columns+1L)[-1]-resx/2)
}
'ursa_seqy' <- function(obj)
{
   if (missing(obj))
      obj <- session_grid()
   if (is.ursa(obj))
      obj <- ursa_grid(obj)
   if (!.is.grid(obj))
      return(NULL)
  # with(obj,seq(miny,maxy,by=resy)[-1]+resy/2)
   with(obj,seq(miny,maxy,len=rows+1L)[-1]-resy/2)
}
'ursa_seqc' <- function(obj)
{
   if (missing(obj))
      obj <- session_grid()
   if (is.ursa(obj))
      obj <- ursa_grid(obj)
   if (!.is.grid(obj))
      return(NULL)
  # with(obj,seq_along(seq(minx,maxx,by=resx)[-1]+resx/2))
   with(obj,seq_along(seq(minx,maxx,len=columns+1L)[-1]-resx/2))
}
'ursa_seqr' <- function(obj)
{
   if (missing(obj))
      obj <- session_grid()
   if (is.ursa(obj))
      obj <- ursa_grid(obj)
   if (!.is.grid(obj))
      return(NULL)
  # with(obj,seq_along(seq(miny,maxy,by=resy)[-1]+resy/2))
   with(obj,seq_along(seq(miny,maxy,len=rows+1L)[-1]-resy/2))
}
