'dim.ursaRaster' <- function(x) {
   res <- as.array(x,dim=TRUE)
   names(res) <- c("samples","lines","bands")
   res[c(2,1,3)]
}
'dim<-.ursaRaster' <- function(x,value) x
