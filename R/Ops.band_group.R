'band_mean' <- function(obj) {
   res <- band_stat(obj)$mean
   names(res) <- bandname(obj)
   res
}
'band_min' <- function(obj) {
   res <- band_stat(obj)$min
   names(res) <- bandname(obj)
   res
}
'band_max' <- function(obj) {
   res <- band_stat(obj)$max
   names(res) <- bandname(obj)
   res
}
'band_sum' <- function(obj) {
   res <- band_stat(obj)$sum
   names(res) <- bandname(obj)
   res
}
'band_sd' <- function(obj) {
   res <- band_stat(obj)$sd
   names(res) <- bandname(obj)
   res
}
'band_n' <- function(obj) {
   res <- band_stat(obj)$n
   names(res) <- bandname(obj)
   res
}
'band_nNA' <- function(obj) {
   res <- band_stat(obj)$nNA
   names(res) <- bandname(obj)
   res
}
