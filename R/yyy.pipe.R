# https://thomasadventure.blog/posts/how-does-the-pipe-operator-actually-work/
`.%>%.ursaRaster` <- function(lhs, rhs) {
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  building_blocks <- c(
    rhs[[1L]],
    lhs,
    as.list(rhs[-1L])
  )
  call <- as.call(building_blocks)
  eval(call, envir = parent.frame())
}
