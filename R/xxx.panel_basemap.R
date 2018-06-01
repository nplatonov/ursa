'.compose_basemap' <- function(style="google") {
   if (.skipPlot(TRUE))
      return(NULL)
   sc <- getOption("ursaPngScale")
   if (is.null(sc)||(!is.numeric(sc)))
      return(NULL)
   print(sc)
   invisible(NULL)
}
'.panel_basemap' <- function(style="google") {
   if (.skipPlot(TRUE))
      return(NULL)
   sc <- getOption("ursaPngScale")
   if (is.null(sc)||(!is.numeric(sc)))
      return(NULL)
   g0 <- session_grid()
   g1 <- regrid(g0,mul=sc)
   b <- .geomap(style=style)
   print(g0)
   print(g1)
   str(b)
   session_grid(g0)
   invisible(NULL)
}
