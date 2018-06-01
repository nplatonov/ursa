invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (first.example <- TRUE) {
      session_grid(NULL)
      session_grid(regrid(mul=1/8))
      ps <- pixelsize()
      compose_open()
      ct <- compose_panel()
      panel_shading(ps>1.1*global_mean(ps),angle=90)
      compose_legend(ct)
      compose_close()
   }
   if (second.example <- TRUE) {
      session_grid(NULL)
      a <- ursa_dummy(nband=15,mul=1/8)
      b <- local_stat(a)
      compose_open()
      lev <- 0.90
      d <- as.matrix(b["slopeS"],coords=TRUE)
      p <- list(significance.raw=colorize(b["slopeS"])
               ,significance.formatted=colorize(b["slopeS"],stretch="significance")
               ,slope=colorize(b["slope"]))
      p <- c(p,rep(p[3],3))
      names(p)[c(3,4,5)] <- c("Slope and dashed significance"
                             ,"Slope and contoured significance"
                             ,"Slope and 'contourLines'")
      compose_open(p,layout=c(2,NA),byrow=FALSE)
      compose_panel(p[1])
      compose_panel(p[2])
      compose_panel(p[3])
      panel_shading(b["slopeS"],level=lev)
      compose_panel(p[4])
      panel_contour(b["slopeS"],value=c(-lev,lev))
      compose_panel(p[5])
      lapply(contourLines(d,levels=c(-lev,lev)),panel_polygon)
      compose_panel(p[6])
      ct <- panel_contour(b["slopeS"],"color"
                         ,value=c(-0.99,-0.95,-0.9,-0.5,0.5,0.9,0.95,0.99))
      compose_legend(c(head(p,-1),'(Colorbar for contours)'=list(ct)),las=3)
      compose_close()
   }
})
