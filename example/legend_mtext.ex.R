invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(1,min=-10,max=+30)
   compose_open(legend=list("right","top","bottom","left"))
   panel_new()
   ct <- panel_raster(a)
   legend_colorbar(ct)#,units=as.expression(substitute(bold(degree*C))))
   legend_mtext("Characters are in bold")
   legend_mtext(as.expression(substitute(italic(
               paste("Units can be interpreted as",~degree*C)))),cex=0.7)
   legend_mtext(text=as.expression(substitute(italic(paste("Omega powered by alpha is",~~Omega^alpha)))))
   compose_close(execute=!FALSE)
})
