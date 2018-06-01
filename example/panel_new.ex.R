invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (TRUE) {
      compose_open(layout=c(1,2),legend=NULL)
      panel_new()
      panel_annotation(label="Default + Empty")
      panel_new(col="#0000FF3F",density=15,angle=45,lwd=3,lty="29",verb=TRUE)
     # panel_decor()
      panel_annotation(label="Settings + Grid")
      compose_close()
   }
   if (TRUE) {
      a <- pixelsize()
      a <- a[a>560]
      display(a,blank.col="#0000FF3F",blank.density=15,blank.angle=45,blank.lwd=3
             ,coast.fill="#007F005F",coast.density=20,coast.angle=c(-30,60))
   }
})
