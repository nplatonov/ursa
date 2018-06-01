invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (FALSE) {
      ursa:::.ursaOptions()
      compose_open()
      panel_new()
      ursa:::.ursaOptions()
      compose_close()
      ursa:::.ursaOptions()
   }
   if (TRUE)
      compose_open(height=400,legend=NULL,dev=TRUE)
   if (TRUE)
      compose_open(width=950,dpi=150,pointsize=16,legend=NULL,dev=TRUE)
   if (TRUE) {
      a <- pixelsize()
      compose_open(pointsize=8,dpi=150,scale="1:130000000")
      compose_plot(colorize(a),scalebar=TRUE)
      compose_close()
   }
   b <- ursa_dummy(nband=7,min=0,max=50,mul=1/16)
   p <- list(colorize(b[1:4],pal.rich=240,pal.rotate=0)
            ,colorize(sqrt(b[5:7]),pal.rich=-15,pal.rotate=0,stretch="equal"))
   print(p)
   if (TRUE) {
      display(p,bg="transparent")
   }
   if (TRUE) {
      cl <- compose_design(layout=c(2,4)
                          ,legend=list(list("top","full"),list("bottom",1:3)))
      compose_open(cl,dev=!TRUE,bg="red")
      ursa:::.ursaOptions()
      compose_close()
   }
   if (TRUE) {
      cl <- compose_design(p,layout=c(3,3),skip=c(4,6))
      compose_open(cl,dev=TRUE)
   }
   if (TRUE) {
      cl <- compose_design(p,side=3)
      compose_open(cl,dev=FALSE);compose_close()
   }
})
