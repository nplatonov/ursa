invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- ursa_dummy(6)
   b1 <- list(maxi=a[1:4]*1e2,mini=a[5:6]/1e2)
   print(b1)
   b2 <- lapply(b1,function(x) colorize(x,nbreak=ifelse(global_mean(x)<100,5,NA)))
   compose_open(b2,byrow=FALSE
               ,legend=list(list("bottom",1:2),list("bottom",3),list("left")))
   ct <- compose_panel(b2,scalebar=2,coastline=3:4,gridline=5:6,gridline.margin=5
                      ,annotation.text=as.character(seq(6)))
   compose_legend(ct)
   legend_mtext(as.expression(substitute(italic("Colorbars are on the bottom"))))
   compose_close()
})
