##~ a <- list(A=list(a=1),B=list(a=2))
##~ str(c('111'=a[1],'222'=a[2]))
##~ q()
invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   b <- lapply(as.list(ursa_dummy(2)),colorize)
   cd <- compose_design(layout=c(1,2),legend=list(list(1,"left"),list(1,"right")
                                          ,list("top","full"),list("bottom",1)))
   for (i in 1:4) {
      compose_open(cd,dev=i==1)
      ct <- compose_panel(b,decor=FALSE)
      if (i==2)
         compose_legend(ct)
      else if (i==3)
         compose_legend(ct[[1]],'Tomorrow'=b[[2]]
                       ,top="This is example of legend composition"
                       ,format(Sys.Date(),"(c) %Y"))
      else if (i==4)
         compose_legend(c(ct,"top","bottom"),units=c("left","right"))
      compose_close()
   }
})
