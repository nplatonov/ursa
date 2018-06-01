invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (TRUE)
   {
      compose_open(layout=c(2,3),legend=NULL,device="cairo")
      for (i in seq(6)) {
         panel_new()
         panel_annotation(label=LETTERS,cex=1.5)
         panel_annotation(label=rev(letters),cex=1.5,pos="bottomright")
         panel_annotation(pos=c(0.7,0.2)
                          ,label=paste("panel",paste("no.",i),sep="\n"))
         if (i==1)
            panel_annotation(pos="center")
      }
      compose_close()
   }
   if (TRUE) {
      display(pixelsize(),scale=2
             ,ann.label="FJL",ann.lon=52,ann.lat=80,ann.buffer=1
             ,ann.bg="#8F6FFF2F",ann.fill="#FFFF7F9F",ann.font="courier")
   }
   if (TRUE) {
      display(blank.col="white",grid=FALSE,coast=FALSE,legend=NULL,scale=NA,dpi=NA
             ,ann=TRUE,ann.label="FJL",ann.lon=52,ann.lat=80,ann.buffer=1.2,
             ,ann.bg="#FF00000F",ann.verbose=TRUE)
   }
   cat("Done!\n")
})
