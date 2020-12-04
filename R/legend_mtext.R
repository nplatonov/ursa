'legend_mtext' <- function(...) {
   if (.skipPlot(FALSE))
      return(NULL)
   kwd <- "mtext"
   arglist <- list(...)
   text <- .getPrm(arglist,name="(text|.*)",kwd=kwd
                  ,class=list("character","expression"),default="Title/subtitle")
   cex <- .getPrm(arglist,name="cex",kwd=kwd,default=1)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   .legend_mtext(text=text,cex=cex)
}
'.legend_mtext' <- function(text="Annotation",cex=1) {
   '.mtext' <- function(text,...) {
      if (is.character(text)) {
         if (getOption("ursaPngDevice") %in% c("windows"))
            toE <- TRUE
         else {
            opWE <- options(warn=2)
            toE <- .try(abbreviate(text,minlength=2,strict=TRUE),silent=TRUE)
            options(opWE)
         }
         if (toE)
            txt <- as.expression(substitute(bold(u),list(u=text)))
         else {
            message(paste("Note (mtext): unable to make bold label for",.dQuote(text)))
            txt <- text
         }
      }
      else if (is.expression(text))
         txt <- text
      .try(mtext(txt,...))
   }
   if (.skipPlot(FALSE))
      return(NULL)
   side <- .getSide()
   if (!length(text))
      text <- ""
   width <- max(strwidth(text,units="inches",cex=cex))
   height <- max(strheight(text,units="inches",cex=cex))
  # print(c(width=width,height=height,ratio=width/height))
   par(mar=c(0,0,0,0))
   plot(0,0,type="n",xlim=c(-1,1),ylim=c(-1,1),axes=FALSE,xlab="",ylab="")
  # text(0.5,0.5,labels=text,las=3,xpd=FALSE,cex=cex,xpd=TRUE,col="red")
   if (side==1)
      ret <- .mtext(text,side=3,cex=cex,line=-0.25,adj=0.5,padj=1.1,xpd=TRUE)
   else if (side==3)
      ret <- .mtext(text,side=1,cex=cex,line=-0.85-0.10,adj=0.5,padj=0,xpd=TRUE)
   else if (side==2)
      ret <- .mtext(text,side=4,cex=cex,line=-1,adj=0.5,padj=0,xpd=TRUE)
   else if (side==4) {
     # sink(.maketmp(ext="par"))
     # str(par())
     # sink()
      ret <- .mtext(text,side=2,cex=cex,line=c(-1.4,0)[1],adj=0.5
                   ,padj=c(0,1.1)[1],xpd=TRUE)
   }
   else
      ret <- .mtext(text,side=side,cex=cex,padj=0.75,xpd=TRUE)
   invisible(ret)
}
