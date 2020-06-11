'ursaProgressBar' <- function(kind=c("tk","txt")
                           # ,title=basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
                             ,title=.argv0(),label=""
                             ,min=0,max=1,initial=min,width=NA,style=1
                             ,tail=FALSE,silent=FALSE) {
   if (silent) {
      pb <- logical()
      class(pb) <- "ursaProgressBar"
      return(pb)
   }
  # if (.isKnitr())
  #    return(NULL)
   if (!.try(kind0 <- match.arg(kind))) {
      n <- length(kind)
      max <- if ((n==1)&&(is.numeric(kind))&&(.is.integer(kind))) kind else n
      kind <- head(eval(formals()$kind),1)
   }
   else
      kind <- kind0
   if (kind=="tk") {
      Rver <- R.Version()
      if ((.Platform$OS.type=="unix")&&(!nchar(Sys.getenv("DISPLAY"))))
         kind <- "txt"
      else if ((!requireNamespace("tcltk",quietly=.isPackageInUse()))||
        (!capabilities("tcltk"))||((Rver$'svn rev' %in% c("78619"))&&(Rver$arch %in% c("x86_64"))))
         kind <- "txt"
   }
   if ((!is.na(title))&&(!nchar(title)))
      title <- "'ursa' package"
   t1 <- proc.time()[3]
   names(t1) <- title
   st <- c(t1,prev=unname(t1),delta=0,min=min,max=max,current=initial
          ,tail=as.numeric(tail))
   if (kind=="tk") {
      if (is.na(width))
         width <- 360
      retina <- 1 # getOption("ursaRetina")
      if (is.null(retina))
         retina <- 1
      width <- round(width*retina)
      title <- sprintf("%s: %.0f of %.0f",title,initial,max)
      pb <- try(tcltk::tkProgressBar(title=title,label=label,min=min,max=max
                         ,initial=initial,width=width))
      if (inherits(pb,"try-error"))
         pb <- utils::txtProgressBar(title=title,label=label,min=min,max=max
                             ,initial=initial,width=width,style=style)
   }
   ##~ else if ((.Platform$OS.type=="windows")&&(kind=="win")) {
      ##~ if (is.na(width))
         ##~ width <- 300
      ##~ pb <- utils::winProgressBar(title=title,label=label,min=min,max=max
                          ##~ ,initial=initial,width=width)
   ##~ }
   else
      pb <- utils::txtProgressBar(title=title,label=label,min=min,max=max
                          ,initial=initial,width=width,style=style)
   pb$optionName <- paste0("ursaProgressBar",basename(.maketmp()))
   op <- list(st)
   names(op) <- pb$optionName
   options(op)
   class(pb) <- c("ursaProgressBar",class(pb))
   if (tail)
      setUrsaProgressBar(pb)
   pb
}
'setUrsaProgressBar' <- function(pb,value,title=NULL,label=NULL) {
   if ((length(class(pb))==1)&&(inherits(pb,"ursaProgressBar")))
      return(pb)
  # if (.isKnitr())
  #    return(pb)
   t2 <- unname(proc.time()[3])
   cl <- class(pb)[2]
   st <- getOption(pb$optionName)
   if (missing(value))
      st["current"] <- st["current"]+1
   else 
      st["current"] <- value
  # if (names(st[1])=="first")
  #    print(st)
   if (!is.null(title))
      names(st)[1] <- title
   st["delta"] <- t2-st["prev"]+st["delta"]
   reset <- st["delta"]>0.5 | st["current"]==1 | st["current"]>st["max"]
   if (reset)
      st["delta"] <- 0
   st["prev"] <- t2
   op <- list(st)
   names(op) <- pb$optionName
   options(op)
   if ((!reset)&&(T & st["max"]!=st["current"]))
      return(NULL)
   if (missing(value))
      value <- unname(st["current"])-1
   ##~ if (st["current"]>st["max"])
      ##~ st["current"] <- st["max"]
  # if (value>=st["max"])
  #    value <- unname(st["max"])-1
  # print(c(st,value=value))
   if (value<st["max"])
      title <- sprintf("%s: %.0f of %.0f",names(st)[1],value+1,st["max"])
   else {
      title <- sprintf("%s: %s!",names(st)[1]
          ,ifelse(isTRUE(all.equal(value,unname(st["max"]))),"completed","OVER LIMIT"))
   }
   d1 <- unname((value-st["min"])/(st["max"]-st["min"]))
   t3 <- t2-st[1]
  # print(cbind(as.data.frame(t(st)),onset=reset,curr=t3))
   if (t2-st[1]<0.49999)
      label <- sprintf("%5.0f%% %56s",d1*100," ")
   else {
      d2 <- 1-d1
      if (d1==0)
         d3 <- 0
      else
         d3 <- d2*(t2-st[1])/d1
      d6 <- ceiling(as.numeric(as.difftime(d3,units="secs")))
      d8 <- round(t3)
      d9 <- d6+d8
      d4h <- d6 %/% 3600
      d4 <- d6-d4h*3600
      d4m <- d4 %/% 60
      d4s <- d4-d4m*60
      if (is.na(d4h)) {
         print(c(d1=d1,d2=d2,d3=d3,d6=d6,d8=d8,d9=d9,d4h=d4h,d4=d4,d4m=d4m,d4s=d4s))
         Sys.sleep(999)
         stop("PROGRESSBAR")
      }
      if (d4h==0) {
         d4f <- sprintf("%02d:%02d:%02d",d4h,d4m,d4s)
         d5 <- format(Sys.time()+d6,"(%H:%M:%S)")
      }
      else {
         d4f <- sprintf("%02dh%02dm",d4h,d4m)
         d5 <- format(Sys.time()+d6,"(%d%b %H:%M)")
      }
      d8s <- d8%%60
      d8 <- (d8-d8s)/60
      d8m <- d8%%60
      d8h <- (d8-d8m)/60
     # d8 <- sprintf("%02d:%02d:%02d",d8h,d8m,d8s)
      d9s <- d9%%60
      d9 <- (d9-d9s)/60
      d9m <- d9%%60
      d9h <- (d9-d9m)/60
      if (d9h==0)
         d9 <- sprintf("%02d:%02d:%02d",d9h,d9m,d9s)
      else
         d9 <- sprintf("%02dh%02dm",d9h,d9m)
      if (d8h==0)
         d8 <- sprintf("%02d:%02d:%02d",d8h,d8m,d8s)
      else
         d8 <- sprintf("%02dh%02dm",d8h,d8m)
      d8 <- paste0("+",d8)
      d4f <- paste0("-",d4f)
     # d9 <- paste0("=",d9)
      if ((TRUE)||((st[4]>1500)||(d1>0.05)))
         label <- sprintf("%5.0f%% %9s %9s %9s %12s",d1*100,d8,d4f,d9,d5)
      else
        # label <- sprintf("%5.0f%% ???:??? (????-??-?? ??:??)",d1*100)
         label <- sprintf("%5.0f%% %30s",d1*100," ")
   }
  # print(c(value=value))
   if (cl=="tkProgressBar")
      return(tcltk::setTkProgressBar(pb,value,title=title,label=label))
   ##~ else if ((cl=="winProgressBar")&&(.Platform$OS.type=="windows"))
      ##~ return(utils::setWinProgressBar(pb,value,label=label))
   utils::setTxtProgressBar(pb,value,title=title,label=label)
}
'close.ursaProgressBar' <- function(con,...) {
   if (!length(con))
      return(invisible(NULL))
   p <- getOption(con$optionName)
   if (p["current"]==p["max"]) {
      setUrsaProgressBar(con,unname(p["max"]))
   }
   NextMethod("close",con,...)
}
# 'close.ursaProgressBar' <- function(con,...) close(con,...)
