'.elapsedTime' <- function(message="",reset=FALSE,toPrint=FALSE)
{
   startTime <- getOption("ursaTimeStart")
   deltaTime <- getOption("ursaTimeDelta")
   if (message=="")
      message <- paste(as.character(Sys.time()),"***")
   else
      message <- paste(message,":",sep="")
   mytext <- sprintf("*** %s: %s %.2f(%.2f) seconds ***"
                   # ,as.character(Sys.time())
                    ,.argv0()
                    ,message,(proc.time()-startTime)[3]
                    ,(proc.time()-deltaTime)[3])
   if (reset)
      options(ursaTimeStart=proc.time())
   options(ursaTimeDelta=proc.time())
   if (toPrint)
      print(mytext)
   return (message(mytext))
}
'.round' <- function(x,digits=0,eps=.Machine$double.eps*9)
{
   round(x+sign(x)*eps,digits=digits)
}
'.try' <- function(...)
{
   a <- try(...)
   if ((is.character(a))&&(class(a)=="try-error"))
      return(FALSE)
   return(TRUE)
}
'.dir' <- function(pattern=NULL,path=".",all.files=FALSE,full.names=FALSE
                ,recursive=FALSE,ignore.case=TRUE,include.dirs=FALSE)
{
   a <- dir(path=path,pattern=NULL,all.files=all.files,full.names=full.names
           ,recursive=recursive,ignore.case=ignore.case,include.dirs=include.dirs)
   if (is.null(pattern))
      return (a)
   if (!.try(b <- basename(a)))
   {
      b <- a
      for (i in seq(along=a))
         if (!.try(b[i] <- basename(a[i])))
            b[i] <- NA
   }
   a[grep(pattern,b,perl=TRUE,ignore.case=ignore.case)]
}
'.grep' <- function(pattern,x,ignore.case=TRUE,perl=TRUE
                  ,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE)
{
   grep(pattern,x,ignore.case=ignore.case,perl=perl
       ,value=value,fixed=fixed,useBytes=useBytes,invert=invert)
}
'.gsub' <- function(pattern,replacement,x,ignore.case=TRUE
                  ,perl=TRUE,fixed=FALSE,useBytes=FALSE)
{
   gsub(pattern,replacement,x,ignore.case=ignore.case
              ,perl=perl,fixed=fixed,useBytes=useBytes)
}
'.gsub2' <- function(pattern,replacement,x,ignore.case=TRUE
                  ,perl=TRUE,fixed=FALSE,useBytes=FALSE)
{
   mypattern <- sprintf("^.*%s.*$",pattern)
   gsub(mypattern,replacement,x,ignore.case=ignore.case
       ,perl=perl,fixed=fixed,useBytes=useBytes)
}
'.lgrep' <- function(pattern,x,ignore.case=TRUE,perl=TRUE
                  ,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE)
{
   length(grep(pattern,x,ignore.case=ignore.case,perl=perl
               ,value=value,fixed=fixed,useBytes=useBytes,invert=invert))
}
'.dirname' <- function(x)
{
   a <- gregexpr("(/|\\\\)",x,ignore.case=TRUE,perl=TRUE)
   .gsub("^$",".",substr(x,1,max(a[[1]]-1)))
}
'.basename' <- function(x)
{
   a <- gregexpr("(/|\\\\)",x,ignore.case=TRUE,perl=TRUE)
   substr(x,max(a[[1]])+1,nchar(x))
}
'.expand.grid' <- function(...,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
    expand.grid(...,KEEP.OUT.ATTRS=KEEP.OUT.ATTRS,stringsAsFactors=stringsAsFactors)

'.gc' <- function(verbose=FALSE)
{
   a1 <- gc()
   a2 <- gc(reset=TRUE)
   if (verbose)
   {
      print(a1)
      print(a2)
   }
   invisible(NULL)
}
# '.paste' <- function(...,sep="",collapse=NULL) paste(...,sep=sep,collapse=collapse)
'.maketmp' <- function(n=1,ext="",prefix="") 
{
   if (!nchar(prefix)) {
      prefix <- basename(tempfile("","."))
      k <- nchar(prefix)
      prefix <- substr(prefix,k-3,k)
   }
   tcount <- getOption("ursaTempFileCount")
   if (is.null(tcount))
      tcount <- 0L
   list1 <- vector("character",length=n)
   for (i in seq(along=list1))
   {
      list1[i] <- sprintf("ursa%02d_%s",tcount+i,prefix)
     # list1[i] <- sprintf("tmp%s_%02d",prefix,tcount+i)
   }
   if (nchar(ext))
   {
      ext <- .gsub("^\\.","",ext)
      list1 <- paste(list1,ext,sep=".")
   }
   options(ursaTempFileCount=tcount+n)
   res <- paste0("___",list1)
  # if (!.isRscript())
  #    res <- file.path(getOption("ursaTempDir"),res)
   res
}
'.args2list' <- function(args) {
   isCMD <- missing(args)
   if (isCMD)
      args <- commandArgs(TRUE)
   else {
      args <- unlist(strsplit(args,split="\\s+"))
   }
   if (!length(args))
      return(NULL)
   if (FALSE)
      a <- strsplit(args,"=")
   else {
      a <- vector("list",length(args))
      for (i in seq_along(args)) {
         ind <- .grep("=",unlist(strsplit(args[i],"")))
         if (length(ind))
            a[[i]] <- c(substr(args[i],1,ind[1]-1)
                       ,substr(args[i],ind[1]+1,nchar(args[i])))
         else
            a[[i]] <- args[i]
      }
   }
   aname <- sapply(a,function(x){if (length(x)==1) "" else x[1]})
   opE <- options(warn=-1,show.error.messages=FALSE)
   a <- lapply(a,function(x){
      n <- length(x)
      y <- x[n]
      if (TRUE) {
        # if (n>1) ## only if named?
         if (y=="NULL")
            y <- NULL
         else if (.try(z <- eval(parse(text=y)))) {
            if (!is.null(z))
               y <- z
         }
      }
      else {
         z <- as.logical(y)
         if (!is.na(z))
            return(z)
         if (.lgrep("^(-)*\\d+$",y))
            return(as.integer(y))
         if (.lgrep("^(-)*\\d+\\.\\d+$",y))
            return(as.numeric(y))
      }
      y
   })
   options(opE)
   names(a) <- aname
   a
}
'.is.integer' <- function(x,tolerance=1e-11) {
   if (inherits(x,c("Date","POSIXt")))
      return(FALSE)
   if (is.ursa(x))
      x <- c(x$value)
   else if ((is.character(x))||(is.factor(x))) {
      ch <- grep("^\\s*(\\-)*\\d+(\\.\\d+)*((\\+|\\-)[eE]\\d+)*\\s*$",x,invert=TRUE)
      if (length(ch))
         return(FALSE)
   }
   if (any(abs(x)>1e9))
      return(FALSE)
   y <- abs(x-round(x)) ## # y <- abs(x-as.integer(round(x)))
   if (all(x>100)) {
      y <- y/x
   }
   res <- all(y<tolerance)
   res
}
'.is.rgb' <- function(obj) {
   if (.is.colortable(obj))
      return(FALSE)
   if (storage.mode(obj$value)!="integer")
      return(FALSE)
   if (!(nband(obj) %in% c(3,4)))
      return(FALSE)
   minv <- min(obj$value,na.rm=TRUE)
   maxv <- max(obj$value,na.rm=TRUE)
   if ((minv>=0)&&(maxv>=200)&&(maxv<=255))
      return(TRUE)
   FALSE
}
'.ursaOptions' <- function() {
   op <- options()
   op <- op[.grep("^ursa(Png|[A-Z]).+",names(op))]
   indPng <- .grep("^ursaPng.+",names(op))
   if (length(indPng))
      return(str(op[indPng]))
   str(op)
}
'.skipPlot' <- function(onPanel=TRUE) {
   toPlot <- getOption("ursaPngPlot")
   if ((!is.logical(toPlot))||(!toPlot))
      return(TRUE)
   if (!onPanel)
      return(FALSE)
   getOption("ursaPngSkip")
}
'.dist2' <- function(src,dst,summarize=!FALSE,positive=FALSE,verbose=TRUE)
{
   if (identical(src,dst))
      positive <- TRUE
   '.modal2' <- function(x,...)
   {
      z <- density(x,...)
      y <- z$x[match(max(z$y),z$y)]
      y
   }
   '.modal3' <- function(x) {
      res <- NA
     ## 'locfit' is not in 'suggests', 'depends'
      if (requireNamespace("locfit",quietly=.isPackageInUse()))
         try(res <- x[which.max(predict(locfit::locfit(~x),newdata=x))])
      res
   }
   d1 <- dim(src)
   d2 <- dim(dst)
   if ((length(d1)<2)||(d1[2]<2)||(length(d2)<2)||(d2[2]<2))
      return(NULL)
   b1 <- .Cursa("dist2dist",x1=as.numeric(dst[,"x"]),y1=as.numeric(dst[,"y"])
                       ,x2=as.numeric(src[,"x"]),y2=as.numeric(src[,"y"])
                       ,nxy=nrow(dst),ndf=nrow(src),positive=as.integer(positive)
                       ,verb=as.integer(verbose)
                       ,dist=numeric(nrow(src)),ind=integer(nrow(src)))
   b1 <- data.frame(ind=b1$ind+1L,dist=b1$dist)
   if (summarize)
   {
      d <- b1$dist
      if (!.try(m <- .modal3(d)))
         m <- NA
      if (verbose)
         print(c(avg=mean(d),median=median(d),mode2=.modal2(d),mode3=m))
   }
   b1
}
'.is.eq' <- function(x,value) {
   if (abs(value)<1)
      abs(x-value)<1e-27
   else
      abs(x/value-1)<1e-6
}
'.is.ge' <- function(x,value) x>value | .is.eq(x,value)
'.is.le' <- function(x,value) x<value | .is.eq(x,value)
'.is.gt' <- function(x,value) x>value
'.is.lt' <- function(x,value) x<value
'.is.near' <- function(x1,x2,verbose=FALSE) {
   m1 <- match(x1,x2)
   if (all(!is.na(m1))) { ## 20161222 add 'all', removed 'any'
      if (verbose)
         message(".is.near: exact matching")
      return(m1)
   }
   n1 <- length(x1)
   n2 <- length(x2)
   b1 <- .Cursa("isNear",x1=as.numeric(x1),x2=as.numeric(x2),n1=n1,n2=n2
           ,res=integer(n1),NAOK=TRUE)$res
   b1[b1==0] <- NA
   if (verbose)
      message(".is.near: fuzzy matching")
   b1
}
'.getMajorSemiAxis' <- function(proj4) {
   ell <- .gsub(".*\\+ellps=(\\S+)\\s.*","\\1",proj4)
   if (ell=="WGS84")
      B <- 6378137
   else if (ell==proj4) {
      B <- .gsub(".*\\+a=(\\S+)\\s.*","\\1",proj4)
      if (B!=proj4)
         B <- as.numeric(B)
      else {
         opW <- options(warn=-1)
         warning("Supposed that this projection is not supported yet")
         options(opW)
         B <- 6378137
      }
   }
   else {
      opW <- options(warn=-1)
      warning("Supposed that this projection is not supported yet")
      options(opW)
      B <- 6378137
   }
   B
}
'.degminsec' <- function(x,suffix=c("A","B")) {
   s <- sign(x)
   x <- abs(x)
   y <- rep("",length(x))
   x1 <- floor(x)
   x2 <- floor((x-x1)*60)
   x3a <- (x-x1-x2/60)*3600
   x3 <- .round(x3a)
   ind2 <- which(x3==60)
   if (length(ind2)) {
      x2[ind2] <- x2[ind2]+1
      x3[ind2] <- 0
   }
   ind2 <- which(x2==60)
   if (length(ind2)) {
      x1[ind2] <- x1[ind2]+1
      x2[ind2] <- 0
   }
   x1a <- abs(x1)
   if (all(c(x2,x3)==0))
      y <- sprintf("%.0f\uB0",x1a)
   else if (all(x3==0))
      y <- sprintf("%.0f\uB0%02.0f'",x1a,x2)
   else
      y <- sprintf("%.0f\uB0%02.0f'%02.0f\"",x1a,x2,x3)
   if (length(ind2 <- s>=0))
      y[ind2] <- paste0(y[ind2],suffix[1])
   if (length(ind2 <- s<0))
      y[ind2] <- paste0(y[ind2],suffix[2])
   if ((length(unique(y))==1)&&(length(unique(x))!=1))
      return(paste0(as.character(x),"\uB0",suffix[1]))
   y
}
'.isRscript' <- function() .lgrep("^(--file=|-f$|--slave$)",commandArgs(FALSE))>=2
'.isPackageInUse' <- function() "ursa" %in% loadedNamespaces()
'.argv0path' <- function() {
   arglist <- commandArgs(FALSE)
   if (length(ind <- .grep("^--file=.+",arglist,ignore.case=FALSE))==1)
      return(strsplit(arglist[ind],"=")[[1]][2])
   if (length(ind <- .grep("^-f$",arglist,ignore.case=FALSE))==1)
      return(arglist[ind+1L])
   ""
}
'.argv0' <- function() basename(.argv0path())
'.argv0dir' <- function() dirname(.argv0path())
'.argv0name' <- function() .gsub("^(.+)(\\.)(.+)*$","\\1",.argv0())
'.argv0ext' <- function() .gsub("^(.+)(\\.)(.+)*$","\\2\\3",.argv0())
'.argv0png' <- function() Fout <- sprintf("%s%%02d.png",.argv0name())
'.argv0pdf' <- function() paste0(.argv0name(),".pdf")
'.dQuote' <- function(ch) paste0("\"",ch,"\"")
'.sQuote' <- function(ch) paste0("'",ch,"'")
'.require' <- function(pkg,quietly=TRUE) do.call("require",list(pkg,quietly=quietly))
'.tryE' <- function(...) {
   opE <- options(show.error.messages=TRUE)
   ret <- try(...)
   options(opE)
   ret
}
'.loaded' <- function() gsub("^package:","",grep("^package:",search(),value=TRUE))
'.in.memory' <- function(obj) {
   if (!is.ursa(obj))
      return(NA)
   !is.null(dim(obj$value))
}
'.normalizePath' <- function(path) normalizePath(path,winslash="/",mustWork=FALSE)
'.isKnitr' <- function() {
  # cond1 <- requireNamespace("knitr",quietly=.isPackageInUse())
  # if (!cond1)
  #    return(FALSE)
  # is.character(knitr::current_input())
   ("knitr" %in% loadedNamespaces())&&(is.character(knitr::current_input()))
}
'.isJupyter' <- function() {
   "jupyter:irkernel" %in% search()
  # "IRkernel" %in% loadedNamespaces()
}
'.open' <- function(...) {
   arglist <- lapply(list(...), function(x) {
      if (!file.exists(x)) {
         if (.lgrep("\\%(\\d)*d",x))
            x <- sprintf(x,1L)
         else
            x <- NULL
      }
      x
   })
   system2("R",c("CMD","open",arglist))
}
'.isSF' <- function(obj) inherits(obj,c("sf","sfc"))
'.isSP' <- function(obj) {
   ((inherits(obj,"Spatial"))||
    (.lgrep("Spatial(Points|Lines|Polygons)DataFrame",class(obj))))

}
'.is.numeric' <- function(obj) {
   opW <- options(warn=-1)
   res <- as.numeric(na.omit(obj))
   options(opW)
   !anyNA(res)
}
'.is.equal.crs' <- function(obj1,obj2=NULL) {
   oprj <- spatial_proj4(obj1)
   sprj <- if (is.null(obj2)) session_proj4() else spatial_proj4(obj2)
   if (nchar(sprj)<3)
      return(FALSE)
   oprj2 <- .gsub("\\+wktext\\s","",oprj)
   sprj2 <- .gsub("\\+wktext\\s","",sprj)
   oprj2 <- .gsub("(^\\s|\\s$)","",oprj2)
   sprj2 <- .gsub("(^\\s|\\s$)","",sprj2)
   ret <- identical(oprj2,sprj2)
   ret
}
