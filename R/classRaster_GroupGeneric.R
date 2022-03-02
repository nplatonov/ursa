## ?groupGeneric
# 'Summary.ursaRaster' <- function(x,cover=0.5-1e-3,weight=NULL,verbose=FALSE) {
'.zzz1.Summary.ursaRaster' <- function(x,cover=0.5-1e-3,weight=NULL,verbose=FALSE
                                ,na.rm=FALSE,bandname="") {
   res <- .groupSummary(x,generic=.Generic,cover=cover,weight=weight
                       ,bandname=bandname,verbose=verbose)
   res
}
'Summary.ursaRaster' <- function(...,na.rm=FALSE) {
   arglist <- list(...)
   cover <- .getPrm(arglist[-1],name="(cov|cvr)",default=0.5-1e-3)
   weight <- .getPrm(arglist[-1],name="w",class="numeric",default=NULL)
   verbose <- .getPrm(arglist[-1],name="verb(ose)*",default=FALSE)
   bandname <- .getPrm(arglist[-1],name="name",default="")
   if (verbose)
      str(list(generic=.Generic,cover=cover,weight=weight
              ,bandname=bandname,verbose=verbose))
   res <- .groupSummary(arglist[[1]],generic=.Generic,cover=cover,weight=weight
                       ,bandname=bandname,verbose=verbose)
   res
}
'.zzz2.Summary.ursaRaster' <- function(x,na.rm=TRUE) {
   print(.Generic)
   stop("GLOBAL")
  ## global
   do.call(.Generic,list(x$value,na.rm=na.rm))
}
'Math.ursaRaster' <- function(x,...)
{
  # print(.Generic)
   x <- discolor(x)
  # opW <- options(warn=ifelse(.isPackageInUse(),-1,1))
   x$value[] <- do.call(.Generic,list(x$value,...))
  # options(opW)
   get("x")
}
'Complex.ursaRaster' <- function(z)
{
   stop("Unsupported")
}
'Ops.ursaRaster' <- function(e1,e2=NULL)
{
   verbose <- FALSE
   if (is.ursa(e1)) {
     # e1 <- discolor(e1) ## removed 20160805
      if (.is.category(e1)) {## added 20161214 # attr(e1$value,"category")
         ct1 <- ursa(e1,"colortable")
         cl1 <- ursa(e1,"category")
         e1 <- discolor(e1)
        # e1 <- .extract(e1)
      }
   }
   else {
      cl1 <- ""
   }
   if (is.ursa(e2)) {
    # e2 <- discolor(e2) ## removed 20160805
      if (.is.category(e2)) {## added 20161214 # attr(e2$value,"category")
         ct2 <- ursa(e1,"category")
         e2 <- discolor(e2)
        # e2 <- .extract(e2)
      }
   }
   else
      cl2 <- ""
   if (nargs()==11L)
   {
      e2 <- e1
      e1 <- 0
      if (.Generic %in% c("!"))
         .Generic <- "!="
   }
   if (nargs()==1L)
   {
      if (.Generic %in% c("!"))
      {
         val <- e1$value
         val[] <- NA
         val[is.na(e1$value)] <- 1L
         e1$value <- val
         class(e1$value) <- "ursaNumeric"
         ignorevalue(e1) <- 127L
         return(e1)
      }
      if (.Generic %in% c("-"))
      {
         e1$value[] <- -e1$value
         return(e1)
      }
   }
   FUN <- get(.Generic,envir=parent.frame(),mode="function")
   f <- quote(FUN(left,right)) ##eval(f)
   if (FALSE)
   {
      if (is.matrix(e1))
         dim(e1) <- c(prod(dim(e1)),1)
      if (is.matrix(e2))
         dim(e2) <- c(prod(dim(e2)),1)
   }
   if ((is.character(e2))&&(is.ursa(e1))) {
      if (.lgrep(e2,names(e1)))
         e2 <- e1[e2]
      else {
         ind <- .grep(e2,cl1)
         if (length(ind) == 1)
            e2 <- ind-1L
         else {
            e1$value[!(e1$value %in% (ind-1L))] <- NA
            e1$value[!is.na(e1$value)] <- 1L
           # ursa(e1,"colortable") <- ct1
            return(e1)
           # message(paste("multiple categories are detected:"
           #              ,paste(.sQuote(ct1[ind]),collapse=", ")))
           # stop("conditions with multiple values are not implemented")
         }
      }
   }
   if ((is.character(e1))&&(is.ursa(e2))) {
      e1 <- e2[e1]
   }
   isImage1 <- is.ursa(e1)
   isImage2 <- is.ursa(e2)
   isArray1 <- is.array(e1)
   isArray2 <- is.array(e2)
   isNumeric1 <- is.numeric(e1)
   isNumeric2 <- is.numeric(e2)
   if (verbose)
      print(c(isImage1=isImage1,isImage2=isImage2
             ,isArray1=isArray1,isArray2=isArray2
             ,isNumeric1=isNumeric1,isNumeric2=isNumeric2))
   n1 <- if (isImage1) dim(e1$value)[2] 
         else if (isArray1) dim(e1)[2] 
         else if (isNumeric1) length(e1)
         else stop("e1: unsupported type")
   n2 <- if (isImage2) dim(e2$value)[2] 
         else if (isArray2) dim(e2)[2] 
         else if (isNumeric2) length(e2)
         else stop("e2: unsupported type")
   n3 <- if ((n1>=n2)&&((n2==1)||(n1==n2))) n1 
         else if ((n2>1)&&(n1==1)) n2 
         else {message(paste0("n1=",n1," n2=",n2,": unpredictive"));max(n1,n2)}
   varName <- if ((isImage1)&&(!isImage2)) "e1"
              else if ((!isImage1)&&(isImage2)) "e2"
              else if ((isImage1)&&(isImage2)) if (n3==n1) "e1" else "e2"
              else stop("What else?")
   if (verbose)
      print(data.frame(n1=n1,n2=n2,n3=n3,varName=varName))
   isNew <- n3!=dim(get(varName)$value)[2]
   if (isNew)
   {
      e3 <- rep(get(varName),n3)
      varName <- "e3"
   }
   seq1 <- rep(seq(n1),len=n3)
   seq2 <- rep(seq(n2),len=n3)
  # st1 <- st2 <- st3 <- 0
   if ((n1==1)||(n2==1))
      seqi <- list(seq(n3))
   else
      seqi <- lapply(seq(n3),function(x) x)
   for (i in seqi) {
     # st1 <- system.time({
         left <- if (isImage1) e1$value[,seq1[i]]
                 else if (isArray1) e1[,seq1[i]]
                 else e1[seq1[i]]
         right <- if (isImage2) e2$value[,seq2[i]]
                  else if (isArray2) e2[,seq2[i]]
                  else e2[seq2[i]]
     # })["elapsed"]+st1
     # st2 <- system.time({
         if (varName=="e1")
            e1$value[,i] <- eval(f)
         else if (varName=="e2")
            e2$value[,i] <- eval(f)
         else if (varName=="e3")
            e3$value[,i] <- eval(f)
     # })["elapsed"]+st2
     # st3 <- system.time({
         if ((TRUE)&&(.Generic %in% c(">","<",">=","<=","==","!=")))
         {
            if (varName=="e1") {
               e1$value[,i][!e1$value[,i]] <- NA
               ignorevalue(e1) <- 127L
            }
            else if (varName=="e2") {
               e2$value[,i][!e2$value[,i]] <- NA
               ignorevalue(e2) <- 127L
            }
            else if (varName=="e3") {
               e3$value[,i][!e3$value[,i]] <- NA
               ignorevalue(e3) <- 127L
            }
         }
     # })["elapsed"]+st3
     # print(summary(e1$value[,i]))
     # message("-------------")
   }
  # print(c(st1=st1,st2=st2,st3=st3))
   return(get(varName))
}
'.groupSummary' <- function(obj,generic=c("blank","all","any","sum"
                                         ,"prod","min","max","range"
                                         ,"mean","median","sd","var","length")
                           ,cover=0.5-1e-3,weight=NULL,bandname=""
                           ,verbose=FALSE)
{
   if (!is.ursa(obj))
      return(NULL)
   generic <- match.arg(generic)
   fun <- generic
   if (generic=="range") { # recursive!!!
      res <- c(.groupSummary(obj,"min",cover=cover,weight=weight,verbose=verbose)
              ,.groupSummary(obj,"max",cover=cover,weight=weight,verbose=verbose))
      return(res)
   }
   code <- switch(generic,blank=0L,all=1L,any=2L,sum=3L,prod=4L
                 ,min=5L,max=6L,range=7L,mean=8L,median=9L
                 ,sd=10L,var=11L,length=12L,-1L)
   g0 <- session_grid()
   session_grid(obj)
   if (.is.con(obj$con))
      nodata <- obj$con$nodata
   else
      nodata <- NA
   bname <- if (nchar(bandname)) bandname else switch(generic,length="n",generic)
   res <- ursa_new(len=1,bandname=bname,nodata=nodata)
   dimx <- dim(obj$value)
   if (is.null(weight))
      weight <- rep(1,dimx[2])
   weight <- weight/sum(weight)
   if (cover>1)
      cover <- cover/length(obj)
   if (cover==0)
      cover <- 1e-6
   if (verbose)
      .elapsedTime(paste(fun,"start",sep=":"))
   a <- .Cursa("groupSummary"
          ,x=as.numeric(obj$value),dim=as.integer(dimx),cover=as.numeric(cover)
          ,weight=weight,generic=as.integer(code),res=numeric(dimx[1]*1L)
          ,NAOK=TRUE)$res
   if (verbose)
      .elapsedTime(paste(fun,"stop",sep=":"))
   if (is.na(obj$con$posR[1])) {
      res$value[] <- a
      class(res$value) <- "ursaNumeric"
   }
   else
   {
      res$con$posR <- obj$con$posR
      res$value <- a
      class(res$value) <- "ursaNumeric"
      dim(res$value) <- c(dimx[1],1)
   }
   session_grid(g0)
   return(res)
}
