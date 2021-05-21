# TODO Stretch: https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization

##~ 'categorize' <- 'colored' <- function(...) .syn('colorize',0,...)
'colorize' <- function(obj,value=NULL,breakvalue=NULL,name=NULL
                        #,col=NULL ## deprecated
                          ,pal=NULL,inv=NA
                         # ,palname=NULL ## deprecated
                          ,stretch=c("default","linear","equal","mean"
                                    ,"positive","negative","diff","category"
                                    ,"julian","date","time"
                                    ,"slope","conc","sd","significance"
                                    ,"bathy","grayscale","greyscale",".onetoone")
                          ,minvalue=NA,maxvalue=NA,byvalue=NA,ltail=NA,rtail=NA
                          ,tail=NA,ncolor=NA,nbreak=NA,interval=0L,ramp=TRUE
                          ,byte=FALSE,lazyload=FALSE,reset=FALSE
                          ,origin="1970-01-01",format="",alpha=""
                          ,colortable=NULL
                          ,verbose=FALSE,...)
{
   rel <- as.list(match.call())
   fun <- "colorize" #"colorize" ## as.character(rel[1])
   if (!FALSE) { ## ++ 20170629
     # .elapsedTime("---")
     # print(c('as.character(rel[[1]])'=as.character(rel[[1]])))
     # print(c(isPackageInUse=.isPackageInUse()))
     # print(c('rel[[1]]'=rel[[1]]))
     # try(print(c(a=head(names(as.list(args(rel[[1]]))))),quote=FALSE))
     # try(print(c(b=head(names(as.list(args(as.character(rel[[1]])))))),quote=FALSE))
     # try(print(c(c=head(names(as.list(args(colorize))))),quote=FALSE))
     # try(print(c(d=head(names(as.list(args(ursa::colorize))))),quote=FALSE))
      argname <- names(as.list(args(colorize)))
      rname <- names(rel)
      j <- NULL
     # str(rel)
     # print(argname)
      for (i in seq_along(rel)[-1]) {
         if (rname[i]=="obj")
            next
         if ((is.language(rel[[i]]))&&(rname[i] %in% argname)) {
            res <- eval.parent(rel[[i]])
            if (is.null(res))
               j <- c(j,i)
            else if (is.language(res)) {
               res <- eval.parent(res)
               if (!is.language(res)) {
                  assign(rname[i],res)
                  rel[[i]] <- res
               }
               else
                  stop("unable to evaluate agrument ",.sQuote(rname[i]))
            }
            else
               rel[[i]] <- res 
         }
      }
      if (length(j))
         rel <- rel[-j]
     # .elapsedTime("===")
   }
   else if (TRUE)
      rel <- .evaluate(rel,"colorize")
   if (is.numeric(alpha)) {
     ## ?adjustcolor 
      if (all(alpha<=1))
         alpha <- round(alpha*255)
      alpha[alpha<0] <- 0
      alpha[alpha>255] <- 255
      alpha <- sprintf("%02X",alpha)
   }
   col <- NULL ## plug
   palname <- NULL ## plug
   keepColors <- ncolor
   dropIndex <- FALSE
   if (missing(obj)) {
      if (length(value)) {
        # v <- length(value)
         obj <- value
         dropIndex <- TRUE
      }
      else if (length(name)) {
         v <- length(name)
         obj <- seq_len(v)
         dropIndex <- TRUE
      }
      else if (length(breakvalue)) {
         v <- length(breakvalue)+1L
         obj <- seq_len(v)
         dropIndex <- TRUE
      }
      else if ((is.character(pal))&&(length(pal))) {
         v <- length(pal)
         obj <- seq_len(v)
         dropIndex <- TRUE
      }
   }
   isList <-  .is.ursa_stack(obj)
   if (isList) { ## recursive!!!
      res <- vector("list",length(obj))
      oname <- names(obj)
      for (i in seq_along(obj)) {
         rel[["obj"]] <- quote(obj[[i]])
         img <- do.call(fun,rel[-1])
         if ((!is.null(oname))&&(nband(img)==1))
            bandname(img) <- oname[i]
         res[[i]] <- img
         rm(img)
      }
      names(res) <- oname
      ##~ for (i in seq_along(obj))
         ##~ res[[i]] <- colorize(obj[[i]])
      return(res)
   }
   if (!is.ursa(obj)) {
      daily <- integer()
      isTime <- inherits(obj,c("Date","POSIXct","POSIXlt")[2:3])
      isDate <- inherits(obj,c("Date","POSIXct","POSIXlt")[1])
     # print(c(isCharacter=is.character(obj),isTime=isTime,isDate=isDate))
      if (inherits(obj,c("Date","POSIXct","POSIXlt"))) {
         obj <- sort(obj)
         od <- sort(unique(diff(sort(unique(obj)))))
         if (all(od %% min(od) == 0)) {
            os <- seq(min(obj),max(obj),by=min(od))
            daily <- match(obj,os)
         }
        # daily <- integer()
         obj <- if (nchar(format)) format(obj,format) else as.character(obj)
      }
      if ((is.numeric(obj))||(is.character(obj))||(is.factor(obj))) {
         isOrdered <- is.ordered(obj)
         isChar <- is.character(obj) | is.factor(obj)
         g1 <- getOption("ursaSessionGrid")
         ctname <- .grep("colortable",names(rel),value=TRUE)
         if (.is.grid(g1))
            session_grid(NULL)
        # if ((isDate)||(isTime))
        #    obj <- sort(obj)
         if (isOrdered) {
            levname <- levels(obj)
            obj <- as.integer(obj)-1L
         }
         else if (isChar) {
            if (length(daily)) {
               oname <- as.character(os)
               obj <- seq_along(os)
            }
            else {
               oname <- as.character(obj)
               obj <- seq_along(obj)
            }
         }
         res <- ursa_new(matrix(rev(obj),nrow=1),flip=FALSE,permute=FALSE) ## rev()?
         if ((isChar)&&(!isOrdered)) {
            res <- reclass(res,src=seq_along(oname),dst=oname)
            if (length(ctname))
               ursa(res,"value") <- match(oname,names(rel[[ctname]]))-1L
         }
         rel[["obj"]] <- quote(res)
         if (length(ind <- .grep("lazy",names(rel))))
            rel[[ind]] <- FALSE
         else
            rel[["lazy"]] <- FALSE
         if (isTime)
            s <- "time"
         else if (isDate)
            s <- "date"
        # else if (isOrdered)
        #    s <- "category"
         else
            s <- "default"
         if (!length(ind <- .grep("stretch",names(rel))))
            rel[["stretch"]] <- s
         if (isOrdered) {
            if (FALSE) { ## TODO
               levvalue <- .deintervale(levname)
               if ((is.numeric(levvalue))&&(length(levvalue)+1==length(levname)))
                  rel[["breakvalue"]] <- seq_along(levname)-1L
               else
                  rel[["value"]] <- seq_along(levname)-1L
            }
            rel[["name"]] <- levname
         }
         if ((TRUE)&&(length(ctname))) {
           # rel$value <- seq_along(rel[[ctname]])-1L
           # rel$name <- names(rel[[ctname]])
           # rel$pal <- unclass(unname(rel[[ctname]]))
            rel$stretch <- "category"
           # rel[[ctname]] <- NULL
           # print(c(ursa(res,"value")))
           # str(rel)
         }
         img <- do.call(fun,rel[-1]) ## RECURSIVE!
         if (length(ctname)) {
            img$colortable <- rel[[ctname]]
         }
        # if (length(ctname)) {
        #    str(img)
        #    q()
        # }
         if (isOrdered) { ## TODO avoid this double coloring
            levvalue <- .deintervale(levname)
            if ((is.numeric(levvalue))&&(length(levvalue)+1==length(levname))) {
               rel$name <- NULL
               rel[["breakvalue"]] <- levvalue
               rel[["obj"]] <- quote(discolor(.extract(img)))
              # img <- colorize(discolor(.extract(img)),breakvalue=levvalue)
               img <- do.call(fun,rel[-1]) ## RECURSIVE!
            }
         }
         if (.is.grid(g1))
            session_grid(g1)
         else
            session_grid(NULL)
         if (dropIndex)
            return(img$colortable)
         if (!length(daily))
            return(list(index=c(img$value)+1L,colortable=img$colortable))
         return(list(index=daily,colortable=img$colortable))
      }
      return(list(index=NA,colortable=NA))
   }
   if (reset) {
      obj <- .extract(obj)
      ursa_colortable(obj) <- character(0)
      class(obj$value) <- "ursaNumeric"
   }
   if (.is.colortable(obj$colortable)) {
      ct <- obj$colortable
      if (all(!is.na(ct))) {
         if (.is.category(obj)) ## attr(obj$value,"category")
            return(obj)
         else {
            rel$pal <- unclass(unname(ct))
         }
      }
      cname <- .deintervale(ct)
      isCategory <- is.character(cname)
      if (isCategory) {
         ursa_colortable(obj) <- character(0)
         rel$name <- cname
         rel$value <- seq_along(cname)-1L
        # rel$obj <- if (TRUE) quote(obj) else obj
      }
      else {
         if (length(cname)==length(ct)) { ## categoral
           # m1: OK for 'obj <- colorize(x,lazy=TRUE);reclass(obj)'
           # m2: WANTED TEST!
            rel$value <- as.numeric(cname) ## m1, 20170401 removed
           # rel$value <- seq_along(as.numeric(cname))-1L ## m2, 20170401 added
            rel$breakvalue <- NULL
         }
         else { ## interval
           # rel$breakvalue <- as.numeric(cname) ## 20170401 removed
            rel$value <- seq_along(as.numeric(cname))-1L ## 20170401 added
            rel$value <- NULL
         }
        # rel$obj <- if (TRUE) quote(reclass(obj)) else reclass(obj)
      }
      ursa_colortable(obj) <- character(0)
     # rel$obj <- if (!TRUE) quote(.extract(obj)) else .extract(obj)
      rel$obj <- quote(obj)
      if (TRUE) {## simple but not quick 
         if (.is.category(obj)) { ##  attr(obj$value,"category")
            rel$value <- seq(length(ct))-1L
            rel$stretch <- ".onetoone"
         }
         res <- do.call(fun,rel[-1]) ## RECURSIVE
         names(res$colortable) <- names(ct)
      }
      else { ## dev
         if (nchar(palname)) {
            maxcol <- 9L
         }
         if ((is.null(pal))&&(is.null(palname))&&(is.null(col)))
            pal <- c("#FFFFAF","#FFBF3F","#FF6F3F","#9F3F9F","#00007F")
         else if ((length(col))&&(length(col)!=length(rel$value))) {
            pal <- col
         }
         print(pal)
         print(palname)
         print(col)
         q()
      }
      return(res)
   }
   if (.is.colortable(colortable)) {
      ct <- ursa_colortable(colortable)
      val <- .deintervale(ct,verbose=TRUE)
      interval <- as.integer(length(val)!=length(ct))
      isChar <- is.character(val)
      name <- if (isChar) val else NULL
      value <- names(ct)
      pal <- as.character(ct)
      if (isChar)
         val <- seq_along(val)-1L
      arglist <- list(obj=quote(obj),interval=interval,value=val,name=name
                     ,pal=pal,lazyload=lazyload)
      res <- do.call("colorize",arglist) ## recursive!!!
      names(ursa_colortable(res)) <- value
      if (FALSE) {
         ct2 <- ursa_colortable(res)
         print(ct)
         print(ct2)
         print(names(ct))
         print(names(ct2))
         print(all.equal(ct2,ct))
         q()
      }
      return(res)
   }
   stretch <- match.arg(stretch)
   if (length(name)) {
      ncolor <- length(name)
      ramp <- FALSE
     # if (stretch=="default")
     #    stretch <- "category"
   }
   if (stretch!=".onetoone") {
      uniqval <- unique(c(obj$value))
      nuniqval <- length(uniqval)
   }
   if ((is.null(value))&&(!is.null(breakvalue))) {
      value <- breakvalue
      if (!interval)
         interval <- 1L
   }
   if (stretch=="default")
   {
     # if ((nuniqval<=21)&&((is.null(value))||(is.null(breakvalue))))
     # if (((nuniqval<=21)&&(is.null(value)))||(length(name)))
     # if ((nuniqval<=21)&&(is.null(value)))
      if (((nuniqval<=21)&&(is.null(value)))||((length(name))&&(length(name)>nuniqval)))
      {
         ncolor <- nuniqval
         stretch <- "category"
         if ((is.null(palname))&&(is.null(pal))) {
           # palname <- "Paired" ## "random"
            if (dev <- T) {
               arglist <- list(...)
               myname <- names(arglist)
               ind <- .grep("^pal\\.",myname)
               if (length(ind)) {
                  arglist <- arglist[ind]
                  names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
                  arglist <- c(list(value=uniqval),arglist)
                  pal <- do.call("cubehelix",arglist)
               }
               else
                  pal <- cubehelix(value=uniqval)#,rotate=2.5)
            }
            else
               pal <- cubehelix(value=uniqval)#,rotate=2.5)
            inv <- FALSE
         }
      }
      else if (storage.mode(obj$value)=="integer") {
         val <- c(obj$value)
         if ((is.null(palname))&&(is.null(pal))) {
            if ((min(val,na.rm=TRUE)<50)&&
               ((max(val,na.rm=TRUE)>200) && (max(val,na.rm=TRUE)<=255))) {
               palname <- "Greys"
               if (is.na(inv))
                  inv <- TRUE
            }
         }
      }
   }
   if ((is.na(ncolor))&&(!is.na(nbreak))) { ## added 2015-11-18
      ncolor <- nbreak+1L
      if (!interval)
         interval <- 1L
   }
   if ((is.null(value))&&(!is.na(ncolor))&&(interval %in% c(1L))) 
      ncolor <- ncolor-1L ## added 2015-11-17
  # print(c(nbreak=nbreak,ncolor=ncolor,keepColors=keepColors))
   if (!is.na(ncolor)) ## added 20170103
      ramp <- FALSE
   if (is.na(ncolor))
   {
      if ((FALSE)&&(length(col))&&(!is.na(col)[1]))
         ncolor <- length(col)
      else if (ramp)
         ncolor <- 255L
      else {
         if (!(stretch %in% c("bathy")))
            ncolor <- 11L
         if ((interval %in% c(1L))) { # && stretch %in% c("diff","slope"))
            ncolor <- 10L
         }
      }
   }
   if ((length(value)>0)&&(length(name)>0)) {
      if ((interval==0)&&(length(value)!=length(name)))
         name <- NULL
      else if (interval>0) {
         if ((interval==1)&&(length(name) %in% (length(value)+c(0)))) {
            n0 <- name
            if (length(n0)>1)
               name <- paste0("(",n0[-length(n0)],";",n0[-1],"]")
            name <- c(paste0("<= ",n0[1]),name,paste0("> ",n0[length(n0)]))
         }
         if ((interval>0)&&((length(value)+1)!=length(name))) {
            name <- NULL
         }
      }
   }
   if (is.null(palname)) {
      if (FALSE) ## 20170105 test for disabling RColorBrewer
         palname <- switch(stretch,slope="RdBu",diff="RdBu",sd="YlGnBu"
                                  ,linear="cubehelix",equal="cubehelix"
                                  ,mean="Spectral",conc="Blues"
                                  ,positive="Oranges",negative="Purples"
                                  ,rainbow=".rainbow",category="Paired" ##"random"
                                  ,julian="YlGn"
                                  ,significance=c("RdBu","PuOr")[2],aspect=".rainbow"
                                  ,grayscale="Greys",greyscale="Greys"
                          ,c("cubehelix","RdYlBu")[1]) ## default "RdYlBu"
      else { ## rownames(RColorBrewer::brewer.pal.info)
         if ((is.character(pal))&&(length(pal)==1)&&(pal %in% c("dummycolornamewhichisabsent"
                        ,"BrBG","PiYG","PRGn","PuOr","RdBu","RdGy"    
                        ,"RdYlBu","RdYlGn","Spectral","Accent","Dark2","Paired"  
                        ,"Pastel1","Pastel2","Set1","Set2","Set3","Blues"   
                        ,"BuGn","BuPu","GnBu","Greens","Greys","Oranges" 
                        ,"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu"    
                        ,"Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")))
            palname <- pal
         else
            palname <- "cubehelix"
      }
   }
   if (stretch=="conc")
   {
      if ((!is.na(ignorevalue(obj)))&&(ignorevalue(obj)==0))
      {
         minvalue <- 1
         maxvalue <- 101
      }
      else
      {
         minvalue <- 0
         maxvalue <- 100
      }
      if (is.na(inv))
         inv <- TRUE
   }
   else if (stretch=="sd")
   {
      if (is.na(inv))
         inv <- FALSE
      stretch <- "positive"
   }
   else if (stretch=="positive") {
      if (is.na(inv))
         inv <- FALSE
   }
   else if (stretch=="negative") {
      if (is.na(inv))
         inv <- TRUE
   }
   else if (stretch=="significance")
   {
      value <- c(-0.999,-0.99,-0.95,-0.9,-0.5,0.5,0.9,0.95,0.99,0.999)
      interval <- TRUE
   }
   else if (stretch=="bathy")
   {
      palname <- "Blues"
      inv <- FALSE
      interval <- 1L
      if (is.na(ncolor))
         ncolor <- 21L
      stretch <- "equal"
   }
   if (FALSE) {
      if (((stretch=="default")&&(is.null(pal))&&(is.null(col)))||
          (palname %in% c("zzz.internal"))) {
         if (palname=="make.your.own.here")
            pal <- c("#FFFFAF","#FFBF3F","#FF6F3F","#9F3F9F","#00007F")
         else if (palname=="viridis")
            pal <- c("#FFFFAF","#FFBF3F","#FF6F3F","#9F3F9F","#00007F")
         else if (palname=="cubehelix") {
            pal <- cubehelix
         }
      }
   }
   isCharOneToOne <- is.character(.deintervale(name))
   if ((is.null(pal))&&(stretch %in% c("default"))) {
      if (palname %in% c("cubehelix")) {
         if (TRUE) {
            pal <- cubehelix
         }
         else {
            if (isCharOneToOne) {
               if (length(unique(name))<12) {
                  pal <- cubehelix(11)
               }
               else {
                  pal <- cubehelix(11,rotate=sample(c(-1,1),1)*2.5*360)
                  pal <- sample(pal)
               }
            }
            else if (FALSE)
               palname <- "Paired"
            else {
               pal <- cubehelix
            }
         }
      }
      else if (palname=="viridis")
         pal <- c("#FFFFAF","#FFBF3F","#FF6F3F","#9F3F9F","#00007F")
   }
   if ((FALSE)&&(stretch==".onetoone")) { ## disable 20170112
      if (isCharOneToOne) {
         palname <- "Paired"
        # pal <- cubehelix
      }
   }
   maxcol <- 9L
   if (!is.null(pal)) {
      if (is.na(inv))
         inv <- FALSE
   }
   else if (palname %in% c("Spectral","RdYlGn","RdYlBu","RdGy","RdBu"
                      ,"PuOr","PRGn","PiYG","BrBG"))
   {
      if (is.na(inv))
         inv <- TRUE
      maxcol <- ifelse(stretch=="slope" & palname!="Spectral",9L,11L)
   }
   else {
      if ((is.na(inv))&&(palname=="Greys"))
         inv <- TRUE
      else if (is.na(inv))
         inv <- FALSE
      if (palname %in% c("Set2","Pastel2","Dark2","Accent"))
         maxcol <- 8L
      else if (palname %in% c("Set3","Paired"))
         maxcol <- 12L
      else if (palname %in% c(".rainbow"))
         stop("HERE")
   }
   ##~ pal <- colorRampPalette(brewer.pal(maxcol,palname))
   ##~ pal <- rgb(runif(maxcol),runif(maxcol),runif(maxcol)))
   ##~ pal <- sample(colorspace::rainbow_hcl(maxcol,l=75,c=80))
   if ((FALSE)&&(!is.null(value))) ## added 20160127
      value <- as.numeric(format(value,trim=TRUE))
   if (stretch=="category")
   {
      arglist <- list(...)
      if (("forceLabel" %in% names(arglist))&&((arglist[["forceLabel"]])))
         ncolor <- 999L
      interval <- 0L
      if (storage.mode(obj$value)=="double") {
         obj$value <- round(obj$value,11)
         class(obj$value) <- "ursaNumeric"
      }
      val <- c(obj$value)
      if (!is.null(value)) {
         v2 <- value
         toSort <- FALSE
         n0 <- length(v2)
      }
      else {
         v1 <- c(na.omit(sort(unique(val))))
         toSort <- length(v1)>ncolor
         if (toSort)
            v1 <- rev(v1)
         n0 <- ifelse(!toSort,length(v1),ncolor-1)
         v2 <- if (toSort) rev(v1[seq(n0)]) else v1[seq(n0)]
      }
      val[!is.na(val)] <- 0
      for (i in seq_along(v2))
         val[obj$value==v2[i]] <- i-!toSort#-other
      obj$value[] <- val
      storage.mode(obj$value) <- "integer"
      ignorevalue(obj) <- n0+toSort
      ok <- FALSE
      if (length(v2)>1) {
        # print(v2,digits=12)
         for (i2 in -6:+6) ## -4:+4
         {
            v3 <- unique(round(v2*10^i2))
            if (length(v3)==length(v2))
            {
               if ((i2==0)&&(!all((v2-floor(v2)) %in% c(0,1))))
                  next
               if (all(v3 %in% c(0,1))) ## added 20150815 (fail for 0.0500001 rounding)
                  next
              # if ((i2<(9))&&(any(abs(v3-v2*(10^i2))>0.499)))
              #    next
               if (TRUE) { ## 20190306 ++
                  v4 <- na.omit(v3*10^(-i2)/v2)
                  if ((length(v4))&&(any(abs(v4-1)>0.101)))
                     next
               }
               ok <- TRUE
               break
            }
         }
      }
      else if (length(v2)<(+2)) {
         iv2 <- try(as.integer(v2))
         if ((is.integer(v2))&&(!is.na(v2))&&(abs(v2-iv2)>.Machine$double.eps)) {
            .v2 <- pretty(v2*(1+c(-1,0,1)*1e-3),n=2)
            v2 <- .v2[which.min(abs(.v2-v2))[1]]
         }
      }
      label2 <- as.character(v2)
      if (!ok)
         label <- label2
      else
      {
         if (i2<(0)) {
           # cat("--- RR2 begin\n")
           # print(i2)
           # print(v2)
           # options(warn=-10)
           # label <- sprintf("%%f",as.double(round(v2,i2+1)))
           # label <- sprintf(sprintf("%%.0f",i2),as.double(round(v2,i2+1)))
           # label <- sprintf(sprintf("%%.0f",i2),as.double(v2)) ## --20210520
            label <- sprintf("%.0f",as.double(v2)) ## ++20210520 or
           # label <- sprintf("%.0f",as.double(round(v2,i2+1))) ## ++20210520 or
           # label <- sapply(as.double(v2),function(v9) sprintf(sprintf("%%.0f",i2),v9)) ##++
           # label <- format(v2,trim=TRUE,scientific=FALSE)
           # label <- label2
           # i2 <- 0
           # cat("--- RR2 end\n")
         }
         else {
           # print(i2)
            label1 <- sprintf(sprintf("%%.%df",i2),as.double(v2))
            label <- if (max(nchar(label2))+0==max(nchar(label1))) label2 else label1
         }
         hasZero <- any(v2==0)
         while((length(label)!=length(unique(label)))||
               ((!hasZero)&&(any(as.numeric(label)==0))))
         {
            i2 <- i2+1
           # message(paste("i2 chanded from",i2-1,"to",i2))
            if (i2<0)
               next
            else
               label <- sprintf(sprintf("%%.%df",i2),as.double(v2))
         }
         sl <- unique(substr(label,nchar(label)-1,nchar(label)))
         if ((length(sl)==1)&&(sl==".0")) {
            label <- sprintf(sprintf("%%.0f",i2),as.double(label))
         }
         dv <- diff(sort(v2))
         if (length(dv))
            dv <- dv/min(dv)
         v3 <- abs(v2)
         v3 <- v3[v3!=0]
         if ((length(dv)==1)&&(.is.integer(v2[2]/v2[1]))) {
            label <- label3 <- as.character(as.numeric(format(v2,trim=TRUE,scientific=FALSE)))
         }
         else if ((length(dv)>1)&&((.is.integer(dv))||(.is.integer(v3/min(v3)))))
            label <- label3 <- format(v2,trim=TRUE,scientific=FALSE)
         else {
            for (i2 in 1:12) {
               label3 <- format(v2,trim=TRUE,scientific=FALSE,digits=1,nsmall=i2)
               if (length(label3)==length(v2))
                  break
            }
         }
      }
      if (FALSE) {
         message("---------")
         print(i2)
         print(.is.integer(dv))
         print(label2,quote=FALSE)
         message("|")
         print(label3,quote=FALSE)
         message("|")
         print(label,quote=FALSE)
         message("=========")
      }
      if (is.null(name))
         name <- if (toSort) c(paste0("/",length(v1)-ncolor,"/"),label)
                 else as.character(label)
      n <- length(name)
      if (is.null(col))
      {
         if ((palname=="random")||(is.null(pal)))
         {
            ##~ if ((palname=="Paired")&&(n<maxcol))
               ##~ col <- colorRampPalette(sample(brewer.pal(maxcol,palname)))(n)
            ##~ else if (palname!="Paired")
               ##~ col <- colorRampPalette(brewer.pal(maxcol,palname))(n)
            if (palname %in% c("cubehelix","random")) {
               pal <- "cubehelix"
               arglist <- list(...)
               myname <- names(arglist)
               ind <- .grep("^pal\\.",myname)
               if (length(ind)) {
                  arglist <- arglist[ind]
                  names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
                  arglist <- c(n=n,arglist)
                  col <- do.call(pal,arglist)
               }
               else
                  col <- do.call(pal,list(n=n))#,dark=191,light=191))
               if (palname=="random")
                  col <- sample(col)
            }
            else {
               message("BREWER A -- not-reproduced ")
               stop("Unexpected branch A which is required deprecated 'RColorBrewer'")
               ##~ requireNamespace("RColorBrewer",quietly=.isPackageInUse())
               ##~ if (n<maxcol) {
                  ##~ col <- colorRampPalette(sample(
                                     ##~ RColorBrewer::brewer.pal(maxcol,palname)))(n)
               ##~ }
               ##~ else
               ##~ {
                  ##~ c1 <- RColorBrewer::brewer.pal(maxcol,palname)
                  ##~ c2 <- NULL
                  ##~ k <- rep(ceiling(2*n/maxcol),maxcol/2)
                  ##~ if (sum(k)>n)
                  ##~ {
                     ##~ ind <- sample(seq(length(k)),sum(k)-n)
                     ##~ k[ind] <- k[ind]-1L
                  ##~ }
                  ##~ j <- seq(1,maxcol-1,by=2)
                  ##~ for (i in seq_along(j))
                  ##~ {
                     ##~ i2 <- j[i]
                     ##~ c2 <- c(c2,colorRampPalette(c1[c(i2+1,i2)])(k[i]))
                  ##~ }
                  ##~ col <- sample(c2)
               ##~ }
            }
         }
         else {
            if (is.function(pal)) {
               arglist <- list(...)
               myname <- names(arglist)
               ind <- .grep("^pal\\.",myname)
               if (length(ind)) {
                  arglist <- arglist[ind]
                  names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
                  arglist <- c(n=n,arglist)
                  col <- do.call(pal,arglist)
               }
               else
                  col <- pal(n)
            }
            else if (is.character(pal)) {
               if ((length(pal)==1)&&(!.lgrep("^#[0-9A-F]+$",pal))&&
                   (requireNamespace("RColorBrewer",quietly=.isPackageInUse()))) {
                  available <- RColorBrewer::brewer.pal.info
                  if (!is.na(ind <- match(pal,rownames(available)))) {
                     pal <- RColorBrewer::brewer.pal(available$maxcolors[ind],pal)
                     if (available$category[ind]=="qual")
                        pal <- sample(pal)
                  }
                  else if (pal %in% available$category) {
                     selected <- available[sample(which(available$category==pal),1),]
                     pal <- RColorBrewer::brewer.pal(selected$maxcolors,rownames(selected))
                     if (rownames(selected)=="qual")
                        pal <- sample(pal)
                  }
               }
              ## 20180316 added alpha=TRUE
               col <- colorRampPalette(unlist(strsplit(pal,split="\\s+")),alpha=TRUE)(n)
            }
            else
               stop("Unable interpret 'pal' argument")
         }
         if (inv)
            col <- rev(col)
      }
      obj <- .as.colortable(obj,col=col,name=name,alpha=alpha)
      class(obj$value) <- "ursaCategory"
      return(obj)
   }
  # labels <- ncolor ## (TODO: keep desired labels if ramp=TRUE)
   if ((FALSE)&&(ramp))# &&(!is.na(keepColors))?
   {
     # interval <- 0L
      if (ncolor<21)
         ncolor <- 255
   }
   if (stretch=="equal")
   {
      val <- sort(c(na.omit(c(obj$value))))
      for (mycol in seq(ncolor,11*(ncolor-1)+1,by=2))
      {
         if (interval %in% c(0L))
            th <- seq(0,1,length=mycol*2+1)[seq(mycol)*2]
         else if (interval %in% c(1L,2L))
         {
            th <- seq(0,1,length=mycol+2)
            th <- th[-c(1,length(th))]
         }
         value <- unique(val[round(length(val)*th)])
         if (length(value) >= ncolor) {
            break
         }
      }
      ok <- FALSE
      for (i2 in -6:+6) ## -4:+4
      {
         v <- unique(round(value*10^i2))
         if ((length(value)==1)&&(v==0))
            next
         if (length(v)==length(value))
         {
            if ((i2==0)&&(!all((value-floor(value)) %in% c(0,1))))
               next
            ok <- TRUE
            break
         }
      }
     # ok <- FALSE
      if (!ok)
      {
         event <- FALSE
         for (i2 in 1:3)
         {
            label <- format(value,trim=TRUE,digits=i2)
            if (length(unique(label))==length(value)) {
               event <- TRUE ## added 20190619
               break
            }
         }
         if (!event) {
            label <- unique(label)
            value <- as.numeric(label)
         }
      }
      else
      {
        # i2 <- i2+1 ## 
         if (i2<0) {
            label <- sprintf(sprintf("%%f",i2),as.double(round(value,i2+1)))
            i2 <- 0
           # label <- sprintf(sprintf("%%f",i2),as.double(value))
         }
         else
            label <- sprintf(sprintf("%%.%df",i2),as.double(value))
         while(length(label)!=length(unique(label)))
         {
            i2 <- i2+1
            label <- sprintf(sprintf("%%.%df",i2),as.double(value))
         }
         sl <- unique(substr(label,nchar(label)-1,nchar(label)))
         if ((length(sl)==1)&&(sl==".0"))
            label <- sprintf(sprintf("%%.0f",i2),as.double(value))
         value <- as.numeric(label)
      }
      names(value) <- label
      rm(val,th,label)
   }
   if (!length(value))
   {
     # ncol <- 11L ## desirable number of colors
      val <- sort(na.omit(c(obj$value)))
      if (!is.na(byvalue)) {
         minb <- floor(min(val)/byvalue)*byvalue
         maxb <- ceiling(max(val)/byvalue)*byvalue
         ncolor <- (maxb-minb)/byvalue
         rm(minb,maxb)
      }
      if (any(is.na(tail))) {
         tail <- 0.11/ncolor
        # print(c(colors=ncolor,tail=tail))
      }
      tail <- rep(tail,2)
      if (!is.na(ltail))
         tail[1] <- ltail
      if (!is.na(rtail))
         tail[2] <- rtail
      if ((stretch %in% c("positive","negative"))&&(interval %in% c(-1L)))
         ncolor <- ncolor+1L
      n <- length(val)
      lo <- floor(n*tail[1])
      hi <- n-floor(n*tail[2])+1
      if (T) { ## ++ 20210111
         lo <- lo+1
         hi <- hi-1
      }
      if ((lo<=1)||(hi>n)) {
         rngv <- (c(lo,hi)-1)*(max(val)-min(val))/(n-1)+min(val)
      }
      minv <- if (lo>0) val[lo] else if (n) rngv[1] else 0
      maxv <- if (hi<=n) val[hi] else if (n) rngv[2] else 1
      if (verbose)
         print(data.frame(n=n,lo=lo,hi=hi,minv=minv,maxv=maxv,min=min(val),max=max(val)))
      if (stretch %in% c("diff","slope"))
      {
         extv <- max(abs(minv),abs(maxv))
         minv <- -extv
         maxv <- extv
         rm(extv)
      }
      if (is.na(minvalue))
         minvalue <- minv
      if (is.na(maxvalue))
         maxvalue <- maxv
      if (!is.na(byvalue)) {
         minb <- floor(minvalue/byvalue)*byvalue
         maxb <- ceiling(maxvalue/byvalue)*byvalue
         ncolor <- (maxb-minb)/byvalue
      }
      rm(n,lo,hi,minv,maxv,val)
      if (verbose)
         print(data.frame(stretch=stretch,min=minvalue,max=maxvalue))
      byv <- ifelse(interval %in% c(1L),0.75,0.25)*(maxvalue-minvalue)/ncolor
      minvalue <- minvalue+byv
      maxvalue <- maxvalue-byv
      if (stretch=="positive")
         minvalue <- 0
      else if (stretch=="negative")
         maxvalue <- 0
      if (maxvalue<minvalue) {
         .m <- c(maxvalue,minvalue)
         minvalue <- .m[1]
         maxvalue <- .m[2]
         rm(.m)
      }
      if (verbose)
         print(data.frame(colors=ncolor,min=minvalue,max=maxvalue))
     # str(ncolor)
      if (interval %in% c(2L,3L)) {
         ncolor <- (ncolor-1)*2+1
      }
     # str(ncolor)
     # q()
      if (!is.na(byvalue)) {
         value <- seq(minb,maxb,by=byvalue)
         rm(minb,maxb)
         names(value) <- value
      }
      else {
         if ((ramp)||(ncolor>51)) {
            if (stretch %in% c("julian")) {
               minvalue <- floor(minvalue)
               maxvalue <- ceiling(maxvalue)
               ncolor <- as.integer(maxvalue-minvalue+1)
               mc <- seq(minvalue,maxvalue,by=1)
            }
            else {
               if ((byte)&&(is.na(keepColors)))
                  keepColors <- 255L
              # if (ncolor>nuniqval)
              #    ncolor <- nuniqval
               for (i in 1:101) {
                  mc <- pretty(c(minvalue,maxvalue),n=ncolor,min.n=ncolor%/%1.5)
                 # print(c(i=i,colors=ncolor,pretty=length(mc)))
                  if (is.na(keepColors))
                     break
                  if (length(mc)<=keepColors)
                     break
                  ncolor <- ncolor-1
                  if (ncolor<2)
                     break
               }
            }
            lab <- format(mc,trim=TRUE)
            if (length(unique(lab))==-1) {
               for (dig in seq(16)) {
                  lab <- format(mc,trim=TRUE,digits=dig)
                  if (length(unique(lab))>1)
                     break
               }
               mc <- as.numeric(lab)
            }
            else if (length(unique(lab))<length(mc)) {
               for (dig in seq(16)) {
                  lab <- unique(format(mc,trim=TRUE,digits=dig))
                  if (length(lab)>1)
                     break
               }
               mc <- as.numeric(lab)
               ncolor <- length(mc)
            }
            mc <- data.frame(at=mc,lab=lab,stringsAsFactors=FALSE)
            if (FALSE)
               cat(sprintf("colors(ramp)=%d\n",nrow(mc)))
         }
         else {
            if (ncolor==1) {
               mc <- .prettyLabel(c(minvalue,maxvalue),ncol=3,onPanel=TRUE)
               if (nrow(mc)==3) {
                  mc <- mc[2,]
               }
               else
                  mc <- .prettyLabel(c(minvalue,maxvalue),ncol=ncolor,onPanel=TRUE)
            }
            else
               mc <- .prettyLabel(c(minvalue,maxvalue),ncol=ncolor,onPanel=TRUE)
            if (nrow(mc)==0) {
               mc <- pretty(c(minvalue,maxvalue),n=ncolor,min.n=ncolor%/%1.5)
               mc <- data.frame(at=mc,lab=format(mc,trim=TRUE),stringsAsFactors=FALSE)
            }
         }
         if (interval %in% c(2L))
         {
            ind <- which(abs(mc$at)<1e-15)
            if ((!length(ind))||(ind%%2==0))
               mc <- mc[seq(1,ncolor,by=2),]
            else
               mc <- mc[seq(2,ncolor,by=2),]
           # mc <- subset(mc,!is.na(at))
            mc <- mc[which(!is.na(mc$at)),]
            ncolor <- (ncolor-1)/2+1
         }
         if ((stretch %in% c("positive","negative"))&&(interval))
         {
            if (length(ind <- which(mc$at==0.0)))
               mc <- mc[-ind,]
         }
         if (verbose)
            print(mc)
         value <- mc$at
         if (stretch=="date")
            mc$lab <- as.Date(mc$at,origin=origin)
         else if (stretch=="time")
            mc$lab <- as.POSIXct(mc$at,origin=origin)
         if ((nchar(format))&&(stretch %in% c("date","time","julian"))) {
            if (stretch %in% "julian")
               mc$lab <- as.Date(paste0("2018",mc$lab),format="%Y%j")
            mc$lab <- format(mc$lab,format)
         }
         names(value) <- mc$lab
      }
   }
   if (!interval)
      ivalue <- c(-Inf,value[-1]-diff(value)/2,+Inf)
   else
      ivalue <- c(-Inf,value,+Inf)
   ivalue <- unname(ivalue)
   if (verbose)
      print(ivalue)
   if ((!lazyload)&&(stretch!=".onetoone")) {
      val <- c(obj$value)
      if (verbose)
         .elapsedTime("reclass:start")
      if (FALSE)
      {
         for (i in seq_along(ivalue)[-1])
         {
            if (verbose)
               print(c(i-2,ivalue[i-1],ivalue[i]))
            val[obj$value>ivalue[i-1] & obj$value<=ivalue[i]] <- i-2
         }
      }
      else if (FALSE)
      {
         ind <- is.na(val)
         if (!all(ind))
         {
            nodata <- ceiling(max(val,na.rm=TRUE))+1
            val[ind] <- nodata
            ivalue[ivalue==-Inf] <- -1e38
            ivalue[ivalue==Inf] <- +1e38
            val <- .Cursa("reclassify",src=as.numeric(val),n=as.integer(length(val))
                     ,nodata=as.numeric(nodata)
                     ,class=as.numeric(ivalue),nclass=length(ivalue)
                     ,dst=integer(length(val)),NAOK=TRUE)$dst
            val[ind] <- NA
         }
      }
      else ## less memory usage
      {
         ind <- which(!is.na(val))
         if (length(ind))
         {
            nodata <- ceiling(max(val,na.rm=TRUE))+1
            ivalue[ivalue==-Inf] <- -1e38
            ivalue[ivalue==Inf] <- +1e38
            val[ind] <- .Cursa("reclassify",src=as.numeric(val[ind])
                     ,n=as.integer(length(ind))
                     ,nodata=as.numeric(nodata)
                     ,class=as.numeric(ivalue),nclass=length(ivalue)
                     ,dst=integer(length(ind)),NAOK=!FALSE)$dst
         }
      }
      if (verbose)
         .elapsedTime("reclass:finish")
      obj$value[] <- val
      storage.mode(obj$value) <- "integer"
   }
   if (!length(name))
   {
      if (stretch=="zzzsignificance")
         n0 <- as.character(abs(value))
      else if (stretch %in% c("julian")) {
         repeat({
            if (!length(ind <- which(value>365)))
               break
            value[ind] <- value[ind]-365
         })
         n0 <- format(as.Date(sprintf("2019%03d",as.integer(value)),"%Y%j")
                     ,ifelse(nchar(format),format,"%d%b"))
      }
      else if (stretch=="zzzdate") {
         n0 <- format(as.Date(value,origin=origin)
                     ,ifelse(!nchar(format),"%Y-%m-%d",format))
      }
      else if (!is.null(names(value)))
         n0 <- names(value)
      else if (inherits(value,"Date")) {
         n0 <- format(value,ifelse(!nchar(format),"%Y-%m-%d",format))
      }
      else {
         if (stretch %in% c("category","equal"))
            n0 <- as.character(value)
         else {
            n0 <- format(value,trim=TRUE)
         }
      }
      ##~ print(n0)
      ##~ print(col)
      ##~ print(length(n0))
      ##~ print(length(col))
      if (interval)
      {
        # if (minval<min(value))
        #    value <- c(minval,value)
        # if (maxval>max(value))
        #    value <- c(value,maxval)
         if (length(n0)>1)
            name <- paste0("(",n0[-length(n0)],";",n0[-1],"]")
         name <- c(paste0("<= ",n0[1]),name,paste0("> ",n0[length(n0)]))
      }
      else
         name <- n0
   }
   if ((verbose)&&(!lazyload))
   {
      vt <- table(c(obj$value))
      names(vt) <- name[as.integer(names(vt))+1]
      print(vt)
   }
   n <- length(name)
   if (!is.null(col))
      inv <- FALSE
   else
   {
      if (palname=="random") {
         arglist <- list(...)
         myname <- names(arglist)
         ind <- .grep("^pal\\.",myname)
         if (length(ind)) {
            arglist <- arglist[ind]
            names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
            arglist <- c(list(n=n,value=unname(value)),arglist)
            col <- do.call("cubehelix",arglist)
         }
         else
            col <- cubehelix(n,value=unname(value))
         col <- sample(col)
      }
      else if (is.null(pal)) {
         if ((stretch==".onetoone")&&(isCharOneToOne)) {
            if (FALSE) {
               message("BREWER C -- fixed")
               ##~ col <- RColorBrewer::brewer.pal(maxcol,palname)
               ##~ if (n<maxcol)
                  ##~ col <- sample(col,n)
               ##~ else
                  ##~ col <- colorRampPalette(sample(col))(n)
            }
            else {
               arglist <- list(...)
               myname <- names(arglist)
               ind <- .grep("^pal\\.",myname)
               if (length(ind)) {
                  arglist <- arglist[ind]
                  names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
                  arglist <- c(list(n=n,value=unname(value)),arglist)
                  col <- do.call("cubehelix",arglist)
               }
               else
                  col <- cubehelix(n,rotate=sample(c(-360,360),1)*
                                       runif(1,min=0.8,max=1.2)*(n/11)^0.75)
            }
         }
         else {
            arglist <- list(...)
            myname <- names(arglist)
            ind <- .grep("^pal\\.",myname)
            if (length(ind)) {
               arglist <- arglist[ind]
               names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
            }
            else
               arglist <- list()
            arglist <- c(list(n=n,value=unname(value)),arglist)
            if (palname=="cubehelix") {
               if (TRUE) {
                  col <- do.call("cubehelix",arglist)
               }
               else {
                  arglist <- list(...)
                  myname <- names(arglist)
                  ind <- .grep("^pal\\.",myname)
                  if (length(ind)) {
                     arglist <- arglist[ind]
                     names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
                     arglist <- c(list(n=n,value=unname(value)),arglist)
                     col <- do.call("cubehelix",arglist)
                  }
                  else
                     col <- cubehelix(n,value=unname(value))
               }
            }
            else {
               if (FALSE) {
                  message("BREWER B -- fixed")
                  ##~ requireNamespace("RColorBrewer",quietly=.isPackageInUse())
                  ##~ col <- colorRampPalette(RColorBrewer::brewer.pal(maxcol,palname))(n)
               }
               else {
                  if (palname=="Greys") {
                     arglist$hue <- 0
                  }
                  if (palname=="Blues") {
                     arglist$rotate <- 45
                     arglist$hue <- 1.5
                  }
                  col <- do.call("cubehelix",arglist)
               }
            }
         }
      }
      else {
         if (is.function(pal)) {
            arglist <- list(...)
            myname <- names(arglist)
            ind <- .grep("^pal\\.",myname)
            if (length(ind)) {
               arglist <- arglist[ind]
               names(arglist) <- .gsub("(^pal\\.)","",myname[ind])
               fname <- names(formals(pal))
               if ("value" %in% names(formals(pal)))
                  arglist <- c(list(n=n,value=unname(value)),arglist)
               else
                  arglist <- c(list(n=n),arglist)
               col <- do.call(pal,arglist)
            }
            else {
               argname <- names(as.list(args(pal)))
               if ("value" %in% argname) {
                  col <- pal(n,value=unname(value))
               }
               else {
                  col <- pal(n)
               }
            }
         }
         else if (is.character(pal)) {
            if ((length(pal)==1)&&
                (requireNamespace("RColorBrewer",quietly=.isPackageInUse()))) {
               available <- RColorBrewer::brewer.pal.info
               if (!is.na(ind <- match(pal,rownames(available)))) {
                  pal <- RColorBrewer::brewer.pal(available$maxcolors[ind],pal)
                  if (available$category[ind]=="qual")
                     pal <- sample(pal)
               }
               else if (pal %in% available$category) {
                  selected <- available[sample(which(available$category==pal),1),]
                  pal <- RColorBrewer::brewer.pal(selected$maxcolors,rownames(selected))
                  if (rownames(selected)=="qual")
                     pal <- sample(pal)
               }
            }
            col <- colorRampPalette(unlist(strsplit(pal,split="\\s+"))
                                   ,alpha=TRUE)(n)
         }
         else
            stop("Unable interpret 'pal' argument")
      }
      if (inv)
         col <- rev(col)
   }
   obj <- .as.colortable(obj,col=col,name=name,alpha=alpha)
   class(obj$value) <- ifelse(lazyload,"ursaNumeric","ursaCategory")
   if (!lazyload)
      ignorevalue(obj) <- n
   else if (is.na(ignorevalue(obj)))
      ignorevalue(obj) <- .optimal.nodata(ursa_value(obj))
   obj
}
'.as.colortable' <- function(x,value=NULL,name=NULL,blank="",col=NULL
                            ,alpha="",colored=FALSE)
{
  # str(list(value=value,name=name,col=col,colored=colored))
   if (!is.ursa(x))
      return(NULL)
   if ((length(value)>0)&&(length(value)==length(name)))
      val <- value
   else if (colored)
      val <- seq(0,max(x$value,na.rm=TRUE),by=1)
   else if (!length(col))
      val <- sort(unique(c(x$value)))
   else
      val <- seq(col)-1L
   n <- length(val)
   if (length(name)==length(val))
      cname <- name
   else
      cname <- paste0(blank,val)
   if (length(col)==n)
      val <- col
   else if (colored)
      val <- grDevices::rgb(runif(n),runif(n),runif(n))
   val <- t(grDevices::col2rgb(val,alpha=TRUE))
   if (any(!is.na(val)))
      val <- grDevices::rgb(val[,1],val[,2],val[,3],val[,4],max=255)
   
   if (nchar(alpha)==2) ## if (toupper(alpha)!="")
      val <- paste0(substr(val,1,7),toupper(alpha))
  # val <- paste0(val,alpha)
   names(val) <- cname
   class(val) <- "ursaColorTable"
   x$colortable <- val
   x
}
