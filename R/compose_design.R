'compose_design' <- function(...) {
   arglist <- list(...)
  # str(lapply(arglist,function(x) list(class=class(x),names=names(x))))
   obj <- .getPrm(arglist,name="^$",default=NULL
                 ,class=list(c("list","ursaRaster"),"ursaRaster","integer"))
   layout <- .getPrm(arglist,name="layout",default=NA_integer_)
  # if (identical(obj,layout))
  #    obj <- NULL
   byrow <- .getPrm(arglist,name="byrow",default=TRUE)
   skip <- .getPrm(arglist,name="skip",class="integer",default=NULL)
   legend <- .getPrm(arglist,name="legend",class="",default=NA)
   side <- .getPrm(arglist,name="side",default=NA_integer_,valid=1L:4L)
   ratio <- .getPrm(arglist,name="ratio",default=(16+0.05)/(9+0.05))
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
   .compose_design(obj=obj,layout=layout,byrow=byrow,skip=skip,legend=legend
                  ,side=side,ratio=ratio,verbose=verbose)
}
'.compose_design' <- function(obj=NULL,layout=NA,byrow=TRUE,skip=NULL,legend=NA
                             ,side=NA,ratio=(16+1)/(9+1),verbose=FALSE) {
   if (verbose)
      str(list(obj=if (is.list(obj)) sapply(obj,class) else class(obj)
              ,layout=layout,byrow=byrow,skip=skip,legend=legend
              ,side=side,ratio=ratio))
   if (is.null(legend)) {
      forcedLegend <- FALSE
      legend <- NA
   }
   else
      forcedLegend <- TRUE
   if (!length(layout))
      layout <- NA
   if ((is.numeric(side))&&(length(side)==1)&&(side %in% c(1:4)))
      legend <- side
   layout[!is.finite(layout)] <- NA
   oldM <- layout
  # print(class(obj))
   ##~ if ((length(layout)==1)&&(is.numeric(layout))) {
      ##~ print("0114 D")
      ##~ nb <- layout
      ##~ layout <- NA
   ##~ }
   if (is.null(obj)) ## || (missing(obj))
   {
     # print(class(obj))
     # stop("NULL")
      if (any(is.na(layout)))
         layout <- c(1L,1L)
      panelr <- layout[1]
      panelc <- layout[2]
      isList <- FALSE
   }
   else
   {
     # session_grid(obj) ## added 2015-12-02 # removed 20161226
      if (is.null(g0 <- getOption("ursaSessionGrid"))) ## added 20161226
         session_grid(obj)
      g1 <- session_grid()
      isList <- .is.ursa_stack(obj)
      if (isList)
      {
        # if ((all(is.na(layout)))&&(is.na(legend))) {
        #   # message("calling internal function '.compose_design.stack()'")
        #   # return(.compose_design.stack(obj))
        # }
         fld <- NULL
         for (i in seq_along(obj)) {
            isF <- is.ursa(obj[[i]])
            if (!isF)
               fld <- c(fld,0L)
            else
               fld <- c(fld,rep(i,nband(obj[[i]])))
         }
      }
      else {
         if (is.integer(obj)) {
            if (length(obj)==1)
               fld <- rep(1L,obj)
            else
               fld <- rep(1L,length(obj))
         }
         else
            fld <- rep(1L,nband(obj))
      }
      if (is.null(skip))
         skip <- which(!fld)
      else {
         res <- rep(1,length(fld)+length(skip))
         res[skip] <- 0
         res[-skip] <- fld
         fld <- res
         rm(res)
      }
      if (length(layout)==2) {
         if ((is.na(layout[1]))&&(is.numeric(layout[2]))) {
            layout[1] <- ceiling(length(fld)/layout[2])
         }
         else if ((is.na(layout[2]))&&(is.numeric(layout[1]))) {
            layout[2] <- ceiling(length(fld)/layout[1])
         }
      }
      if (any(is.na(layout)))
      {
         nl <- length(fld)
         ##~ if (!isList) {
            ##~ nc <- obj$grid$columns
            ##~ nr <- obj$grid$rows
         ##~ }
         ##~ else {
            ##~ nc <- obj[[1]]$grid$columns
            ##~ nr <- obj[[1]]$grid$rows
         ##~ }
         nc <- g1$columns
         nr <- g1$rows
         s0 <- rep(0,nl)
         for (pr in seq(nl))
         {
            pc <- ceiling(nl/pr)
            if (((pc-1)*pr>=nl)||(pc*(pr-1)>=nl)) {
               s0[pr] <- 9999
               next
            }
            if ((isList)&&((pc>2)&&(pr>2))) {
               s0[pr] <- 9999
               next
            }
            lc <- pc*nc
            lr <- pr*nr
            s1 <- (lc/lr)/ratio#-1 ## ratio 1.5=15:10  16:9   4:3
            if (s1<1)
               s1 <- 1/s1
            s0[pr] <- s1-1
            if (verbose)
               print(data.frame(panelc=pc,panelr=pr,nc=nc,nr=nr
                               ,lc=lc,lr=lr,ratio=(lc/lr)/ratio,s=s0[pr]))
         }
         ind <- which.min(abs(s0))[1]
         if ((FALSE)&&(length(legend)==1)&&(is.na(legend)))
            legend <- ifelse(s0[ind]<=0,4L,1L)
         panelr <- ind
         panelc <- ceiling(nl/panelr)
         if (isList & (panelc>2 & panelr>2)) {
            if (panelc>panelr) { ## *ratio?
               panelr <- 2
               panelc <- ceiling(nl/panelr)
            }
            else {
               panelc <- 2
               panelr <- ceiling(nl/panelc)
            }
         }
         ##~ if (nl<=panelr*(panelc-1)) repeat({
            ##~ panelc <- panelc-1
            ##~ if (nl<=panelr*(panelc-1))
               ##~ break
         ##~ })
         ##~ else if (nl<=(panelr-1)*panelc) repeat ({
            ##~ panelr <- panelr-1
            ##~ if (nl<=(panelr-1)*panelc)
               ##~ break
         ##~ })
         if (!length(skip))
            skip <- which(is.na(match(seq(panelc*panelr),seq(nl))))
         if (verbose)
            print(c(bands=nl,columns=panelc,rows=panelr,panels=panelc*panelr
                   ,samples=nc,lines=nr))
         rm(nc,nr,lc,lr,s0,ind)
      }
      else
      {
         panelr <- layout[1]
         panelc <- layout[2]
         if (!length(skip)) {
            oldF <- fld
            fld <- rep(0L,prod(layout))
            fld[seq(length(oldF))] <- oldF
            skip <- which(!fld)
         }
      }
   }
   mosaic <- matrix(0,ncol=panelc*2+3,nrow=panelr*2+3)
   k <- 0L
   m <- k
   if (byrow)
   {
      for (ir in 1:panelr)
      {
         for (ic in 1:panelc)
         {
            k <- k+1L
            if (k %in% skip)
               next
            m <- m+1L
            mosaic[2*ir+1,2*ic+1] <- m
         }
      }
   }
   else
   {
      for (ic in 1:panelc)
      {
         for (ir in 1:panelr)
         {
            k <- k+1L
            if (k %in% skip)
               next
            m <- m+1L
            mosaic[2*ir+1,2*ic+1] <- m
         }
      }
   }
   k <- m+1
   if (length(legend)==1) {
      if ((isList)&&(forcedLegend)&&((panelc>1)||(panelr>1))) { ## 20160112 added &&(forcedLegend)
         nl <- length(unique(fld[fld>0]))
        # print(c(panelc=panelc,panelr=panelr))
         oldL <- legend
         step3 <- (length(oldM)==2) && (!is.na(oldM[1])) && (oldM[1]==2)
         step4 <- (length(oldM)==2) && (!is.na(oldM[2])) && (oldM[2]==2)
         if (!step3 & !step4) {
            if ((panelr==2)&&(panelc>2))
               step3 <- TRUE
            else if ((panelc==2)&&(panelr>2))
               step4 <- TRUE
            else if ((panelc==2)&&(panelr==2))
               step4 <- TRUE
         }
         legend <- vector("list",nl)
         if ((panelc==1)&&(panelr==1)) { ## single panel
            ;
         }
         if (panelc==1) { ## 1-column
            if (is.na(oldL))
               oldL <- "right"
            for (i in seq(nl))
               legend[[i]] <- list(i,oldL)
         }
         else if (panelr==1) { ## 1-row
            if (is.na(oldL))
               oldL <- "bottom"
            for (i in seq(nl))
               legend[[i]] <- list(oldL,i)
         }
         else if (step3) { ## 2-rows
            j <- 1L
            if (!byrow) {
               for (i in seq(panelc)) {
                  for (i2 in c(1,2)) {
                     i3 <- (i-1)*panelr+(i2-1)+1
                     if (!(i3 %in% fld))
                        next
                     legend[[j]] <- list(ifelse(i2==1,"top","bottom"),i)
                     j <- j+1L
                  }
               }
            }
            else {
               for (i2 in c(1,2)) {
                  for (i in seq(panelc)) {
                     i3 <- (i2-1)*panelc+(i-1)+1
                     if (!(i3 %in% fld))
                        next
                     legend[[j]] <- list(ifelse(i2==1,"top","bottom"),i)
                     j <- j+1L
                  }
               }
            }
         }
         else if (step4) { ## 2-columns
            j <- 1L
            if (!byrow) {
               for (i2 in c(1,2)) {
                  for (i in seq(panelr)) {
                     i3 <- (i2-1)*panelr+(i-1)+1
                     if (!(i3 %in% fld))
                        next
                     legend[[j]] <- list(i,ifelse(i2==1,"left","right"))
                     j <- j+1L
                  }
               }
            }
            else {
               for (i in seq(panelr)) {
                  for (i2 in c(1,2)) {
                     i3 <- (i-1)*panelc+(i2-1)+1
                     if (!(i3 %in% fld))
                        next
                     legend[[j]] <- list(i,ifelse(i2==1,"left","right"))
                     j <- j+1L
                  }
               }
            }
         }
         else {
            opW <- options(warn=1)
            warning("Unable to composite multiple scalebars")
            options(opW)
            legend <- list()#(NULL)
            forcedLegend <- FALSE
         }
      }
      else if ((forcedLegend)&&(is.na(legend)))
         legend <- "right"
   }
   if (is.character(legend))
      legend <- list(list(switch(legend[1],top=0,bottom=99,1:99)
                         ,switch(legend[1],left=0,right=99,1:99)))
   else if ((length(legend)==1)&&(is.numeric(legend))&&
            (as.integer(legend) %in% 1L:4L))
   {
      if (legend==1)
         legend <- list(list(99,1:99))
      else if (legend==2)
         legend <- list(list(1:99,0))
      else if (legend==3)
         legend <- list(list(0,1:99))
      else if (legend==4)
         legend <- list(list(1:99,99))
   }
   for (i in seq_along(legend))
   {
     # if ((length(skip))&&(panelc*panelr==length(legend))&&(i %in% skip))
     #    next
      leg <- legend[[i]]
      if (length(leg)!=2)
      {
         if (is.list(leg))
            kwd <- leg[[1]]
         else
            kwd <- leg[1]
         leg <- switch(kwd
                      ,left=list("full","left")
                      ,right=list("full","right")
                      ,top=list("top","full")
                      ,bottom=list("bottom","full")
                      ,list())
         if (!length(leg))
            next
        # stop("wrong format")
        # k <- k+1
        # next
      }
      leg1 <- leg[[1]]
      leg2 <- leg[[2]]
      if (is.character(leg1))
         leg1 <- switch(leg1,top=0,first=1,last=panelr,bottom=99,full=1:99
                       ,stop("unknown keyword"))
      if (is.character(leg2))
         leg2 <- switch(leg2,left=0,first=1,last=panelc,right=99,full=1:99
                       ,stop("unknown keyword"))
      posr <- range(leg1)
      posc <- range(leg2)
      posr <- (posr[1]:posr[2])*2+1
      posc <- (posc[1]:posc[2])*2+1
      posr[posr<1] <- 1
      posc[posc<1] <- 1
      posr[posr>nrow(mosaic)] <- nrow(mosaic)
      posc[posc>ncol(mosaic)] <- ncol(mosaic)
      posc <- unique(posc)
      posr <- unique(posr)
      if ((length(leg1)>1)&&((length(leg2)==1)))
         posr <- posr[posr>1 & posr<nrow(mosaic)]
      else if ((length(leg2)>1)&&((length(leg1)==1)))
         posc <- posc[posc>1 & posc<ncol(mosaic)]
      mosaic[posr,posc] <- k
      k <- k+1
   }
   if (FALSE)
   {
      a1 <- diff(apply(mosaic,1,function(x){all(x==0)}))
      a2 <- diff(apply(mosaic,2,function(x){all(x==0)}))
      if (a1[1]==0)
         mosaic <- mosaic[-c(1,2),,drop=FALSE]
      if (a1[length(a1)]==0)
         mosaic <- mosaic[-nrow(mosaic)+c(0,1),,drop=FALSE]
      if (a2[1]==0)
         mosaic <- mosaic[,-c(1,2),drop=FALSE]
      if (a2[length(a2)]==0)
         mosaic <- mosaic[,-ncol(mosaic)+c(0,1),drop=FALSE]
   }
   if (!forcedLegend)
      sLegend <- 1
   else
      sLegend <- sapply(legend,function(x) length(x)>0)
   options(ursaPngSkipLegend=which(!sLegend)) ## ,ursaPngSkip=skip
   res <- list(layout=mosaic,image=m
              ,legend=ifelse(!forcedLegend,0L,sum(sLegend)))
  # if (!is.null(names(legend)))
  #    res$legendName=names(legend)
  # res <- list(layout=mosaic,image=vector("list",prod(image))
  #            ,legend=vector("list",length(legend)))
   class(res) <- "ursaLayout"
   res
}
