'legend_colorbar' <- function(...) {
   if (.skipPlot(FALSE))
      return(NULL)
  # if (!getOption("ursaPngLegend"))
   if (!getOption("ursaPngLayout")$legend)
      return(invisible(10L))
   arglist <- list(...)
   kwd <- "colorbar"
   colorbar <- .getPrm(arglist,class="logical",name=paste0("^",kwd,"$")
                      ,default=TRUE)
   if (!is.logical(colorbar))
      colorbar <- TRUE
   if (!colorbar)
      return(NULL)
   obj <- .getPrm(arglist,name="",default=NULL
                 ,class=list(c("list","ursaRaster"),"ursaRaster"
                            ,c("list","ursaColorTable"),"ursaColorTable"))
   if (is.null(obj))
      obj <- ursa_colortable(arglist[[1]])
   if (is.null(obj))
      return(invisible(NULL))
   if (!.is.colortable(obj)) {
      ind <- which(sapply(obj,.is.colortable))
      if (length(ind)) {
         for (i in ind) {
            arglist[[1]] <- ursa_colortable(obj[[i]])
            do.call("legend_colorbar",arglist)
         }
      }
      return(NULL)
   }
   units <- .getPrm(arglist,name="unit(s)*",kwd=kwd
                   ,class=list("character","expression"),default="",verbose=FALSE)
   if (!nchar(units)) {
      aname <- names(arglist[1])
      if (!is.null(aname))
         units <- names(arglist[1])
   }
   labels <- .getPrm(arglist,name="labels",kwd=kwd,default=NA_real_)
   align <- .getPrm(arglist,name="align",class=list("character","numeric")
                   ,kwd=kwd,default=NULL,verbose=FALSE)
   shift <- .getPrm(arglist,name="shift",kwd=kwd,default=1)
   cex <- .getPrm(arglist,name="cex",kwd=kwd,default=1)
   adj <- .getPrm(arglist,name="adj",kwd=kwd,default=NA_real_)
   las <- .getPrm(arglist,name="las",kwd=kwd,default=1)
   forceLabel <- .getPrm(arglist,name="forceLabel",kwd=kwd,default=FALSE)
   lomar <- .getPrm(arglist,name="lomar",kwd=kwd,default=0)
   himar <- .getPrm(arglist,name="himar",kwd=kwd,default=0)
   turn <- .getPrm(arglist,name="turn",kwd=kwd,default=FALSE)
   useRaster <- .getPrm(arglist,name="useRaster",kwd=kwd,default=NA)
   trim <- .getPrm(arglist,name="trim",kwd=kwd,default=0L)
   abbrev <- .getPrm(arglist,name="abbrev",class=list("integer","logical")
                    ,kwd=kwd,default=18L)
   opacity <- .getPrm(arglist,name="opacity",kwd=kwd,default=NA_real_)
   if (is.logical(abbrev))
      abbrev <- 24L*as.integer(abbrev)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   if (is.ursa(obj)) {
      if (.is.colortable(obj))
         ct <- obj$colortable
      else
         ct <- colorize(obj,...)$colortable
   }
   else if (inherits(obj,"ursaColorTable"))
      ct <- obj
   else
      stop("Unclear coercion 'object' -> colorbar") ## return(NULL)
   ret <- .legend_colorbar(ct,units=units,labels=labels,align=align,shift=shift
                          ,cex=cex,adj=adj,las=las,forceLabel=forceLabel
                          ,lomar=lomar,himar=himar,turn=turn
                          ,useRaster=useRaster,trim=trim,abbrev=abbrev
                          ,opacity=opacity,verbose=verbose)
   ret
}
'.legend_colorbar' <- function(ct,units="",labels=NA,align=NULL,shift=1 # labels=11L
                            ,cex=1,adj=NA,las=1
                            ,forceLabel=FALSE,lomar=0,himar=0,turn=FALSE
                            ,useRaster=NA,trim=0L,abbrev=24L,opacity=NA
                            ,verbose=FALSE)
{
   devPrettyLab <- F | .isPackageInUse()
   if (.skipPlot(FALSE))
      return(NULL)
   if (!getOption("ursaPngLayout")$legend)
      return(invisible(10L))
  # print(names(ct),quote=FALSE)
  # str(as.list(match.call()))
   if (is.null(names(ct)))
      return(NULL)
   if (all(is.na(names(ct)))) {
      if (!FALSE) {
        # display colorbar without labels or not?
     }
     else {
       # par() ## or 'plot'?
        plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
        return(0L)
     }
   }
   if (is.na(useRaster))
      useRaster <- getOption("ursaPngDevice")!="windows"
  # isChar <- length(grep("([A-Za-d]|[f-z]|/|\\*)",names(ct)))>0
   if ((devSkipExtraTiffValues <- TRUE)&&(length(ct) %in% 2^c(8,16,32))) {
     # ct <- tail(ct,-2540) ## dev N_20200407_concentration_v3.0.tif 
      ind <- unname(which(rev(ct)=="#000000"))
      seqi <- rev(seq(length(ct)))
      if ((length(ind)>1)&&(1L %in% ind)) {
         ind2 <- which(diff(ind)>1)
         if (length(ind2)) {
            ind3 <- seq(seqi[tail(ind2,1)]+1,seqi[1])
         }
         else {
            ind3 <- seq(seqi[tail(ind,1)-1],seqi[1])
         }
         ct <- ct[-ind3]
      }
   }
   label <- .deintervale(ct,verbose=FALSE)
   fmtLabel <- attr(ct,"label")
   if (length(label)==length(fmtLabel)) {
      label <- fmtLabel
      isChar <- TRUE
   }
   isChar <- is.character(label)
   isInterval <- length(label)!=length(ct)
   if ((isChar)&&(!isInterval))
      label <- names(ct)
   family <- getOption("ursaPngFamily")
   side <- .getSide()
   if ((length(ct)==1)&&(isChar | !isChar))
      las <- 0
  # maxlabel <- ifelse(isChar || forceLabel,999,21) ## removed 2015-12-13
   maxlabel <- ifelse(forceLabel,999,21) ## added 2015-12-13
   offset <- NULL
   if (length(offset)!=4)
      offset <- rep(offset[1],4)
   if (is.na(adj)) {
      if (!isChar) {
         if (side %in% c(2,4)) {
            if (las %in% c(0,3)) ## vert
               adj <- 0.5
            else
               adj <- 1
         }
         else { ## side %in% c(1,3)
            if (las %in% c(2,3)) ## vert
               adj <- 1
            else
               adj <- 0.5
         }
      }
      else {
        # adj <- ifelse(side %in% c(4,3),0,1)
         if (side %in% c(2,4)) {
            if (las %in% c(0,3)) ## vert
               adj <- 0.5
            else
               adj <- ifelse(side==2,1,0)
         }
         else { ## side %in% c(1,3)
            if (las %in% c(2,3)) ## vert
               adj <- ifelse(side==1,1,0)
            else
               adj <- 0.5
         }
        # if (verbose)
        #    print(c(adj=adj))
      }
   }
   par(mar=c(ifelse(side %in% c(2,4),lomar,0)
            ,ifelse(side %in% c(1,3),lomar,0)
            ,ifelse(side %in% c(2,4),himar,0)
            ,ifelse(side %in% c(1,3),himar,0)))
   scale <- getOption("ursaPngScale")
  # colorbar <- getOption("ursaPngBar")
   col <- as.character(ct)
   if (!length(col)) {
      ct <- c(no_data="transparent")
      col <- as.character(ct)
   }
   np <- length(col)
   # label <- if (isChar) names(ct) else .deintervale(names(ct))
   ##~ label <- paste0("sdsddsd",label)
   ##~ label <- substr(label,1,1)
   nl <- length(label)
   interval <- as.integer(nl!=np)
   at1 <- seq(nl)+ifelse(interval,0.5,0.0)
   isRamp <- FALSE
   if (is.na(labels[1]))
      labels <- ifelse(isChar,31L,11L)
   isSpecified <- ((length(labels)>1)||(labels[1]==0)||(!.is.integer(labels[1]))||
                   ((!is.integer(labels[1]))&&(labels[1]==1)))
   if (!isSpecified) {
      labels[labels>nl] <- nl
   }
   isTick <- (nl>maxlabel) | isSpecified
   keepLabel <- NULL
   z <- z0 <- seq(col)-0L ## start from 0 or 1?
  # z <- z0 <- seq(keepLabel)-0L ## start from 0 or 1?
   if (side %in% c(1,3))
      plot(0,0,type="n",xlim=c(min(z0)-0.5,max(z0)+0.5),ylim=c(0,1)
          ,xlab="",ylab="",axes=FALSE)
   else if (side %in% c(2,4))
      plot(0,0,type="n",ylim=c(min(z0)-0.5,max(z0)+0.5),xlim=c(0,1)
          ,xlab="",ylab="",axes=FALSE)
   if ((isChar)&&(abbrev>0)) {
      if (all(Encoding(label)!="UTF-8")) {
         label2 <- label #iconv(label,to="UTF-8")
         if (.isRscript()) {
            shorten <- side %in% c(1,3) & las %in% c(0,1) |
                       side %in% c(2,4) & las %in% c(0,3)
            if (!shorten)
               a <- .try(label <- abbreviate(label2,minlength=abbrev,strict=TRUE))
            else {
               abbrev <- 64
               mwidth <- par()$fin[ifelse(side %in% c(1,3),1,2)]
               for (abbr in c(abbrev:2)) {
                  a <- .try(label <- abbreviate(label2,minlength=abbr,strict=TRUE))
                  if (!a)
                     break
                  width <- max(strwidth(paste0("Ww",label)
                                   ,units="inches",cex=cex,family=family))
                 # print(c(w0=width,w1=mwidth,w2=width*length(label)))
                  if (width*length(label)<mwidth)
                     break
               }
            }
         }
         else {
           # encdng <- Encoding(label)
           # Encoding(label) <- "UTF-8"
            a <- .try(label <- abbreviate(label2,minlength=abbrev,strict=TRUE))
           # Encoding(label) <- encdng
         }
         rm(label2)
      }
      else
         a <- FALSE
      if (!a) {
         ind <- which(nchar(iconv(label,to="UTF-8"))>abbrev)
         if (F) {
            label[ind] <- substr(label[ind],1,abbrev)
            substr(label[ind],abbrev,abbrev) <- ">"
            a2 <- .try(label[ind] <- substr(label[ind],1,abbrev))
         }
         else {
            label[ind] <- paste0(substr(label[ind],1,abbrev-1),"\u2026")
         }
      }
   }
   if (isTick)
   {
      if (TRUE) { #(!isChar) {
         if (isChar) {
            keepLabel <- label
           # label <- factor(label)
            label <- seq_along(label)-0L ## start from 0 or 1?
         }
         else if (any(diff(as.numeric(label))<0)) { ## unsorted means categories
            keepLabel <- label
            label <- seq_along(label)-0L ## start from 0 or 1?
         }
         y <- as.numeric(label)
         uniy <- unique(diff(y))
         isRamp <- TRUE
         x <- seq(nl)+ifelse(interval,0.5,0.0)
         isRegular <- ((length(uniy)==1)||(sd(uniy)<1e-11))
         if (isSpecified) {
            isManual <- TRUE
            label <- labels
            labels <- length(label)
         }
         else if (TRUE) {
            if (!isRegular)
               keepIrreg <- label
            isW <- side %in% c(1,3) & las %in% c(0,1) |
                   side %in% c(2,4) & las %in% c(0,3)
            isManual <- FALSE
            if (!isW) {
               height <- 1.5*strheight("Mg",units="inches",cex=cex,family=family)
               mheight <- max(par()$fin)
               repeat({
                  label <- pretty(y,n=labels)
                  if ((length(label)+1)*height<mheight)
                     break
                  labels <- labels-1
                  if (labels<1)
                     break
               })
            }
            else {
               mwidth <- max(par()$fin)
               repeat({
                  if (!isChar) {
                     label <- pretty(y,n=labels)
                     if (F & devPrettyLab & !is.null(keepLabel)) {
                        label <- label[label>0]
                       # ct <- colorize(keepLabel,ncolor=length(label),tail=0)
                       # label4 <- as.numeric(names(ursa_colortable(ct)))
                       # print(c('L4:'=label4))
                        label1 <- keepLabel[label]
                        label2 <- pretty(label1,n=length(label))
                        print(c('L1:'=label1))
                        print(c('L2:'=label2))
                        label3 <- sapply(label1,function(x) {
                           dl <- abs(x-label2)
                           ind <- which.min(dl)
                           dl[ind]
                          # label2[ind]
                          # ind
                        })
                        print(c('L3:'=label3))
                       # print(c(label=length(label),label1=length(label1)
                       #        ,label2=length(label2),label3=length(label3)
                       #        ,label4=length(label4),labels=labels))
                       # print(keepLabel[label3])
                       # label <- label+label3
                       # label <- label[label>=1 & label<=max(y)]
                     }
                  }
                  else {
                     labelProposed <- .prettyLabel(y,ncol=labels)$at
                     if (is.null(labelProposed))
                        break
                     label <- labelProposed
                     if (!.is.integer(label)) {
                        labels <- labels-1
                        next
                     }
                  }
                  labelW <- if (isChar) keepLabel[label] else label
                  width <- max(strwidth(paste0(ifelse(isChar,"Wii","Wii"),labelW)
                                   ,units="inches",cex=cex,family=family))
                  if (width*length(label)<mwidth)
                     break
                  labels <- labels-1
               })
            }
            if (!isRegular)
               label <- keepIrreg
          # print(c(have=(length(label)+1)*height,lim=mheight))
         }
         if ((!isRegular)&&(is.numeric(label))&&(!isSpecified)) {
            y <- as.numeric(label)
            uniy <- unique(diff(y))
            if (length(uniy)==1)
               isRegular <- TRUE
         }
        # adj <- if ((side %in% c(4L))&&())
         if (isRegular) {
            a0 <- (max(x)-min(x))/(max(y)-min(y))
            b0 <- min(x)-a0*min(y)
            at2 <- a0*label+b0
            at1 <- at2[at2>=min(x) & at2<=max(x)]
            label <- label[match(at1,at2)]
            if (all(is.na(label))) {
               label <- unique(y)
            }
            rm(at2)
         }
         else {
            if (!isManual) {
               val <- reclass(ct) ## 20160128 reclass(obj) -> reclass(ct)
               if (is.ursa(val))
                  val <- sort(c(na.omit(c(val$value))))
               for (mycol in seq(labels,11*(labels-1)+1,by=2))
               {
                  if (interval %in% c(0L))
                     th <- seq(0,1,length=mycol*2+1)[seq(mycol)*2]
                  else if (interval %in% c(1L,2L))
                  {
                     th <- seq(0,1,length=mycol+2)
                     th <- th[-c(1,length(th))]
                  }
                  value <- unique(val[round(length(val)*th)])
                  if (length(value) >= labels)
                     break
               }
               dth <- mean(diff(th))/2
              # sh <- c(1/2,1/4,1/8,1/16,1/32,1/64)
               sh <- c(seq(0.6,0.1,by=-0.1),seq(0.1,0.02,by=-0.02)[-1])
              # names(sh) <- as.character(sh)
               v <- vector("list",length(value))
               n <- length(val)
               ind <- c(1,1)
               for (i in seq_along(value)) {
                  if (i!=1L)
                     ind[1L] <- ind[2L]+1
                  ind[2L] <- ceiling((th[i]+dth)*n)
                  if (!FALSE)
                     v[[i]] <- val[ind[1]:ind[2]]
                  else
                     v[[i]] <- val[ceiling(c(th[i]-dth+1e-6,th[i]+dth)*n)]
               }
               res <- vector("list",length(sh))
              # names(res) <- names(sh)
               for (j in seq_along(sh)) {
                  value2 <- rep(NA,length(value))
                  for (i in seq_along(value)) {
                     v2 <- v[[i]]
                     if ((v2[1]<=0)&&(v2[2]>=0))
                        value2[i] <- 0
                     else {
                       # a <- pretty(v2,n=3,shrink=sh[j])
                       # a <- pretty(range(v2),shrink=sh[j],n=3)
                        a <- pretty(median(v2),shrink=sh[j])
                        value2[i] <- a[which.min(abs(a-median(v2)))[1]]
                     }
                  }
                  res[[j]] <- unique(value2)
                  if (length(res[[j]])>=length(value))
                     break
               }
               ind <- which.min(abs(length(value)-sapply(res,length)))
               label <- res[[ind]]
              # print(length(label))
               rm(val,th,dth,value)
            }
            label <- label[label>=min(y) & label<=max(y)]
            at1 <- rep(NA,length(label))
            for (i in seq_along(label)) {
               lab <- label[i]
               ind2 <- which((y-lab)>0)[1]
               if (is.na(ind2))
                  at1[i] <- tail(x,1)
               else if (ind2>1) {
                  ind1 <- ind2-1L
                  at1[i] <- x[ind1]+(lab-y[ind1])/(y[ind2]-y[ind1])
               }
               else
                  at1[i] <- x[ind2]
              # print(data.frame(i=i,lab=lab,ind2=ind2,at=at1[i]))
            }
            if (exists("ind1"))
               rm(ind1)
            if (exists("ind2"))
               rm(ind2)
         }
         if (!is.null(keepLabel)) {
            if (!devPrettyLab | is.character(keepLabel)) {
               label <- keepLabel[unique(as.integer(round(label)))]
            }
            else  {
               label <- unique(as.integer(round(label)))
              # print(label)
               label1 <- keepLabel[label]
               label2 <- pretty(label1,n=length(label))
              # print(c('L1:'=label1))
              # print(c('L2:'=label2))
               label3 <- sapply(label1,function(x) {
                  dl <- abs(x-label2)
                  ind <- which.min(dl)
                  dl[ind]
                 # label2[ind]
                 # ind
               })
              # print(c('L3:'=label3))
               label <- label1+label3
              # print(c('L:'=label))
               if (F & is.numeric(keepLabel)) {
                  label0 <- label
                  dlab <- diff(unique(sort(label)))
                  if (.is.integer(dlab))
                     dlab <- as.integer(round(dlab))
                  if (length(unique(dlab))==1) {
                     ud1 <- unique(dlab)
                     desired <- pretty(keepLabel,n=length(label))
                     print(length(label)==length(desired))
                     ud2 <- unique(diff(desired))
                     if (ud1==ud2) {
                        print(label0)
                        label <- c(na.omit(match(desired,keepLabel)))
                       # label <- keepLabel[label]
                        print(label)
                        q()
                     }
                  }
               }
            }
         }
        # if ((!isChar)&&(!((side %in% c(1,3))&&(las %in% c(0,1)))))
        #    label <- format(label,trim=TRUE,scientific=FALSE)
         rm(x,y)
      }
      else {
         maxlabel <- (labels-1L)/2+1L
         i1 <- seq(label)
         i2 <- 1+.round((nl-1)*seq(0,1,length=(maxlabel)*2+1)[2*seq(1,maxlabel,by=1)])
         ind <- i1 %in% i2
         label[!ind] <- ""
         rm(i1,i2)
      }
   }
  # print(data.frame(at=at1,label=label))
  # str(keepLabel)
  # print(c(col=length(col),label=length(keepLabel)))
   if (turn)
   {
      z <- rev(z)
      label <- rev(label)
   }
   shadow <- getOption("ursaPngShadow")
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   tcol <- ifelse(bg<128,"#FFFFFF","#000000")
   scol <- paste0(tcol,"7F")
   if (length(z)>32767)
      useRaster <- FALSE
   if (any(substr(col,1,1)!="#")) {
      .col <- col2rgb(col)
      col <- rgb(.col[1,],.col[2,],.col[3,],maxColorValue=255)
   }
   ocol <- substr(col,8,9)
  # print(all(nchar(ocol)==2))
  # print(any(as.integer(paste0("0x",ocol))))
   if (is.na(opacity))
      opacity <- ((all(nchar(ocol)==2))&&((any(as.integer(paste0("0x",ocol))<255))))
   if (opacity<=0)
      opacity <- 0
   else if (opacity>=1)
      opacity <- 1
   if (opacity)
      shadow <- ""
   if (side %in% c(1,3))
   {
      if (opacity) {
         colBW <- colorRampPalette(c("grey40","grey80","white","white"))(16)
         if (side==3)
            colBW <- rev(colBW)
         nu <- length(colBW)
         zu <- matrix(rep(seq_along(colBW),times=length(z0)),nrow=nu)
         image(x=z0,y=seq(0,1,length=nu),t(zu),axes=FALSE,col=colBW
              ,xlab="",ylab="",useRaster=!useRaster,add=TRUE)
      }
      image(x=z0,y=c(0,1),z=matrix(z,ncol=1),axes=FALSE,col=col
           ,xlab="",ylab="",useRaster=useRaster,add=TRUE)
      if (nchar(shadow)) {
         image(x=z0,y=if (side==3) c(0.7,1) else c(0,0.3)
              ,z=matrix(z,ncol=1),axes=FALSE,col=shadow
              ,xlab="",ylab="",useRaster=useRaster,add=TRUE)
      }
      if (!isTick)
         abline(v=z0[-1]-0.5,h=0,col=scol,lty=1,lwd=0.5)
      else {
         for (d in c(1,3))
            axis(d,col=NA,col.ticks=scol,at=at1,tck=0.2
                ,labels=NA,lty=1,lwd=0.5)
      }
   }
   else if (side %in% c(2,4))
   {
      if (opacity) {
         colBW <- colorRampPalette(c("grey40","grey80","white","white"))(16)
         if (side==2)
            colBW <- rev(colBW)
         nu <- length(colBW)
         zu <- matrix(rep(seq_along(colBW),each=length(z0)),ncol=nu)
         image(y=z0,x=seq(0,1,length=nu),t(zu),axes=FALSE,col=colBW
              ,xlab="",ylab="",useRaster=!useRaster,add=TRUE)
      }
      image(y=z0,x=c(0,1),z=matrix(z,nrow=1),axes=FALSE,col=col
           ,xlab="",ylab="",useRaster=useRaster,add=TRUE)
      if (nchar(shadow)) {
         image(y=z0,x=if (side==4) c(0.7,1) else c(0,0.3)
              ,z=matrix(z,nrow=1),axes=FALSE,col=shadow
              ,xlab="",ylab="",useRaster=useRaster,add=TRUE)
      }
      if (!isTick)
         abline(v=0,h=seq(col)[-1]-0.5,col=scol,lty=1,lwd=0.5)
      else {
         for (d in c(2,4))
            axis(d,col=NA,col.ticks=scol,at=at1,tck=0.2
                ,labels=NA,lty=1,lwd=0.5)
      }
   }
   if (!length(label)) {
      label <- ""
      skipLabel <- TRUE
   }
   else
      skipLabel <- FALSE
  # return(NULL)
  # a <- 2*adj*width#*scale[2]
  # print(a)
  ## dependence on compose_open(pointsize=...)
   usr <- par()$usr
   fin <- par()$fin
   if (!skipLabel) {
      if (is.numeric(label)) {
         if (TRUE) {
            label1 <- format(label,trim=TRUE,scientific=NA)
            label2 <- as.character(label)
            label3 <- format(label,trim=TRUE,scientific=FALSE)
            sci <- length(.grep("e(\\+|\\-)\\d+$",label2))
            if (sci) {
               dig <- max(nchar(gsub("^(.+)\\.(\\d+)$","\\2",label2)))
               label2 <- format(label,trim=TRUE,scientific=FALSE,nsmall=dig)
            }
            cond1 <- max(nchar(label1))+0<=max(nchar(label2))
            cond2 <- (!sci)||(sci==length(label))
            cond3 <- length(unique(label1))==length(label)
            label <- if (((cond1)||(!cond2))&&(cond3)) label1
                     else label2
            if (localVerb <- FALSE) {
               print(label1)
               print(label2)
               print(label3)
               print(c(cond1=cond1,cond2=cond2,cond3=cond3))
               print(label)
               q()
            }
         }
         else {
            if (length(label)>2) {
               difL <- diff(label)
               if (length(which(!is.na(.is.near(difL,mean(difL)))))==length(difL))
                  label <- format(label,trim=TRUE,scientific=NA)
               else
                  label <- as.character(label)
            }
            else
               label <- format(label,trim=TRUE,scientific=NA)
         }
      }
   }
   if (!length(align)) {
      width <- try(max(strwidth(label,units="inches",cex=cex,family=family)))
      if (inherits(width,"try-error")) {
         labE <- Encoding(label)
         if (all(c("unknown","UTF-8") %in% unique(labE))) {
            Encoding(label) <- "unknown"
            width <- max(strwidth(label#[labE %in% "unknown"]
                        ,units="inches",cex=cex,family=family))
         }
      }
   }
   else
      width <- max(strwidth(align,units="inches",cex=cex))
   height <- max(strheight(label,units="inches",cex=cex,family=family)) ## family was missing
   if (is.numeric(shift)) {
     # print(c(side=side,las=las,shift=shift))
      if ((side %in% c(2,4))&&(las %in% c(1,2))||
          (side %in% c(1,3))&&(las %in% c(0,1)))
         width <- width*shift
      else
         height <- height*shift
   }
   if (width < 0) {
      width <- strwidth(paste(rep(0,max(nchar(label))),collapse="")
                       ,units="inches",cex=cex,family=family)
      width <- strheight("000",units="inches",cex=cex,family=family)
   }
   if (!skipLabel) {
      if (side==4)
      {
         adj1 <- adj
         if ((isRamp)&&(trim)) {
            mul <- (usr[4]-usr[3])/fin[2]
            n <- length(label)
            if (las %in% c(1L,2L))
               w <- strheight(label,units="inch",cex=cex)*mul
            else {
               w <- strwidth(label,units="inch",cex=cex)*mul
               adj1 <- 0.5
            }
            if (at1[1]-w[1]/2<usr[3]) {
               at1[1] <- usr[3]+1.3*w[1]/2
               if (trim>1)
                  label[1] <- ""
            }
            if (at1[n]+w[n]/2>usr[4]) {
               at1[n] <- usr[4]-(1.3*ifelse(las %in% c(1L,2L),1.5,1))*w[n]/2
               if (trim>1)
                  label[n] <- ""
            }
            if ((!numeric(shift))&&(!length(align))&&(trim>1))
               width <- max(strwidth(label,units="inches",cex=cex,family=family))
         }
         if (las %in% c(1,2)) ## horiz
            mtext(text=label,at=at1,las=las,line=0.6+cex*adj*width/height*1.0
                 ,side=side,cex=cex,padj=ifelse(isRamp,0.5,0.4),adj=adj1,col=tcol)
         else
            mtext(text=label,at=at1,las=las,line=0.1
                 ,side=side,cex=cex,padj=0.5,adj=adj1,col=tcol)
         ##~ mtext(text=label,at=at1,las=las,line=0.6+adj*width/height
              ##~ ,side=side,padj=ifelse(isRamp,0.5,0.4),adj=adj1,cex=cex,col="black")
      }
      else if (side==3)
      {
         if ((isRamp)&&(trim)) {
            mul <- (usr[2]-usr[1])/fin[1]
            n <- length(label)
            if (las %in% c(2L,3L))
               w <- strheight(label,units="inch",cex=cex)*mul
            else {
               w <- strwidth(label,units="inch",cex=cex)*mul
            }
            if (at1[1]-w[1]/2<usr[1]) {
               at1[1] <- usr[1]+(1.3*ifelse(las %in% c(2L,3L),1.5,1))*w[1]/2
               if (trim>1)
                  label[1] <- ""
            }
            if (at1[n]+w[n]/2>usr[2]) {
               at1[n] <- usr[2]-(1.3*ifelse(las %in% c(2L,3L),1,1))*w[n]/2
               if (trim>1)
                  label[n] <- ""
            }
            if ((!numeric(shift))&&(!length(align))&&(trim>1))
               width <- max(strwidth(label,units="inches",cex=cex,family=family))
         }
         mtext(text=label,at=at1,las=las
              ,line=ifelse(las %in% c(0,1),0,0.5+adj*width/height)
              ,side=side
              ,padj=ifelse(las %in% c(0,1),-0.25,0.4)
              ,adj=adj #ifelse(las==1,0.5,adj)
              ,cex=cex,col=tcol)
      }
      else if (side==2)
      {
         if ((isRamp)&&(trim)) {
            mul <- (usr[4]-usr[3])/fin[2]
            n <- length(label)
            if (las %in% c(1L,2L))
               w <- strheight(label,units="inch",cex=cex)*mul
            else {
               w <- strwidth(label,units="inch",cex=cex)*mul
            }
            if (at1[1]-w[1]/2<usr[3]) {
               at1[1] <- usr[3]+1.3*w[1]/2
               if (trim>1)
                  label[1] <- ""
            }
            if (at1[n]+w[n]/2>usr[4]) {
               at1[n] <- usr[4]-(1.3*ifelse(las %in% c(1L,2L),1.5,1))*w[n]/2
               if (trim>1)
                  label[n] <- ""
            }
            if ((!numeric(shift))&&(!length(align))&&(trim>1))
               width <- max(strwidth(label,units="inches",cex=cex,family=family))
         }
         mtext(text=label,at=at1,las=las,line=0.4
              ,side=side,padj=ifelse(las %in% c(0,3),0.2,0.4)
              ,adj=ifelse(las %in% c(0,3),0.5,adj),cex=cex,col=tcol)
      }
      else if (side==1)
      {
         if ((isRamp)&&(trim)) {
            mul <- (usr[2]-usr[1])/fin[1]
            n <- length(label)
            if (las %in% c(2L,3L))
               w <- strheight(label,units="inch",cex=cex)*mul
            else {
               w <- strwidth(label,units="inch",cex=cex)*mul
            }
            if (at1[1]-w[1]/2<usr[1]) {
               at1[1] <- usr[1]+(1.3*ifelse(las %in% c(2L,3L),1.5,1))*w[1]/2
               if (trim>1)
                  label[1] <- ""
            }
            if (at1[n]+w[n]/2>usr[2]) {
               at1[n] <- usr[2]-(1.3*ifelse(las %in% c(1L,2L),1,1))*w[n]/2
               if (trim>1)
                  label[n] <- ""
            }
            if ((!numeric(shift))&&(!length(align))&&(trim>1))
               width <- max(strwidth(label,units="inches",cex=cex,family=family))
         }
         mtext(text=label,at=at1,las=las
              ,line=ifelse(las %in% c(0,1),0,0.3+(1-adj)*width/height)
              ,side=side,padj=0.5
              ,adj=adj # ifelse(las %in% c(0,1),0.5,adj)
              ,cex=cex,col=tcol)
      }
   }
   if (getOption("ursaPngBox")) {
      panel_box()
   }
  # options(ursaWidth=width,ursaHeight=height)
   if (((is.character(units))&&(is.na(units)))||(!nchar(units)))
      return(invisible(10L))
  # b <- 2*width+0.5+height
  # print(b)
  ## TODO: how to use UTF8 in "as.expressions" 
   if (is.character(units)) {
      if (getOption("ursaPngDevice") %in% c("windows"))
         toE <- TRUE
      else {
         opWE <- options(warn=2)
         toE <- .try(abbreviate(units,minlength=2,strict=TRUE),silent=TRUE)
         options(opWE)
      }
      if (toE)
         units <- as.expression(substitute(bold(u),list(u=units)))
      else
         message(paste("Note: unable to make bold caption for",.dQuote(units)))
   }
   else if (is.list(units))
      units <- as.expression(substitute(bold(u)
                            ,list(u=paste(unlist(units),collapse=", "))))
   if (side==4) {
      if (las %in% c(1,2))
         mtext(units,side=side,padj=0,adj=0.5,las=3,col=tcol,cex=cex
              ,line=width/height+1.2)
      else
         mtext(units,side=side,padj=0,adj=0.5,las=3,col=tcol,cex=cex
              ,line=height+2)
   }
   else if (side==3) {
      mtext(units,side=side,padj=0,adj=0.5,las=1,col=tcol,cex=cex
           ,line=ifelse(las %in% c(0,1),1.5,width/height+0.7))
   }
   else if (side==2) {
      mtext(units,side=side,padj=0,adj=0.5,las=3,col=tcol,cex=cex
           ,line=ifelse(las %in% c(1,2),0.8+width/height,height+1.5))
   }
   else if (side==1) {
      ##~ mtext(units,side=side,padj=1,adj=0.5,las=1,col=tcol,cex=cex
           ##~ ,line=0.85)
      mtext(units,side=side,padj=1,adj=0.5,las=1,col=tcol,cex=cex
           ,line=ifelse(las %in% c(0,1),0.85,1*width/height))
   }
   invisible(0L)
}
