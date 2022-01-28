## modified from rje::cubeHelix 
# b <- colorize(n,weak=-270*pi/180,rotate=270/360) ~ colorBrewer "Spectral"
# b <- colorize(n,rich=0,rotate=60,hue=1.5 ~ for bathymetry

'cubehelix' <- function(n,value=numeric(),weak=NA,rich=NA,rotate=NA,hue=NA,gamma=1
                       ,dark=NA,light=NA,inv=NA,verbose=NA)
{
  # str(match.call())
  # set.seed(as.integer(as.numeric(Sys.time())/10))
  # value <- NULL
   verbose <- FALSE
   bg <- getOption("ursaPngBackground")
   bg <- ifelse(is.null(bg),255,sum(c(col2rgb(bg))*c(0.30,0.59,0.11)))
   .dark <- 91 # '63' for monitors, '91' for printers
   .light <- 241
  # print(getOption("ursaPngBackground"))
   default.dark <- ifelse(bg>127,.dark,255-.light)
   default.light <- ifelse(bg>127,.light,255-.dark)
   autobright <- FALSE
   if ((is.na(dark))&&(is.na(light))) {
      dark <- default.dark/255
      light <- default.light/255
      autobright <- TRUE
   }
   else if ((is.na(dark))&&(!is.na(light))) {
      if (light>1)
         light <- light/255
      dark <- default.dark/255
     # autobright <- FALSE
   }
   else if ((!is.na(dark))&&(is.na(light))) {
      if (dark>1)
         dark <- dark/255
      light <- default.light/255
     # autobright <- FALSE
   }
   if ((dark>1)||(light>1)) {
      dark <- dark/255
      light <- light/255
   }
   value <- unname(sort(value))
   if (missing(n))
      n <- if (length(value)) length(value) else 256L
   if (is.na(verbose))
      verbose <- FALSE #is.na(rich) || is.na(rotate) || is.na(hue)
   if (autobright) {
     # k <- round(1/(exp(n^0.5)),6) ## --20180324
     # k <- round(0.5/(exp(n^0.44)),6) ## ++20180324
      k <- round(0.7/(exp(n^0.475)),6) ## ++20180324
   }
   else
      k <- 0
   divergent <- length(value)>0 ## 20170705 -- length(value)>0
   if (divergent) {
      isCategory <- length(value)==n
      isInterval <- length(value)==n-1
      if ((!isCategory)&&(!isInterval))
         divergent <- FALSE
      else {
        # verbose <- TRUE
        # eps <- min(abs(value))
         eps <- 1e-14 #ifelse(eps<1e-11,1e-11,0)
         npos <- length(which(value>(-eps)))
         nneg <- length(which(value<(+eps)))
         nzer1 <- length(which(abs(value)<eps))
         if ((!npos)||(!nneg)||((npos==1)&&(nneg==1))) {
            divergent <- FALSE
         }
         else {
            if (isInterval) {
               dval <- diff(value)
               value <- c(head(value,1)-head(dval,1)/2
                         ,tail(value,-1)-dval/2
                         ,tail(value,1)+tail(dval,1)/2)
            }
            nzer2 <- length(which(abs(value)<eps))
           # npos <- length(which(value>(-eps)))
           # nneg <- length(which(value<(+eps)))
            indZ <- which(abs(abs(value)-min(abs(value)))<1e-11)
            if (nzer2) { ## 20170123 added '(TRUE)', 20170201 changed (nzer)
              # print(c(pos=npos,neg=nneg))
               npos <- length(which(seq(n)>=head(indZ,1)))
               nneg <- length(which(seq(n)<=tail(indZ,1)))
            }
            rest <- n-max(npos,nneg)#-as.integer(isInterval)
            n <- n-rest
            if (verbose)
               print(c(pos=npos,neg=nneg,z1=nzer1,z2=nzer2,n=n,rest=rest,indZ=indZ
                      ,int=isInterval,cat=isCategory))
         }
      }
   }
   if (dark+k>light-k) {
      k <- 0
      if (n==1)
         dark=0.7*light
   }
  # k is not optimal for n=2
   dark <- dark+3*k/2
   light <- light-1*k/2
   lambda <- rev(seq(dark,light,length.out=n))
   if ((divergent)&&(rest>0)) {
      if (length(indZ)==1) {
         lambda <- c(lambda[rev(1+seq(rest))],lambda)
         n <- n+rest
         if (anyNA(lambda)) {
            lambda <- rev(seq(dark,light,length.out=n))
            divergent <- FALSE
         }
      }
      else {
         if (TRUE) { #(length(lambda)>2) {
            nr <- 4
           # print(rest)
           # print(lambda[rev(0+seq(rest))])
           # print(rep(lambda[1],nr))
            lambda <- c(lambda[rev(0+seq(rest))],rep(lambda[1],nr),lambda)
            n <- n+rest+nr
            toOmit <- if (nneg>npos) n-rev(rest+seq(nr))+1 else rest+seq(nr)
         }
         else
            toOmit <- integer()
      }
      if (nneg>npos)
         lambda <- rev(lambda)
     # print(round(lambda*255,1))
   }
   if ((is.character(rotate))&&(.lgrep("circle|round",rotate))) {
      if (n>1)
         rotate <- .sample(c(-1,1),1)*360*(1-1/n)
      else
         rotate <- NA
   }
   if ((!is.na(rich))&&(!is.na(weak))) {
      rotate <- rich-weak
   }
   if (any(is.na(rotate))) {
      if (length(unique(lambda))==1) {
         rotate <- round(runif(1,min=0.5,max=0.9),2)
        # rotate <- 1
      }
      else {
         if (divergent)
            rotate <- round(runif(1,min=0.6,max=0.8),2)
         else if ((light-dark)/n<0.005)
            rotate <- round(runif(1,min=0.5,max=0.9),2)
         else
            rotate <- round(runif(1,min=0.4,max=1.2),2)
      }
      rotate <- sample(c(-1,1),1)*rotate*360
   }
   if ((is.na(weak))&&(!is.na(rich))) {
      weak <- rich-rotate
   }
   else if ((!is.na(weak))&&(is.na(rich))) {
      rich <- weak+rotate
   }
   else if ((is.na(weak))&&(is.na(rich))) {
      rich <- round(runif(1,min=0,max=2*pi),2)*180/pi
      weak <- rich-rotate
   }
   if (length(rotate)>1)
      rotate <- sample(rotate,1)
   if (is.na(hue))
      hue <- round(runif(1,min=0.9,max=1.5),2)
   if ((!FALSE)&&(verbose))
      message(sprintf("cubehelix: br=%d:%d weak=%4.0f rich=%4.0f rotate=%4.0f hue=%4.2f k=%.2f n=%d"
                     ,round(dark*255),round(light*255),weak,rich,rotate,hue,k,n))
   rich <- rich*pi/180
   weak <- weak*pi/180
   rotate <- rotate/360
   l <-  rep(lambda^gamma,each=3)
   phi = weak+2*pi*rotate*seq(0,1,length.out=n) # head(seq(0,1,length.out=n+1),-1))
  # print(round(phi*180/pi))
   tau <- rbind(cos(phi),sin(phi))
   M <- matrix(c(-0.14861,-0.29227,1.97294,1.78277,-0.90649,0),ncol=2)
  # print(M %*% tau)
   out <- l + hue * l * (1 - l)/2 * (M %*% tau)
   if ((divergent)&&(rest>0)&&(length(indZ)==2)) {
     # print(toOmit)
      if (length(toOmit))
         out <- out[,-toOmit]
   }
   out <- pmin(pmax(out, 0), 1)
   out <- apply(out,2,function(x) rgb(x[1],x[2],x[3]))
   if ((FALSE)&&((light-dark)/n<0.005))
      out <- sample(out)
  # if ((length(value))&&(nneg>npos))
  #    return(rev(out))
   if (is.na(inv)) {
      cond1 <- !length(value[value>0]) & length(value[value<0])>2
      inv <- bg<128 | cond1
   }
   if (inv)
      return(rev(out))
   out
}
