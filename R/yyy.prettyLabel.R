'.prettyLabel' <- function(x,ncol,min=NULL,max=NULL,onPanel=TRUE)
{
   if ((ncol==2L)&&(length(x)==2)) {
      y <- pretty(x,n=21)
      y <- y[c(1,length(y))]
      return(data.frame(at=y,lab=as.character(y),stringsAsFactors=FALSE))
   }
   saveCriterium <- 0.1
   finalY <- NULL
   finalLabel <- NULL
   saveCriterium <- 0.1
   for (pass in 1:2)
   {
      for (i in 2:(ncol+5))
      {
         for (s in c(10,12,15,16,20,25,30,40,50,60,70,80,90))
         {
            y <- s*pretty(x/s,n=i)
            if (!onPanel)
            {
               by <- diff(y[1:2])
               y <- c(y[1]-by/2,y+by/2)
            }
            if (length(y)>ncol)
               next
            for (i2 in -4:+4)
            {
               if (all(round((y*10^(i2))%%10,digits=6) %in% 0:10))
                  break;
            }
            if (i2<0)
               i2 <- 0
           # label <- sprintf(sprintf("%%.%df",i2),as.double(y))
           # if (length(label)!=length(unique(label)))
           #    label <- format(y)
            label <- format(y)
            if (pass==1)
            {
               if ((!is.null(min))&&(head(y,1)!=min))
                  next
               if ((!is.null(max))&&(tail(y,1)!=max))
                  next
            }
           # criterium <- length(y)/(0.1+abs(ratio-0.6))
            criterium <- 0L 
           # criterium <- criterium+diff(x)/(min(x)-min(y)+max(y)-max(x))
            criterium <- criterium+2/(1+abs(ncol-length(y)))
            criterium <- criterium+1/max(nchar(label))
            if ((length(y)>=length(finalY))&&(criterium>saveCriterium))
            {
               finalY <- y
               finalLabel <- label
               saveCriterium <- criterium
              # print(criterium)
            }
         }
      }
      if (!is.null(finalY))
         break
   }
   data.frame(at=finalY,lab=finalLabel)
}
'.deintervale' <- function(value,verbose=FALSE)
{
   if (is.ursa(value))
      value <- ursa_colortable(value)
   if (.is.colortable(value))
      value <- names(value)
   opW <- options(warn=-10);on.exit(options(opW))
   ind <- .grep("^\\d{2}\\.\\d{2}$",value)
   if (length(ind)==length(value)) ## date can be interpeted as numeric
      return(value)
   res <- as.numeric(value)
   if (!anyNA(res)) {
      if (.lgrep("^0\\d.*",value))
         return(value)
      return(res)
   }
   dev <- !FALSE
   if (!dev)
      patt <- "(<=|<|=|>|\\(|\\[|;|,|\\]|\\))" ## <= < = > >= [ ] ( ) ; ,
   else
      patt <- "(^(<=|<|>|>=).+$|^(\\(|\\[).+(\\]|\\))$)"
     # patt <- "^(\\(|\\[).+]$"
   found <- sum(grepl(patt,value,perl=TRUE))
   if (found>0) {
      if (dev) {
         ivalue <- value
        # ivalue <- gsub("^(<=|<|>|>=)","",ivalue) ## OK
        # ivalue <- gsub("^(\\(|\\[)","",ivalue)
        # ivalue <- gsub("(\\]|\\))$","",ivalue)
         ivalue <- gsub("^(<=|<|>|>=)(.+)$","\\2",ivalue)
         ivalue <- gsub("^(\\(|\\[)(.+)(\\]|\\))$","\\2",ivalue)
         l1 <- length(ivalue)
         ivalue <- unlist(strsplit(ivalue,split="[,;]"))
         ivalue <- gsub("^\\s+|\\s+$","",ivalue)
      }
      else {
         ivalue <- .gsub(patt," ",value)
         l1 <- length(ivalue)
         ivalue <- paste(ivalue,collapse=" ")
         ivalue <- unlist(strsplit(ivalue,split="\\s+"))
         ivalue <- ivalue[nchar(ivalue)>0]
      }
      invalid <- ((found<l1)||(length(unique(c(table(ivalue))))>1)) ## 20170609 intro
     # print(c(invalid=invalid))
      ivalue <- unique(ivalue) ## added 20161101
      ##~ leadingZero <- .grep("^0[1-9]",ivalue)
      ##~ if(!length(leadingZero))
         ##~ res <- as.numeric(ivalue)
      ##~ else
      res <- ivalue
      if ((anyNA(res))||(invalid)) {
         return(value)
      }
      l2 <- length(ivalue)
   }
   else {
      ivalue <- value
      l1 <- l2 <- length(ivalue)
   }
   leadingZero <- .grep("^0[1-9]",ivalue)
   if (!length(leadingZero))
      res <- as.numeric(ivalue)
   else
      res <- ivalue
   if (anyNA(res)) {
      if (l1==l2)
         return(ivalue)
      else if (l1==l2+1) ## ++ 20170107
         return(ivalue)
      else
         return(value)
   }
   ivalue <- res
   if (l1==l2)
      return(ivalue)
   if ((l1-1)*2!=l2)
   {
      if (is.numeric(ivalue))
         ivalue <- ivalue[match(sort(unique(ivalue)),ivalue)]
      else
         ivalue <- ivalue[match(unique(ivalue),ivalue)]
     # message("dev message: is parsing of intervals correct?")
      return(ivalue)
   }
   ivalue <- ivalue[seq(l2)%%2==1]
   if ((FALSE)&&(is.character(ivalue))) { ## proposed to introduce
      val <- as.integer(format(as.Date(paste0("2012",ivalue),format("%Y%d%b")),"%j"))
      if (!anyNA(val))
         ivalue <- val
   }
   ivalue
}
'.prettyNum' <- function(x,ncol=5) {
   a <- .prettyLabel(x,ncol)$at
   a <- a[which.min(abs(x-a))][1]
   a
}
