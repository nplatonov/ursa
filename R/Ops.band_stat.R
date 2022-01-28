'band_stat' <- function(x,grid=FALSE,raw=FALSE)
{
   if (!is.ursa(x))
      return(NULL)
  # inMemory <- !is.null(dim(x$value))
   if ((!.in.memory(x))||(grid))
      return(ursa_info(x))
   n <- x$dim[2] # length(x$name)
   if (.is.con(x$con)) {
      ind <- x$con$posZ
      if (anyNA(ind))
         ind <- seq(n)
      else
         n <- length(ind)
   }
   else
      ind <- seq(n)
   id <- sprintf(paste0("[%0",nchar(as.character(n)),"d]"),ind)
   myname <- names(x)
   if ((length(na.omit(myname))!=n))
      myname <- rep("____",n)
  # n <- dim(x$value)[2]
   if (n==0) {
      result <- data.frame(name=character(0),mean=rep(NA,0),sd=rep(NA,0)
                          ,sum=rep(NA,0),min=rep(NA,0),max=rep(NA,0)
                          ,n=rep(NA,0),nNA=rep(NA,0),stringsAsFactors=FALSE)
      return(result)
   }
   else {
      
      id0 <- if (length(id)==length(unique(id))) id else make.unique(id,sep=".")
      result <- data.frame(name=myname,mean=NA,sd=NA,sum=NA
                          ,min=NA,max=NA,n=NA,nNA=NA,row.names=id0
                          ,stringsAsFactors=FALSE)
   }
   if ((TRUE)&&(!sum(nchar(myname)))) {
     # result <- subset(result,select=-c(name))
     # result <- result[,-c("name")] ## -- 20211209
      result$name <- rep("____",n) ## ++ 20211209
   }
   for (i in seq(along=myname))
   {
     # str(x$value[,i])
      if (raw)
         tmp <- as.numeric(x$value[,i])
      else
         tmp <- as.numeric(.extract(x)$value[,i])
      tmpNA <- length(tmp[is.na(tmp)])
      tmp <- tmp[!is.na(tmp)]
      if (length(tmp))
      {
         result[i,"sum"] <- sum(tmp)
         result[i,"mean"] <- mean(tmp)
         result[i,"sd"] <- sd(tmp)
         result[i,"min"] <- min(tmp)
         result[i,"max"] <- max(tmp)
      }
      result[i,"n"] <- length(tmp)
      result[i,"nNA"] <- tmpNA
      rm(tmp)
   }
   sparse <- attr(x$value,"sparse")
   if (is.null(sparse))
      ni <- 0L
   else if (all(sparse<0))
      ni <- length(sparse)
   else if (all(sparse>0))
      ni <- with(x$grid,columns*rows)-length(sparse)
   else
      ni <- 0L
   result[,"nNA"] <- result[,"nNA"]+ni
   result
}
