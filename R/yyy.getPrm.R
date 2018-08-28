'.getPrm' <- function(link,name="",kwd="",class=NA,index=0L,default=NULL
                     ,valid=NULL,coerce=TRUE,verbose=FALSE) {
   
   if (benchmark <- FALSE) {
      .cum <- getOption("ursaPrm")
      if (is.null(.cum))
         .cum <- list(time=0,n=0L)
      .begin <- proc.time()[3]
      on.exit({
         .delta <- proc.time()[3]-.begin
         if (.delta>0) {
            print(unname(.delta),digits=6)
            print(name)
         }
         options(ursaPrm=list(time=.cum$time+.delta,n=.cum$n+1L))
      })
   }
   if (index>0)
      return(link[[index]])
   if (!((is.character(class))||(is.list(class))))
      class <- class(default)
   myname <- names(link)
   if (is.null(myname))
      myname <- rep("",length(link))
   if (nchar(kwd)) {
      if (.lgrep("\\$$",kwd))
         kwd <- substr(kwd,1,nchar(kwd)-1)
      name <- c(paste0(name,"\\.",kwd),paste0(kwd,"\\.",name)
               ,paste0("^",name,"$"))[2:3]
      if (.lgrep("\\^\\$",name))
         name <- c(kwd,name)
   }
   if (verbose)
      print(name)
   xName <- expand.grid(where=myname,what=name,KEEP.OUT.ATTRS=FALSE)
   if (verbose)
      print(xName)
   ind <- which(apply(xName,1,function(x) .lgrep(x["what"],x["where"]))>0)
  # ind <- which(apply(xName,1,function(x) length(which(!is.na(match(x["what"],x["where"])))))>0)
   if ((length(valid))&&(is.character(valid))&&(is.null(default)))
      default <- match.arg(valid,valid)
   if (!length(ind)) {
      return(default)
   }
   if (verbose)
      print(xName[ind[1],])
   i <- match(xName[ind[1],"where"],myname)
   cl <- class(link[[i]])
   if ("ursaStack" %in% cl)
      cl <- "list"
   isList <- is.list(link[[i]])
   if (!is.list(class))
      class <- list(class)
   for (j in seq_along(class)) {
      toInteger <- FALSE
      toLogical <- FALSE
      toNumeric <- FALSE
      cl2 <- class[[j]]
      if (("NULL" %in% cl2)&&(length(valid)))
         cl2 <- class(valid)
      if (("list" %in% cl2)&&(!("list" %in% cl)))
         next
     # if (("list" %in% cl2)&&(!isList))
     #    next
      if (("list" %in% cl)&&(!(("list" %in% cl2)||("" %in% cl2))))
         next
     # if ((isList)&&(!(("list" %in% cl2)||("" %in% cl2))))
     #    next
      if ("" %in% cl2)
         return(link[[i]])
      cl2 <- .grep("list",cl2,value=TRUE,invert=TRUE)
      if (verbose) {
         print(c(pat=name,arg=myname[i],cl=cl,cl2=cl2),quote=FALSE)
      }
      if (("integer" %in% cl2)&&("numeric" %in% cl)) {
         toInteger <- TRUE
        # m2 <- all(na.omit(.is.integer(link[[i]]))) ## removed 20161228
         m2 <- all(.is.integer(na.omit(link[[i]]))) ## added 20161228
      }
      else if (("logical" %in% cl2)&&
               (!(any(c("integer","numeric") %in% cl2)))&&
               (any(c("integer","numeric") %in% cl))) {
         toLogical <- TRUE
         m2 <- TRUE #as.logical(link[[i]])
      }
     # else if (isList) { ## ("list" %in% cl)
      else if ("list" %in% cl) {
        # m2 <- all(sapply(link[[i]],function(x) class(x)) %in% cl2) ## removed 2016-06-10
         m2 <- all(sapply(link[[i]],function(x) any(class(x) %in% cl2))) ## added 2016-06-10
      }
      else if ("numeric" %in% cl2){
         toNumeric <- TRUE
         m2 <- is.numeric(link[[i]]) | is.logical(link[[i]])
      }
      else {
         m2 <- length(na.omit(match(cl2,cl)))>0 ##cl2 %in% cl
      }
      if (verbose)
         print(m2)
      if (!m2) {
         if (length(valid)) {
            if (!any(cl %in% cl2))
               next
            w <- paste(paste0(.sQuote(name),"="
                             ,ifelse(is.character(link[[i]])
                                          ,.dQuote(link[[i]]),link[[i]]),".")
                      ,"Expected: ",paste0(paste(valid,collapse=", "),".")
                      ,"Use default:",default)
            opW <- options(warn=1)
            warning(w)
            options(opW)
         }
         next
      }
      if (length(valid)) {
         m2 <- FALSE
         if ((is.numeric(valid))||(is.numeric(link[[i]]))) {
            ind <- na.omit(which(as.numeric(link[[i]]) %in% as.numeric(valid)))
            if (length(ind)) {
               link[[i]] <- link[[i]][ind]
               m2 <- TRUE
            }
            else if (nchar(myname[i])) {
               w <- paste(paste0(.sQuote(myname[i]),"=",link[[i]])
                         ,"is not in a list:"
                         ,paste0(paste(valid,collapse=", "),".")
                         ,"Use default:",default)
               opW <- options(warn=1)
               warning(w)
               options(opW)
            }
         }
         else if ((is.character(link[[i]]))||(is.character(valid))) {
            ind <- na.omit(pmatch(as.character(link[[i]])
                                 ,as.character(valid)))
            if (length(ind)) {
               link[[i]] <- unname(sapply(as.character(link[[i]])
                             ,function(x) match.arg(x,as.character(valid))))
               m2 <- TRUE
            }
            else if (nchar(myname[i])) {
               if (is.null(default))
                  default <- match.arg(valid,valid) ## valid[1]
               w <- paste(paste0(.sQuote(myname[[i]]),"=",.dQuote(link[[i]]))
                         ,"is not in a list:"
                         ,paste0(paste(.dQuote(valid),collapse=", "),".")
                         ,"Use default:",.dQuote(default))
               opW <- options(warn=1)
               warning(w)
               options(opW)
               print(link[[i]])
            }
         }
         if (!m2)
            next
      }
      if (!coerce)
         return(link[[i]])
      if (toInteger) {
         return(as.integer(link[[i]]))
      }
      else if (toLogical) {
         return(as.logical(link[[i]]))
      }
      else if (toNumeric) {
         return(as.numeric(link[[i]]))
      }
      else
         return(link[[i]])
   }
   default
}
