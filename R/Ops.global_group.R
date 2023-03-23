'global_mean' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("mean",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="mean"))
   res
}
'global_min' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("min",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="min"))
   res
}
'global_max' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("max",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="max"))
   res
}
'global_sd' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("sd",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="sd"))
   res
}
'global_sum' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("sum",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="sum"))
   res
}
'global_median' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   if (!.lgrep("na\\.rm",names(arglist)))
      arglist$na.rm <- TRUE
   res <- try(do.call("median",c(list(c(x$value)),arglist)))
   if (inherits(res,"try-error"))
      res <- NA
   if (ursa)
      return(ursa_new(res,bandname="median"))
   res
}
'global_n' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   res <- length(na.omit(c(x$value)))
   if (ursa)
      return(ursa_new(res,bandname="n"))
   res
}
'global_nNA' <- function(x,ursa=FALSE,...) {
   arglist <- list(...)
   res <- length(which(is.na(c(x$value))))
   if (ursa)
      return(ursa_new(res,bandname="n"))
   res
}
'global_quantile' <- function(x,ursa=FALSE,...) {
   v <- quantile(na.omit(c(ursa_value(x))),...)
   if (!ursa)
      return(v)
   ursa(v,bands=length(v))
}
'global_range' <- function(x,ursa=FALSE,...)
   c(global_min(x,ursa=ursa,...),global_max(x,ursa=ursa,...))


## code below is proposal to reduce COPY/PASTE, but doesn't work correctly for 'range'
## try mget(names(match.call())[-1]) instead of as.list(match.call())[-1]
'.global_fun1' <- '.global_fun2'<- 
     function(x,ursa=FALSE,...) do.call(".global_common",as.list(match.call()))
'.global_common' <- function(...) {
   rel <- as.list(sys.call())[-1]
   fun <- .gsub("^global\\.","",as.character(rel[[1]]))
   rel <- rel[-1]
   obj <- .getPrm(rel,name="x",class="name",default=NULL)
   rel <- rel[-1]
   if (length(rel)) {
      scalar <- !.getPrm(rel,name="ursa",default=FALSE)
      if (length(ind <- grep("ursa",names(rel))))
         rel <- rel[-ind]
      if (!.lgrep("na\\.rm",names(rel)))
         rel$na.rm <- TRUE
   }
   else {
      rel <- list(na.rm=TRUE)
      scalar <- TRUE
   }
   rel <- c(list(ursa_value(eval(obj))),rel)
   res <- do.call(fun,rel)
   if (scalar)
      return(res)
   ursa_new(res,bandname=fun)
}
