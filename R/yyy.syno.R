'.syn' <- function(new,kind=0,...) { ## function names synonyms
   if (!FALSE) {
      res <- .parentFunc()
      old <- res[length(res)]
      res <- res[-c(1,length(res))]
      parent <- paste(paste0(res,"()"),collapse=" -> ")
      msg <- paste0("In ",parent,": '",old,"()' is obsolete. Use '",new,"()'")
      if (kind==2)
      {
         op <- options(warn=0)
         warning(msg,call.=!TRUE)
         on.exit(options(op))
      }
      else if (kind==1)
         message(msg)
   }
   do.call(new,list(...))
}
'.parentFunc' <- function() {
   op <- options(warn=0)
   n <- sys.parent()-3:1+1
   n <- n[n>0]
   res <- as.character(sys.call(which=0))[1]
   for (i in n) {
      res <- c(res,as.character(sys.call(which=i))[1])
   }
   options(op)
  # class(res) <- "parentFunc"
   res
}
# '.print.ParentFunc' <- function(x,...) message(paste(x[-1],collapse=" -> "))
