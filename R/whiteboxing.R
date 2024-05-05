'whiteboxing' <- function(tool_name,...) {
   if (is.character(tool_name))
      prm <- c(tool_name,list(...))
   else {
      prm <- c(list(...),list(tool_name))
   }
   ind <- which(sapply(prm,function(x) (is.character(x))&&(!grepl(".+\\.tif$",x))))
   if (length(ind)==1) {
      tool_name <- prm[[ind]]
      prm <- prm[-ind]
   }
   else {
      stop("cannot extract 'tool_name'")
   }
   verbose <- .getPrm(prm,name="verbose",default=F)
   wbt <- !.isPackageInUse() & .getPrm(prm,name="^wbt$",default=F)
   if (!wbt) {
      if (!requireNamespace("whitebox",quietly=.isPackageInUse()))
         stop("Suggested package `whitebox` is required for this operation")
   }
   else {
      if (!.require("whitebox",quietly=.isPackageInUse()))
         stop("Suggested package `whitebox` is required for this operation")
   }
  # data("wbttools",package="whitebox")
  # if (is.na(indT <- match(tool_name,wbttools$tool_name))) {
  #    if (is.na(indF <- match(tool_name,wbttools$function_name))) {
  #       return(NULL)
  #    }
  #    tool_name <- wbttools$tool_name[indF]
  # }
   if (grepl("\\w_\\w",tool_name)) {
      if (!grepl("^wbt_\\w",tool_name))
         tool_name <- paste0("wbt_",tool_name)
   }
   tool_name <- gsub("_(\\w)","\\U\\1",gsub("(^wbt)(_.+)$","\\2",tool_name),perl=TRUE)
   if (wbt)
      prm <- prm[grep("^wbt$",names(prm),invert=TRUE)]
   pname <- names(prm)
   if (is.null(pname))
      pname <- rep("",length(prm))
   if (!nchar(pname[1]))
      pname[1] <- "input"
   for (i in seq_along(prm)) {
      if (is_ursa(prm[[i]])) {
         fname <- tempfile(fileext=".tif")
         write_gdal(prm[[i]],fname,COMPRESS="LZW",TILED="NO")
         prm[[i]] <- fname
      }
      if (!nchar(pname[i])) {
         p <- prm[[i]]
         if (!is.null(names(p))) {
            pname[i] <- names(p)
            prm[[i]] <- unname(p)
         }
         else {
            sp <- strsplit(p,split="=")[[1]]
            if (length(sp)==2) {
               prm[[i]] <- sp[2]
               pname[i] <- sp[1]
            }
         }
      }
   }
   pname <- gsub("^--","",pname)
   names(prm) <- pname
   if (isUrsa <- !length(ind <- grep("^output$",names(prm)))) {
      prm[["output"]] <- tempfile(fileext=".tif")
   }
   if (is.logical(prm[["output"]])) {
      isUrsa <- !isTRUE(prm[["output"]])
      prm[["output"]] <- tempfile(fileext=".tif")
   }
   ret <- prm[["output"]]
   if (!wbt) {
      names(prm) <- paste0("--",names(prm))
      prm <- paste0(names(prm),"=",sapply(prm,\(x) x))
      prm <- gsub("=TRUE","",prm)
      prm <- gsub("--.+=FALSE","",prm)
      prm <- paste(prm[nchar(prm)>0],collapse=" ")
      if (verbose)
         cat(paste0(tool_name,": ",prm,"\n"))
      whitebox::wbt_run_tool(tool_name=tool_name,args=prm)
   }
   else {
      if (verbose)
         str(prm)
      a <- do.call("wbt",c(tool_name,prm))
      attr(ret,"wbt") <- a
   }
   if (!isUrsa)
      return(ret)
   ret <- read_gdal(ret)
   names(ret) <- tool_name
   ret
}
