# utils::globalVariables("wbttools")
'.ursaToolSetDummyFunction' <- function() NULL
#try(Sys.setenv(R_PLASTER_TEMPLATE=
#     file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")))
# try(Sys.setenv(R_PLASTER_TEMPLATE=system.file("inst","template",package="ursa")

.onLoad.blank <- function(lib, pkg) {
   invisible(0L)
}
.onLoad <- function(lib, pkg) {
   compiler::enableJIT(0) ## speed up if 'ByteCompile: no' in "DESCRIPTION"
  # print("ursa -- .onLoad")
   p <- proc.time()
   options(ursaTimeStart=p,ursaTimeDelta=p) # ,ursaForceSF=TRUE 
   rm(p)
   options(ursaNoticeMatchCall=FALSE & !.isPackageInUse())
   if (!.isPackageInUse())
      options(show.error.messages=TRUE)
   if (is.null(getOption("ursaProj4Legacy")))
      options(ursaProj4Legacy=TRUE)
   if (is.null(getOption("ursaForceWKT")))
      options(ursaForceWKT=FALSE) ## sf_project: proj4 is faster than WKT
  # if (is.null(getOption("ursaTolerance")))
  #    options(ursaTolerance=1e-8)
   if ((FALSE)&&(nchar(system.file(package="proj4"))>0)) {
      .forceProj4package(TRUE)
   }
  # session_pngviewer()
   fpath <- getOption("ursaCacheDir") ## e.g., from ~/.Rprofile
   if (is.null(fpath))
      try(options(ursaCacheDir=tempdir())) 
   else
      if (!file.exists(fpath))
         dir.create(fpath)
  ## ursaCacheDir=file.path(dirname(tempdir()),"RtmpUrsaCache") ## out of CRAN policy
   .ursaCacheDirClear()
   session_tempdir()
  # if ((FALSE)&&(interactive()))
  #    print(data.frame(pngviewer=session_pngviewer()
  #                    ,tempdir=session_tempdir()
  #                    ,row.names="session"))
  # welcome2 <- .elapsedTime("ursa -- onload 1111",toPrint=FALSE)
  # fpath <- file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")
   fpath0 <- system.file("requisite",package="ursa")
   fpath <- getOption("ursaRequisite") ## e.g., from ~/.Rprofile
   if ((!is.null(fpath))&&(file.exists(fpath))) {
     # ok <- try(Sys.setenv(R_RMAP_TEMPLATE=fpath))
      ok <- try(options(ursaRequisite=fpath))
      if (!inherits(ok,"try-error")) {
         sapply(.dir(path=fpath0),function(x)
                               file.copy(file.path(fpath0,x),file.path(fpath,x)
                                        ,overwrite=FALSE,copy.date=TRUE))
        # if (("plutil" %in% loadedNamespaces())&&(.isPackageInUse())) {
        #    NULL
        # }
        # spatialize <<- ursa:::spatialize
        # assign("spatialize",ursa:::spatialize,envir=.GlobalEnv) ## OK
        # assign("spatialize",get("spatialize"),envir=.GlobalEnv) ## OK
        # assign("spatialize",get("ursa:::spatialize"),envir=.GlobalEnv) ## FAIL
         return(invisible(0L))
      }
   }
  # try(Sys.setenv(R_RMAP_TEMPLATE=fpath))
   try(options(ursaRequisite=fpath0))
   invisible(0L)
}
.onAttach <- function(lib, pkg) { ## FAILED for 'Rscript -e "ursa::display()"'
  # print("ursa -- .onAttach")
  # welcome <- .elapsedTime("ursa -- attach 2222",toPrint=FALSE)
  # packageStartupMessage(welcome,appendLF=FALSE)
   invisible(0L)
}
.Last.hide <- function() {
   message("ursa -- last")
   if (!FALSE)
   {
      delafter <- getOption("ursaPngDelafter")
      fileout <- getOption("ursaPngFileout")
      if ((is.logical(delafter))&&(is.character(fileout))&&(delafter)&&(file.exists(fileout)))
      {
        # dev.off()
         graphics.off()
        if (!file.remove(fileout))
           message(sprintf("'ursa' package message: Unable to remove file '%s'.",fileout))
      }
   }
   con <- showConnections(all=!FALSE)
   ind <- which(!is.na(match(con[,"class"],"file")))
   if ((!FALSE)&&(length(ind)))
   {
      con <- con[ind,,drop=FALSE]
      for (i in seq(nrow(con)))
      {
         con2 <- con[i,,drop=FALSE]
        # close(getConnection(as.integer(rownames(con2)))) ## del
         fname <- con2[,"description"]
         if (length(grep("\\.unpacked(.*)\\~$",fname)))
         {
            close(getConnection(as.integer(rownames(con2)))) ## ins
            if (!file.remove(fname))
               message(sprintf("'ursa' package message: Unable to remove file '%s'."
                              ,fname))
         }
      }
   }
}
.noGenerics <- TRUE
.onUnload <- function(libpath) {
  # message("ursa -- unload")
   library.dynam.unload("ursa",libpath)
}
.onDetach <- function(libpath) {
  # message("ursa -- detach")
}
