'.undoc_functions' <- function() {
  # list1 <- readLines(system.file("NAMESPACE",package="ursa"))
   list1 <- readLines(file.path("./ursa","NAMESPACE"))
   list1 <- grep("^export\\(",list1,value=TRUE)
   list1 <- gsub("^export\\(\\\"(.+)\\\"\\)","\\1",list1)
   ns <- asNamespace("ursa")
   list2 <- ls(envir=ns)
   unloadNamespace(ns)
   list2 <- grep("^[A-Za-z]",list2,value=TRUE)
   list2 <- grep("\\.(ursa(Raster|Grid|ColorTable|Connection|Numeric|Category|Stack))"
                ,list2,value=TRUE,invert=TRUE)
   list2 <- grep("^(as\\.Raster|djqwotrhfndh)\\.",list2,value=TRUE,invert=TRUE)
   list2 <- list2[which(is.na(match(list2,list1)))]
   list2
}
'.generate_namespace' <- function(verbose=FALSE) {
   U <- file.path("./ursa")
   Fns <- file.path(U,"NAMESPACE.")
   if (file.exists(Fns))
      file.remove(Fns)
  # a <- tools::checkDocFiles(dir=U)
  # a <- tools::checkS3methods(dir=U)
   a <- names(attributes(tools::codoc(dir=U))$function_args_in_code)
  # print(grep("^session",a,value=TRUE))
   patt <- "^(\\[+|\\w.+)\\.(\\w+)$"
   clA <- a #grep("^(as|is)\\..+",a,value=TRUE,invert=TRUE)
   clA <- grep(patt,clA,value=TRUE)
   clA <- gsub(patt,"\\2",clA)
   clA <- unique(clA)
   clA <- grep("^(ursa|Raster)$",clA,value=TRUE,invert=TRUE)
   clP <- paste0("\\.(",paste(clA,collapse="|"),")$")
   if (verbose)
      message(clP)
   a1 <- grep("^[A-Za-z].+",a,value=TRUE)
   a1 <- grep(clP,a1,invert=TRUE,value=TRUE)
   a1 <- c(grep("^\\.on.+",a,value=TRUE),a1)
   if (FALSE) {
      b <- tools::undoc(dir=U)
      if (length(b[[1]])) {
         print(b)
         if (length(ind <- match(b[[1]],a1))>0)
            a1 <- a1[-ind]
      }
   }
   a1 <- paste0("export(",dQuote(a1),")")
   if (verbose)
      message(paste(c(head(a1),"...",tail(a1),"-----"),collapse="\n"))
   #print(a[ind])
   a2 <- grep(paste0(".+",clP),a,value=TRUE)
   a2 <- grep("^\\.",a2,value=TRUE,invert=TRUE)
   #head(a2)
   patt2 <- paste0("(^.+)",clP)
   a2 <- paste0("S3method(",dQuote(gsub(patt2,"\\1",a2)),",",gsub(patt2,"\\2",a2),")")
   res <- readLines(file.path(U,"R/_NAMESPACE.header"))
   res <- c(res,a1,"",a2)
   if (verbose) {
      message(paste(c(head(a2),"...",tail(a2),"-----"),collapse="\n"))
      if (file.exists(file.path(U,"R/_NAMESPACE.pattern"))) {
         ns1 <- grep("^S3method",res,value=TRUE)
         ns2 <- grep("^S3method",readLines(file.path(U,"R/_NAMESPACE.pattern")),value=TRUE)
         ind12 <- match(ns1,ns2)
         ind21 <- match(ns2,ns1)
         if (length(which(is.na(ind12)))+length(which(is.na(ind12)))>0) {
            str(ns1)
            str(ns2)
            print(ind12)
            print(ind21)
            message("----")
            return(1L)
         }
      }
   }
   writeLines(res,Fns)
   if (TRUE) {
      if (verbose)
         message("double check")
      b <- tools::undoc(dir=U)
      if (length(b[[1]])) {
         if (verbose)
            print(b)
         patt <- paste0("export\\(\"(",paste0(b[[1]],collapse="|"),")\"\\)")
         a <- readLines(Fns)
        # if (length(ind <- grep(paste0("export(\"(",paste0(b,collapse="|"),")\")"),a))>0) {
         if (length(ind <- grep(patt,a))>0) {
            if (verbose)
               str(ind)
            a <- a[-ind]
         }
         writeLines(a,Fns)
         writeLines(b[[1]],file.path(U,"R/_NAMESPACE.undoc"))
      }
      if (!FALSE) {
         b2 <- .undoc_functions()
         if (length(b2)) {
            message("-----------------\nUndocumented:")
            print(b2,quote=FALSE)
            message("-----------------")
            patt <- paste0("export\\(\"(",paste0(b[[1]],collapse="|"),")\"\\)")
            a <- readLines(Fns)
            if (length(ind <- grep(patt,a))>0)
               a <- a[-ind]
            writeLines(a,Fns)
            writeLines(b2,file.path(U,"R/_NAMESPACE.undoc"))
         }
      }
   }
   0L
}
'.buildAndInstall' <- function() {
   wd <- setwd("C:/platt/R/ursa-package")
   if (requireNamespace("ursa"))
   stopifnot(!.generate_namespace(verbose=FALSE))
   patt <- "^ursa_.*\\.tar\\.gz$"
   a <- file.remove(dir(pattern=patt))
   system("R --vanilla CMD build ursa")
   pkg <- tail(plutil::filelist(patt))
   if (length(pkg)==1) {
      opt1 <- "--fake" ## --no-multiarch
      opt2 <- "--no-html"
      opt3 <- "--no-html --build"
      system(paste("R --vanilla CMD INSTALL",opt2,pkg)) 
      file.remove(pkg)
   }
   setwd(wd)
   NULL
}
invisible({
   .a <- try(ursa:::.argv0())
  # .a <- basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
   if ((!is.na(.a))&&(.a=="_ursa_install.R"))
      .buildAndInstall()
   else {
     # print("mysource")
     # try(Sys.setenv(R_RMAP_TEMPLATE=
     #     file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")))
   }
   rm(.a)
   NULL
})
