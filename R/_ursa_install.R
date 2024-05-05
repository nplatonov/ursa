# stop("Under development")
'.undoc_functions' <- function() {
  # list1 <- readLines(system.file("NAMESPACE",package="ursa"))
   list1 <- readLines(file.path("./ursa","NAMESPACE"))
   list1 <- grep("^export\\(",list1,value=TRUE)
   list1 <- gsub("^export\\(\\\"(.+)\\\"\\)","\\1",list1)
   ns <- asNamespace("ursa")
   list2 <- ls(envir=ns)
   unloadNamespace(ns)
   list2 <- grep("^[A-Za-z]",list2,value=TRUE)
   list2 <- grep("\\.(ursa(Raster|Grid|CRS|ColorTable|Connection|Numeric|Category|Stack|ProgressBar))"
                ,list2,value=TRUE,invert=TRUE)
   list2 <- grep("^(as\\.Raster|djqwotrhfndh)\\.",list2,value=TRUE,invert=TRUE)
   if (length(list3 <- grep("^C_.+",list2,value=TRUE,invert=FALSE))) {
      list3 <- paste0(list3," <- \"",gsub("^C_","",list3),"\"")
      writeLines(list3,"ursa/R/_ursa_C_registration.R")
   }
   list2 <- grep("^C_.+",list2,value=TRUE,invert=TRUE)
   list2 <- list2[which(is.na(match(list2,list1)))]
   list2
}
# setwd("../..");.undoc_functions();q()
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
  # a1 <- c(grep("^\\.on.+",a,value=TRUE),a1)
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
   wd <- setwd("C:/platt/R/ursa-package");on.exit(setwd(wd))
   src <- "ursa/DESCRIPTION"
   dst <- tempfile()
   file.copy(src,dst)
   wd2 <- setwd("news")
   source("versionConsistence.R")
   setwd(wd2)
   if (requireNamespace("tools")) {
      toWrite <- TRUE
      md5fname <- "ursa/R/_md5"
      new <- tools::md5sum(dir(path="ursa"
                             # ,pattern="(^NAMESPACE|^DESCRIPTION|\\.(R|c|md))$"
                              ,recursive=TRUE,full.names=TRUE))
      new <- data.frame(md5=unname(new),file=names(new))
      new <- new[new$file!=md5fname,]
      if (file.exists(md5fname)) {
         old <- read.table(md5fname,header=FALSE)
         colnames(old) <- colnames(new)
         if ((identical(new$file,old$file))&&(identical(new$md5,old$md5)))
            toWrite <- FALSE
      }
      if (toWrite)
         write.table(new,md5fname,quote=FALSE
                    ,col.names=FALSE,row.names=FALSE)
      else {
         cat("`ursa` is up-to-date\n")
         file.rename(dst,src)
         return(NULL)
      }
   }
   if (requireNamespace("ursa"))
      stopifnot(!.generate_namespace(verbose=FALSE))
   patt <- "^ursa_.*(\\.tar\\.gz|\\.zip)$"
   nul <- file.remove(dir(pattern=patt))
   desc <- readLines(src)
   pattV <- "^(Version:\\s*)(\\S+)\\s*$"
   if (subVersion <- length(indC <- grep(pattV,desc))>0) {
      cfile <- "ursa/.counter"
      ver <- unlist(package_version(gsub(pattV,"\\2",desc[indC])))
      if (length(ver)==3)
         ver[3] <- ver[3]-1
      ver <- paste(ver[1:3],collapse=".")
      counter <- as.integer(readLines(cfile))
      desc[indC] <- gsub(pattV,sprintf("\\1%s-%04d",ver,counter),desc[indC])
      print(desc[indC],quote=FALSE)
      writeLines(desc,src)
   }
   system("R --vanilla CMD build ursa")
  # if (file.exists(dst))
  #    file.rename(dst,src)
   pkg <- tail(dir(pattern=patt))
   if (length(pkg)!=1)
      return(NULL)
  # if (length(indC))
  #    writeLines(sprintf("%04d",counter+1L),cfile)
   opt1 <- "--fake" ## --no-multiarch
   opt2 <- "--no-html"
   opt3 <- "--no-html --build"
   checkArch <- dir(path=file.path(Sys.getenv("R_HOME"),"bin")
                   ,pattern="Rterm",recursive=TRUE,full.names=TRUE)
   if (length(checkArch)==1)
      opt3 <- paste(opt3,"--no-multiarch")
   if (!TRUE) {
      src <- file.path(Sys.getenv("HOME"),".R","Makevars")
      a <- readLines(src)
      a <- a[nchar(a)>0]
      ind <- regexpr("#",a)
      ind[ind<0] <- 32767
      a <- substr(a,1,ind-1)
      a <- a[nchar(a)>0]
      a <- strsplit(a,"\\s*=\\s*")
      prm <- lapply(a,function(x) x[2])
      names(prm) <- lapply(a,function(x) x[1])
      do.call("Sys.setenv",prm)
      prm <- lapply(prm,function(x) "")
      install.packages(pkg,INSTALL_opts=opt2,lib=.libPaths()[1],repos=NULL)
      do.call("Sys.setenv",prm)
   }
   else {
      Sys.setenv(R_LIBS_USER=.libPaths()[1])
     # Sys.setenv(BINPREF=Sys.getenv("R_BINPREF"))
      ret <- system2("R",c("--vanilla","CMD","INSTALL",opt3,pkg)[-1])
     # system(paste("R","--vanilla","CMD","INSTALL",opt2,pkg))
      if ((!ret)&&(subVersion)) {
         writeLines(sprintf("%04d",counter+1L),cfile)
      }
   }
  # file.remove(pkg)
   NULL
}
invisible({
  # .a <- try(ursa:::.argv0())
   .argv0 <- commandArgs(FALSE)
   .ind1 <- grep("^-f$",.argv0)
   .ind2 <- grep("(.+/_|.+\\\\_|^_)ursa_install.R$",.argv0)
   .ind3 <- grep("--file=.*_ursa_install.R$",.argv0)
   if ((length(.ind3)==1)||((length(.ind1)==1)&&(length(.ind2)==1)&&(.ind1+1L==.ind2)))
      .buildAndInstall()
   else {
     # .buildAndInstall()
      print("mysource")
     # try(Sys.setenv(R_RMAP_TEMPLATE=
     #     file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")))
   }
  # rm(.a)
   NULL
})
