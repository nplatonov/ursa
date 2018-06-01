'.rename.files.and.classes' <- function() {
   require(plutil)
   if (FALSE) { ## rename files
      src <- "classImage"
      dst <- "classRaster"
      a <- filelist(paste0("^",src,".+\\.R$"))
      if (length(a)) {
         b <- gsub(src,dst,a,ignore.case=TRUE,perl=TRUE,fixed=FALSE,useBytes=FALSE)
         print(data.frame(src=a,dst=b))
         file.rename(a,b)
      }
   }
   if (!FALSE) {
     ## '\\.spatialize' --> 'spatialize'
     ## 'ursa:::spatialize' --> 'spatialize'
      src <- "open_ncdf"
      dst <- "open_nc"
      toWrite <- FALSE
      ind <- as.character(c(1,2,3,4,5,6,7,8,9,10,11,12))
      dpath1 <- c('1'="C:/platt/R/ursa-package/ursa/R"
                 ,'2'="C:/platt/R/ursa-package/ursa/man"
                 ,'3'="C:/platt/R/ursa-package/ursa/example"
                 ,'4'="C:/platt/R")
      dpath2 <- c('5'="C:/platt/R/ursa-package/land-polygons"
                 ,'6'="C:/platt/R/ursa-package/run"
                 ,'7'="C:/platt/misc"
                 ,'8'="D:/RAS/2018"
                 ,'9'="D:/RAS/2017"
                 ,'10'="D:/DATA"
                 ,'11'="D:/NRT"
                 ,'12'="D:/update")
      ind1 <- which(names(dpath1) %in% ind)
      ind2 <- which(names(dpath2) %in% ind)
      list1 <- list.files(path=dpath1[ind1],pattern="^[a-z].+\\.(R|Rd)$"
                       ,ignore.case=TRUE,recursive=FALSE,full.names=TRUE)
      list2 <- list.files(path=dpath2[ind2],pattern=".+\\.R$"
                       ,ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
      res <- sapply(c(list1,list2),function(x){
         if (basename(x)=="plaster.syno.R")
            return(0L)
         a <- readLines(x,warn=FALSE)
         ind <- mygrep(src,a)
         if (length(ind)) {
            message(paste("=======",x,"======="))
            b1 <- a[ind]
            a[ind] <- gsub(src,dst,a[ind],ignore.case=TRUE
                  ,perl=TRUE,fixed=FALSE,useBytes=FALSE)
            b2 <- a[ind]
            message(paste(b1,collapse="\n"))
            message("-------------")
            message(paste(b2,collapse="\n"))
            if (toWrite)
               writeLines(a,con=x)
         }
         if (length(grep(paste0("^",src,"\\.(Rd|R|ex\\.R)$")
                        ,gsub("^$","",basename(x)),ignore.case=TRUE))) {
            y <- file.path(dirname(x),gsub(src,dst,basename(x)))
            message("********************")
            print(c(src=x,dst=y),quote=FALSE)
            message("********************")
            if (toWrite)
               file.rename(x,y)
         }
         length(ind)
      })
   }
   0L
}
invisible({
  # .a <- basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
   .a <- try(ursa:::.argv0())
   if ((!is.na(.a))&&(.a=="_rename20a660d5.R"))
      .rename.files.and.classes()
   rm(.a)
   NULL
})
