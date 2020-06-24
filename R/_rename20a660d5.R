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
      if (stage1 <- F) {
         src <- "\\$proj(4)*"
         dst <- "$crs"
      }
      else if (stage2 <- F) {
         src <- "crsstring"
         dst <- "proj4string"
      }
      else if (stage3 <- F) {
         src <- "proj4="
         dst <- "crs="
      }
      else if (stage4 <- F) {
         src <- "\\\"proj4\\\""
         dst <- "\"crs\""
      }
      else if (stage5 <- F) {
         src <- "crs=="
         dst <- "ZZZ=="
      }
      else {
         stop("please select stage")
      }
      toWrite <- FALSE
      ind <- "1" # as.character(c(1,2,3,4,5,6,7,8,9,10,11,12))
      dpath1 <- c('1'="C:/platt/R/ursa-package/ursa/R"
                 ,'2'="C:/platt/R/ursa-package/ursa/man"
                 ,'3'="C:/platt/R/ursa-package/example"
                 ,'4'="C:/platt/R"
                 )
      dpath2 <- c('5'="C:/platt/R/ursa-package/land-polygons"
                 ,'6'="C:/platt/R/ursa-package/run"
                 ,'7'="C:/platt/misc"
                 ,'8'="D:/DATA"
                 ,'9'="D:/NRT"
                 ,'10'="D:/update"
                 ,'11'="D:/RAS/2020"
                 ,'12'="D:/RAS/2019"
                 )
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
