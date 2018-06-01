'.rename.files.and.classes' <- function() {
   if (FALSE) { ## rename files
      src <- "classImage"
      dst <- "classRaster"
      a <- filelist(paste0("^",src,".+\\.R$"))
      if (length(a)) {
         b <- mygsub(src,dst,a)
         print(data.frame(src=a,dst=b))
         file.rename(a,b)
      }
   }
   if (FALSE) {
      src <- "ursa_crs"
      dst <- "session_grid"
      list1 <- filelist("^[a-z].+\\.R$")
      res <- sapply(list1,function(x){
         a <- readLines(x)
         ind <- mygrep(src,a)
         if (length(ind)) {
            print(x)
            print(a[ind])
            a[ind] <- mygsub(src,dst,a[ind])
           # writeLines(a,con=x)
         }
         length(ind)
      })
   }
   0L
}
invisible({
   .a <- basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
   if ((!is.na(.a))&&(.a=="_rename20a660d5.R"))
      .rename.files.and.classes()
   rm(.a)
   NULL
})
