require(plutil)
invisible({
   list1 <- filelist(path="../R/",pattern="^[a-z].+\\.R$")
   list1 <- mygrep("^zzz",list1,value=TRUE,inv=TRUE)
   n1 <- length(list1)
   list2 <- filelist(path="../man/",pattern=".+\\.Rd$")
   n2 <- length(list2)
   name1 <- gsub("\\.R$","",list1)
   name2 <- gsub("\\.Rd$","",list2)
   ind12 <- match(name1,name2)
   ind21 <- match(name2,name1)
   name3 <- name2[which(is.na(ind21))]
   if (length(name3))
      print(name3)
   else {
      ind <- which(is.na(ind12))
      print(name1[ind])
      msg <- paste(paste("source files:",n1)
                ,sprintf("manuals are ready for: %d (%.1f%%)",n2,round(100*n2/n1,1))
                ,sprintf("remain man files: %d (%.1f%%)",n1-n2,round(100*(1-n2/n1),1))
                ,sep="\n")
      if (FALSE)
         msgShow(msg)
      else
         message(msg)
   }
})
