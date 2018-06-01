require(plutil)
invisible({
   require(ursa)
  # a <- lsf.str("package:ursa")
  # a <- ls("package:ursa")
  # print(a)
  # q()
   .examList <- ursa:::.dir("^[a-z].+\\.R$")
   .examOut <- "__runExamples.csv"
   if (!file.exists(.examOut)) {
      write.csv2(data.frame(fname=.examList,check=0L,exam=0L),.examOut
                ,quote=FALSE,row.names=FALSE)
   }
   .examData <- read.csv2(.examOut)
   ind <- which(.examData$check==2 | .examData$exam==2)
   toRepair <- length(ind)
   for (.examI in sample(seq(nrow(.examData)))) {
      .examNow <- .examData[.examI,,drop=TRUE]
      if ((toRepair)&&(.examNow$check!=2)&&(.examNow$exam!=2))
         next
      if ((.examNow$check %in% c(1))&&(.examNow$exam %in% c(1)))
         next
      message("============================================================")
      message(.examNow$fname)
      message("------------------------------------------------------------")
      system("i_view32 /killmesoftly")
      session_grid(NULL)
      if (!(.examNow$check %in% c(1))) {
         .examTry <- inherits(try(source(.examNow$fname)),"try-error")
         .examData[.examI,"check"] <- .examTry+1L
      }
      if (!(.examNow$exam %in% c(1))) {
         .examName <- gsub("\\.ex\\.R$","",.examNow$fname)
         .examTry <- example(.examName,character.only=TRUE)
         q()
         .examData[.examI,"exam"] <- .examTry+1L
      }
      write.csv2(.examData,.examOut,quote=FALSE,row.names=FALSE)
      if (.examTry)
         break
   }
})
