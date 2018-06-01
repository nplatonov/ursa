invisible({
   require(ursa) ##ursa::session_proj4
  # plutil::ursula()
   if (FALSE) {
      session_grid(NULL)
      example(as.Raster)
      session_grid(NULL)
      example(image.ursaRaster)
      q()
   }
   .examList <- ls("package:ursa")
   .examOut <- "__runExamples2.csv"
   if (!file.exists(.examOut)) {
      write.csv2(data.frame(name=.examList,check=0L),.examOut
                ,quote=FALSE,row.names=FALSE)
   }
   .examData <- read.csv2(.examOut)
   .toRepair <- length(which(.examData$check==2))
   for (.examI in sample(seq(nrow(.examData)))) {
      .examNow <- .examData[.examI,,drop=TRUE]
      if ((.toRepair)&&(.examNow$check!=2))
         next
      if (.examNow$check %in% c(1))
         next
      message("============================================================")
      message(.examNow$name)
      message("------------------------------------------------------------")
      system("i_view32 /killmesoftly")
      session_grid(NULL)
      if (!(.examNow$check %in% c(1))) {
         #.examTry <- inherits(try(source(.examNow$fname)),"try-error")
        # .examName <- gsub("\\.ex\\.R$","",.examNow$fname)
         .examTry <- inherits(try(example(.examNow$name,character.only=TRUE))
                             ,"try-error")
         .examData[.examI,"check"] <- .examTry+1L
      }
      write.csv2(.examData,.examOut,quote=FALSE,row.names=FALSE)
      if (.examTry)
         break
   }
})
