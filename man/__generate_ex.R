require(plutil)
invisible({
   rd <- filelist(path=".",pattern="\\.Rd$",full.names=TRUE)
   ex <- file.path("../example",mygsub("\\.Rd$",".ex.R",basename(rd)))
   ind <- which(!file.exists(ex))
   if (length(ind))
      for (i in ind)
         file.copy("___prompt.R",ex[i])
})
