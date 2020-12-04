desc <- commandArgs(TRUE)
if (!length(desc)) {
   a <- readLines("DESCRIPTION")
   if (length(ind <- grep("Version:",a)))
      desc <- gsub("Version:\\s*","",a[ind])
   else
      desc <- "ongoing"
}
desc
system("git add -A")
if (!system(paste0("git commit -m",dQuote(desc))))
   system("git push")
