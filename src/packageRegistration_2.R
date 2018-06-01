# require(plutil)
invisible({
   a <- readLines("ursa.c")
   ind1 <- grep("^(void|int).+",a)
   ind2 <- rep(NA,length(ind1))
  # print(substr(a[ind1],1,64),quote=FALSE)
   b <- NULL
   b <- c("#include <R.h>"
         ,"#include <Rinternals.h>"
         ,"#include <R_ext/Rdynload.h>"
         ,"#include \"ursa.h\""
         ,"")
   d <- NULL
   b5 <- ""
   b4 <- "static const R_CMethodDef CEntries[] = {"
   for (i in sample(seq_along(ind1))) {
      j <- ind1[i]+seq(10)-1
      ind2[i] <- j[grep("\\)",a[j])[1]]
      a3 <- a[ind1[i]:ind2[i]]
      a3 <- gsub("\\s*\\{","",a3)
      a2 <- paste(a3,collapse="")
      if (length(grep("\\)\\s*;\\s*",tail(a2,1)))) {
        # print("declaration")
         next
      }
      b2 <- gsub("\\S+\\s+(\\S+)\\s*\\(.*","\\1",a2)
      if (b2=="main")
         next
      a3 <- c(head(a3,-1),paste0(tail(a3,1),";"))
      d <- c(d,a3)
      a1 <- gsub(".+\\((.+)\\).*$","\\1",a2)
      b1 <- unlist(strsplit(a1,split=","))
      b1 <- gsub("\\s+$","",b1)
      b1 <- gsub(".*(double|int|char).*","\\1",b1)
      b1 <- gsub("double","REALSXP",b1)
      b1 <- gsub("int","INTSXP",b1)
      b1 <- gsub("char","STRSXP",b1)
      b5 <- c(b5
             ,paste0("static R_NativePrimitiveArgType ",b2,"_t[] = {")
             ,paste0("   ",paste(b1,collapse=", "))
             ,"};"
             ,"")
      if (b2=="zzzareaIncrement") {
        # print(a3)
        # print(a2)
         print(b5,quote=FALSE)
         q()
      }
     # print(b1)
      b4 <- c(b4,paste0("   {",dQuote(b2),", (DL_FUNC) &",b2,", ",length(b1)
                       ,", ",b2,"_t","},"))
   }
   b4 <- c(b4
         ,"   {NULL, NULL, 0, NULL}"
         ,"};"
         ,""
         )
   b <- c(b
         ,b5
         ,b4
        # ,"#include <Rversion.h>"
         ,"void R_init_ursa(DllInfo *dll)"
         ,"{"
         ,"   R_registerRoutines(dll, CEntries, NULL, NULL, NULL);"
         ,"   R_useDynamicSymbols(dll, FALSE);"
        # ,"#if defined(R_VERSION) && R_VERSION >= R_Version(2, 16, 0)"
         ,"   R_forceSymbols(dll, TRUE);"
        # ,"#endif"
         ,"}"
         )
  # message("-----------")
  # print(b,quote=FALSE)
   Fout <- file("init.c","wb")
   writeLines(b,Fout,sep="\n")
   close(Fout)
   Fout <- file("ursa.h","wb")
   writeLines(d,Fout,sep="\n")
   close(Fout)
})
