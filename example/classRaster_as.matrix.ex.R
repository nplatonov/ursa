invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   pdf.start(pdfname())
   a <- ursa_dummy(nband=3,min=0,max=100)
   a <- a[a>=20 & a<=80]
   ignorevalue(a) <- 121
   str(ursa_value(a[2]))
   str(as.matrix(a[2]))
   b1 <- a[[2]]
   str(b1)
  # \dontrun{
   image(b1,asp=1)#}
   b2 <- as.matrix(a[2:3],coords=TRUE)
   print(c('theSame?'=identical(b1,b2)))
   a2 <- as.ursa(b2)
   res <- c(src=a[2],exported_then_imported=a2,diff=a[2]-a2)
   print(res)
   pdf.stop(pdfname())
})
