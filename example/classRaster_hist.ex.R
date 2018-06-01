invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- pixelsize()
   histogram(a,breaks=21)
   pdf.start(pdfname())
   hist(a)
   pdf.stop(pdfname())
})
