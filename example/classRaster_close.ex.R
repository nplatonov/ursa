invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- create_envi()
   fname <- a$con$fname
   message(paste("Created file",dQuote(basename(fname)),"will be deleted."))
   print(dir(pattern=basename(envi_list(fname))))
   close(a)
   invisible(envi_remove(fname))
})
