invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- create_envi()
   fname <- a$con$fname
   dir(pattern=basename(envi_list(fname)))
   close(a)
   invisible(envi_remove(fname))

   a <- create_envi("exam1",layername=paste("Band",1:5)
                   ,ignorevalue=99,datatype="Int16",interleave="bil")
   ursa_info(a)
   print(a[])
   close(a)
   invisible(envi_remove("exam1"))
})
