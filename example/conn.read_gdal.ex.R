invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   
  # \dontrun{
  # print(rgdal::gdalDrivers())
  # }
   Fin1 <- system.file("pictures/Rlogo.jpg",package="rgdal")
  # Fin1 <- "C:/tmp/pictures/Rlogo.jpg"
   a1 <- read_gdal(Fin1)
   print(a1)
  # \dontrun{
  # display(a1)
  # }

   Fin2 <- system.file("pictures/test_envi_class.envi",package="rgdal")
  # Fin2 <- "C:/tmp/pictures/test_envi_class.envi"
   b1 <- read_gdal(Fin2)
   b2 <- read_envi(Fin2,resetGrid=TRUE)
   print(identical(ursa_grid(b1),ursa_grid(b2)))
   print(identical(ursa_value(b1),ursa_value(b2)))
   print(identical(ursa_colortable(b1),ursa_colortable(b2)))
   print(ursa_colortable(b1))
   print(as.table(b1))
  # \dontrun{
   display(b1,detail="l")
  # }
})
