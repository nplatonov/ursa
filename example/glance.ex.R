invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (dontrun <- !FALSE) {
      f <- system.file("vectors","scot_BNG.shp",package="rgdal")
      glance(f,style="merc",field="(NAME|COUNT)")
     # cmd <- paste("Rscript --vanilla -e ursa::glance()",dQuote(f)
     #             ,paste0("proj=",dQuote("merc"))
     #             ,paste0("field=",dQuote("(lon|lat)")))
      cmd <- paste("Rscript --vanilla -e ursa::glance()",paste0("\"",f,"\"")
                  ,paste0("proj=\"merc\"")
                  ,paste0("field=\"(lon|lat)\""))
      cat(" --------- Try in command line: -----------\n")
      message(cmd)
      cat(" ----------- end of quoting ---------------\n")
   }
   if (dontrun <- !FALSE) {
      system(cmd)
   }
   if (dontrun <- FALSE) {
      require(sp)
      a <- data.frame(lat=c(70.734,71.657),lon=c(178.577,-177.38)
                     ,place="Wrangel Island")
      coordinates(a) <- ~lon+lat
      proj4string(a) <- "+init=epsg:4326"
      glance(a,style="google color maptype=terrain")
      glance(a,style="openstreetmap color")
      glance(a,style="mapnik color tile")
   }
   if (dontrun <- !FALSE) {
      glance("Svalbard")
   }
})
