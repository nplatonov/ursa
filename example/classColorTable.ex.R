invisible({
   plutil::ursula(2)
   session_grid(NULL)
  # \dontrun{
   print(methods(class="ursaColorTable"))#}

   a <- pixelsize()
   print(a)
   b1 <- colorize(a,value=c(400,500,600,700),interval=FALSE)
   b2 <- colorize(a,value=c(450,550,650)    ,interval=TRUE)
  # \dontrun{
  # display(list(b1,b2))#}
   print(is.ursa(a,"colortable"))
   print(is.ursa(b1,"colortable"))
   print(is.ursa(b2,"colortable"))
   print(ursa_colortable(a))
   print(ursa_colortable(b1))
   print(ursa_colortable(b2))
   ursa_colortable(b2) <- c("Low"="darkolivegreen1"
                           ,"Moderate"="darkolivegreen2"
                           ,"High"="darkolivegreen3"
                           ,"errata"="darkolivegreen4")
   print(ursa_colortable(b2))
   names(ursa_colortable(b2))[4] <- "Polar"
   print(ursa_colortable(b2))
  # write_envi(b2,"_res1")
  # \dontrun{
   display(b2)#}
})
