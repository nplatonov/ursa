invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # \dontrun{
   print(methods(class="ursaGrid"))#}
   a <- pixelsize()
   g <- ursa_grid(a)
   print(is.ursa(a,"grid"))
   print(is.ursa(g,"grid"))
   print(g)
})
