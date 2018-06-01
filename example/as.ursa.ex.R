plutil::mysource(ursa)
invisible({
   session_grid(NULL)
   a <- try(as.ursa(R.version.string))
   if (TRUE) { ## as.ursa example
      session_grid(NULL)
      n <- 1e3
      da <- data.frame(x=runif(n,min=-1200100,max=1600900)
                      ,y=runif(n,min=-1400800,max=1600200)
                      ,z=runif(n,min=0,max=100))
      ra <- as.ursa(da)
      str(ursa_grid(ra))
      display(ra)
      print(ra)
   }
})
