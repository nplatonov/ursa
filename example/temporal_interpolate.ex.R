invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # set.seed(352)
   n <- 45 # bands
   m <- 3 # sample size
   k <- median(seq(n))+seq(m)-(m %/% 2)-1 ## sample subset
   s <- 5 # window size
   a <- round(ursa_dummy(n,min=-60,max=60,elements=15,mul=1/8))
   a[a<(-40)] <- NA
   b <- temporal_interpolate(a,7)
   p1 <- colorize(a,lazy=TRUE)
   p2 <- colorize(b,lazy=TRUE,colortable=p1)
   display(list('Source'=p1[k],'Gaps are filled'=p2[k]),layout=c(2,NA)
          ,legend=list(list(1,"right"),list(2,"right")),decor=FALSE)
})
