invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   set.seed(352)
   n <- 45 # bands
   m <- 3 # sample size
   k <- median(seq(n))+seq(m)-(m %/% 2)-1 ## sample subset
   s <- 5 # window size
   a <- round(ursa_dummy(n,min=-60,max=60,elements=15,mul=1/8))
   if (dontrun <- FALSE) {
      if (requireNamespace("caTools")) {
         b1 <- as.ursa(t(apply(as.matrix(a),1,caTools::runmean,k=s,endrule="mean")))
         b2 <- temporal_mean(a,s)
         print(b1[k])
         print(b2[k])
         print(c('identical?'=all.equal(ursa_value(b1),ursa_value(b2))))
      }
   }
   a[a<(-40)] <- NA
   va <- as.matrix(a) # or 'ursa_value(a)'
   b3 <- temporal_mean(a,s,cover=3/4,verbose=TRUE)
   b4 <- as.ursa(temporal_mean(as.matrix(va),s,cover=3/4,verbose=TRUE))
   p <- list('Before moving window'=a[k]
            ,'After moving window'=b3[k]
            ,'\'temporal_mean\' to matrix'=b4[k])
   print(p)
   print(c('identical?'=all.equal(ursa_value(b3),ursa_value(b4))))
   display(p[1:2],legend=list(list(1,"right"),list(2,"right")),decor=FALSE)
})
