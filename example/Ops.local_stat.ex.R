invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   set.seed(353)
   session_grid(regrid(mul=1/8))
   a <- ursa_dummy(nband=15)
   a[a<60] <- NA
   str(unclass(a))
   cvr <- 12
   b <- local_stat(a,cover=cvr)
   print(b)
   c.mean <- c('<bundle> mean'=b["mean"]
              ,'local_mean'=local_mean(a,cover=cvr)
              ,'<generic> mean'=mean(a,cover=cvr))
   c.max <- c('<bundle> max'=b["max"]
             ,'local_max'=local_max(a,cover=cvr)
             ,'<generic> max'=max(a,cover=cvr))
   print(c.mean)
   print(c.max)
   cmp <- c(mean=b["mean"]-local_mean(a,cover=cvr)
           ,sd=b["sd"]-local_sd(a,cover=cvr))
   print(round(cmp,12))
   d <- as.list(b)
   d[["slopeS"]] <- colorize(d[["slopeS"]],stretch="signif")
   display(d,blank.density=20,blank.angle=c(-45,45))
})
