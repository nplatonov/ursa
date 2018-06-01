invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # set.seed(352)
   v <- round(runif(8,min=-1,max=1),3)
   customFilter <- matrix(c(v[1:4],-sum(v),v[5:8]),ncol=3)
   a <- ursa_dummy(1,mul=4/8,elements=32)
   if (FALSE) {
      verb <- 1L
      marg <- !TRUE
      sig <- 0.7
      s <- 11
      cov <- 0.75
      b1 <- focal_special(a,"gaussian",size=s,cover=cov,sigma=sig
                         ,verbose=verb,saveMargin=marg)
      b2 <- focal_special(a,"LoG",size=s,cover=cov,sigma=sig
                         ,verbose=verb,saveMargin=marg)
      b3 <- focal_special(b1,"laplacian",cover=cov,alpha=0
                         ,verbose=verb,saveMargin=marg)
      d <- c(src=a,gaus=b1,dif=a-b1,LoG=b2,gaus.lapl=b3)
      d["LoG"] <- d["LoG"]/band_sd(d["LoG"])
      d["gaus.lapl"] <- d["gaus.lapl"]/band_sd(d["gaus.lapl"])
      print(d)
      display(d,stretch="lin",layout=c(1,NA),decor=FALSE)
     # q()
   }
   if (FALSE) {
      for (s in c(3,5,7,9,11)) {
         message(s)
         focal_special(a,"LoG",size=s,sigma=(s-1)/4,verbose=2L)
      }
   }
   tpList <- eval(formals("focal_special")$type)
   res <- c(src=a,as.ursa(bandname=tpList))
   for (tp in tpList) {
      message(tp)
      res[tp] <- focal_special(a,tp,fmask=customFilter,size=11,sigma=1,alpha=0.8
                              ,saveMargin=0,verbose=2L)
   }
   print(res)
   display(res,decor=FALSE)
  # a[a<75] <- NA
   ##~ for (s in c(3,5,7,9))
      ##~ b.gaus <- focal_special(a,"gaus",size=s,sigma=(s-1)/4,verbose=2L)
  # b.lapl <- focal_special(a,"lapl",size=5,alpha=0.4,verbose=2L)
  # b.osisaf <- focal_special(a,"osisaf",verbose=2L)
  # display_stack(c(a,b.osisaf))

})
