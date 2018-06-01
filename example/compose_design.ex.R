plutil::plutil::mysource(ursa)
invisible({
   session_grid(regrid(mul=1/4))
   session_grid(NULL)
   a <- ursa_dummy(nband=5,min=1,max=200)
   b <- list(colorize(a[1:3],pal.rich=240,pal.rotate=0)
            ,colorize(sqrt(a[4:5]),pal.rich=-15,pal.rotate=0,stretch="equal"))
   
   cl <- compose_design(b)
   print(cl)
   ##~ cl <- compose_design(b,byrow=TRUE,ratio=2.3)
   ##~ compose_open(cl)
   ##~ compose_close()
   ##~ q()
   
   if (dontrun <- TRUE)
   {
      cl1 <- compose_design(layout=c(2,3),byrow=TRUE,legend=NULL)
      print(cl1)
      compose_open(cl1)
      compose_close()
     # q()
   }
   if (dontrun <- FALSE)
   {
      cl2 <- compose_design(layout=c(2,3),byrow=FALSE,legend="left")
      print(cl2$layout)
      compose_open(cl2)
      compose_close()
     # q()
   }
  # cl3 <- compose_design(a,side=2)
  # print(cl3)
   if (dontrun <- FALSE)
   {
      compose_open(cl3)
      compose_close()
     # q()
   }
   cl4 <- compose_design(b)
  # print(cl4)
   if (dontrun <- FALSE)
   {
      compose_open(cl4)
      compose_plot(b,trim=TRUE,labels=7)
      compose_close()
     # q()
   }
   cl5 <- compose_design(b,zzzlayout=c(2,3),byrow=FALSE,skip=3
                        ,legend=list(list("full","left"),list(1:2,"right")))
  # print(cl5)
   if (dontrun <- !FALSE)
   {
      compose_open(cl5)
      compose_plot(b)
      compose_close()
     # q()
   }
   ##~ leg <- list("7"=list(row=1,col=0),"8"=list(2,"left")
              ##~ ,"9"=list("full","right"),"10"=list("top","full")
              ##~ ,"11"=list(99,1:2),"12"=list("bottom",3))
   leg <- list(list(1,0),list(2,"left")
              ,list("full","right"),list("top","full")
              ,list(99,1:2),list("bottom",3))
   str(leg)
   cl6 <- compose_design(layout=c(2,3),skip=NA,legend=leg)
  # print(cl6)
   if (dontrun <- FALSE)
   {
      compose_open(cl6,scale=3,pointsize=16)
      compose_close("crop")
   }
})
