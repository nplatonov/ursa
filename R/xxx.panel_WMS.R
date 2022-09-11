'.compose_wms' <- function(src
                          ,version=""
                          ,layers=""
                          ,epsg=""
                          ,format=""
                          ,bbox="" #"{minx},{miny},{maxx},{maxy}"
                          ,styles=""
                          ,transparent=""
                          ,legend="topright",gdalwarp=FALSE,tile=1024L
                          ,size=800L,fail180=FALSE
                          ,transpose=FALSE ## 'rotate' 'reorder' ?
                          ,extend=TRUE
                          ,cache=TRUE
                          ,verbose=FALSE,...) {
   isCache <- (is.logical(cache))&&(isTRUE(cache))||(is.character(cache))
   src <- unlist(src)
  # dst <- if (.isPackageInUse()) tempfile(fileext=".xml") 
  #        else paste0(.argv0name(),".xml")
  # dst <- tempfile(fileext=".xml") 
  # dst <- paste0(.argv0name(),".xml")
   isMetadata <- FALSE
   versionList <- c("1.1.1","1.3.0","1.1.0","1.0.0")
  # if (nchar(version))
  #    version <- match.arg(version,versionList)
  # else
  #    version <- versionList[1]
   if (!.lgrep("\\?",src))
      src <- paste0(src,"?")
   else if (!.lgrep("\\?&",src))
      src <- .gsub("\\?","?&",src)
   toStop <- 0L
   multiple <- FALSE
   multiple <- all(layers==" ") #.lgrep("^\\s.$",layers)>0
   ind1 <- .lgrep("&version=",src)==0 ## 'version' is not in request
   ind2 <- nchar(as.character(version))>0 ## 'version' is set manually
   if (ind2)
      version <- match.arg(version,versionList)
   else if (!ind1)
      version <- .gsub(".+&version=(.+)(&|$)","\\1",src)
   else
      version <- "" #versionList[1]
   if ((!ind1)&&(ind2)) {
      src <- sapply(strsplit(src,split="&"),function(x)
         paste(.grep("^version=",x,invert=TRUE,value=TRUE),collapse="&"))
   }
   else if (ind2)
      src <- paste0(src,"&version=",as.character(version))
   src1 <- unlist(strsplit(src,split="&"))
   src1 <- .grep("^(bbox|request|service)=",src1,value=TRUE,invert=TRUE)
   src1 <- paste0(paste(src1,collapse="&"),"&service=WMS&request=GetCapabilities")
   ind1 <- .lgrep("&layer(s)*=",src)==0
   ind2 <- if (!multiple) sum(nchar(layers)) else 0L
   if ((ind1)||(ind2)) {
      if (ind2) {
        # layerString <- paste(layers,collapse=",")
        # layers <- unlist(strsplit(layers,split=","))
         if (!ind1) {
            src <- unlist(strsplit(src,split="&"))
            src <- paste(.grep("^layer(s)*=",src,invert=TRUE,value=TRUE),collapse="&")
         }
         src <- paste0(src,"&layers=",layers)
        # src <- paste0(src,"&layers=",URLencode(URLencode(iconv(layers,to="UTF-8"))))
         multiple <- length(layers)>1
      }
      else {
        # src1 <- paste0(src,"&service=WMS&request=GetCapabilities")
        # if (!file.exists(dst))
        #    download.file(src1,dst,mode="wt",quiet=!verbose)
        # else if ((verbose)&&(!isMetadata))
        #    message(src1)
         src1 <- gsub("\\?\\&","?",src1)
         if (isCache)
            dst <- .ursaCacheDownload(src1,mode="wt",cache=cache,quiet=!verbose)
         else {
            dst <- tempfile(fileext=".xml") 
            download.file(src1,dst,mode="wt",quiet=!verbose)
         }
         if (!isMetadata)
            isMetadata <- TRUE
         md <- .parse_wms(dst,verbose=verbose)
         if (.lgrep("<ServiceException(>|\\s)",md)) {
            message("Exception is found")
            message(paste(md,collapse="\n"))
            return(src)
         }
         ind1 <- .grep("<Layer(>|\\s)",md)
         ind2 <- .grep("<Name(>|\\s)",md)
         ind3 <- .grep("<Title(>|\\s)",md)
         dist2 <- .dist2(cbind(x=ind1,y=0),cbind(x=ind2,y=0)
                        ,verbose=FALSE,summarize=FALSE)
         ind2a <- ind2[dist2$ind]
         dist3 <- .dist2(cbind(x=ind2a,y=0),cbind(x=ind3,y=0)
                        ,verbose=FALSE,summarize=FALSE)
         ind3a <- ind3[dist3$ind]
         desc <- data.frame(Layer=md[ind2a+1],Title=md[ind3a+1])
         if (TRUE) {
            desc <- unique(desc)
            rownames(desc) <- NULL
         }
         n1 <- max(nchar(desc$Layer))
         n2 <- max(nchar(desc$Title))
         n <- 77-n1-n2
         if (n<0)
            desc$Title <- substr(desc$Title,1,n2+n)
        # print(desc[sort(sample(nrow(desc),26)),],row.names=FALSE)
         res <- data.frame(ind1=ind1,ind2=ind2a,dist=dist2$dist)
         layers <- unique(md[res$ind2+1])
         if (!multiple) {
            message("Layers are not specified. Available layers (argument 'layers'):")
           # print(layers,quote=!FALSE)
            print(desc,row.names=FALSE)
           # layers <- NULL
           # stop("Layers are not specified")
            toStop <- toStop+8L
         }
         else {
            src <- paste0(src,"&layers=",layers)
         }
      }
   }
   else {
      layers <- sapply(strsplit(src,split="&"),function(x) 
                     .gsub(".+=(.+)","\\1",.grep("^layer(s)*=",x,value=TRUE)))
   }
   if (multiple)
      sname <- layers
   isSingle <- length(layers)==1L
   if (isSingle)
      src1 <- paste0(src1,"&layers=",layers)
   ind <- .grep("&format=",src)
   if (!length(ind)) {
      if (nchar(format)) {
         if (.lgrep("image",format))
            src <- paste0(src,format)
         else
            src <- paste0(src,"&format=image/",format)
      }
      else {
        # src1 <- paste0(sample(src,1),"&service=WMS&request=GetCapabilities")
        # if (!file.exists(dst))
        #    download.file(src1,dst,mode="wt",quiet=!verbose)
        # else if ((verbose)&&(!isMetadata))
        #    message(src1)
         src1 <- gsub("\\?\\&","?",src1)
         if (isCache)
            dst <- .ursaCacheDownload(src1,mode="wt",cache=cache,quiet=!verbose)
         else {
            dst <- tempfile(fileext=".xml") 
            download.file(src1,dst,mode="wt",quiet=!verbose)
         }
         if (!isMetadata)
            isMetadata <- TRUE
         md <- .parse_wms(dst,verbose=verbose)
         ind1 <- .grep("<GetMap>",md)
         if (length(ind1)) {
            ind2 <- .grep("</GetMap>",md)
            ind2 <- ind2[ind2>=ind1][1]
            md <- md[(ind1+1):(ind2-1)]
            ind1 <- .grep("<Format>",md)
            md <- md[ind1+1]
            message("Image format is not specified. Available formats:")
            print(md,quote=FALSE)
           # md <- .gsub("image/png; mode=8bit","image/png8",md)
           # md <- .gsub("image/png; mode=24bit","image/png24",md)
            message("Supported formats (argument 'format'):")
            patt <- "image/(png|pngX|pngXX|jpg|jpeg|tif|tiff|gif|bmp)$"
            fmt <- .gsub(patt,"\\1",.grep(patt,md,value=TRUE))
            print(fmt)
            toStop <- toStop+1L
         }
      }
   }
   ind1 <- .lgrep("&styles=",src)==0
   ind2 <- nchar(as.character(styles))>0
   if ((ind1)||(ind2)) {
      if (ind1) {
         if (!ind2) {
            src1 <- gsub("\\?\\&","?",src1) ## move to upper level
            if (isCache)
               dst <- .ursaCacheDownload(src1,cache=cache,mode="wt",quiet=!verbose)
            else {
               dst <- tempfile(fileext=".xml") 
               download.file(src1,dst,mode="wt",quiet=!verbose)
            }
            if (!isMetadata)
               isMetadata <- TRUE
            md <- .parse_wms(dst,verbose=verbose)
            ind1 <- .grep("<Name(>|\\s)",md)
            layer <- unlist(strsplit(paste(layers,collapse=","),split=","))#[1]
            if (length(layer)==1) {
               ind2 <- match(layer,md[ind1+1])
               if (is.na(ind2))
                  indS <- integer()
               else {
                  ind1 <- ind1[ind2]
                  ind2 <- .grep("</Layer>",md)
                  ind2 <- ind2[ind2>ind1][1]
                  md1 <- md[ind1:(ind2-1)]
                 # indS <- .grep("epsg:\\d+$",md1)#+ind1-1L
                 # epsgL <- unique(.gsub("^.*epsg:","",.grep("epsg",md1[indL],value=TRUE)))
                  ind1 <- .grep("<Style>",md1)
                  if ((length(ind1))&&(!anyNA(ind1))) {
                     ind2 <- .grep("<Name>",md1[ind1+1])
                     if (length(ind2)==length(ind1)) {
                        valueStyle <- md1[ind1+2]
                        message("Image style is not specified. Available formats:")
                        message("Supported values (argument 'styles'):")
                        print(valueStyle)
                        toStop <- toStop+16L
                     }
                  }
               }
            }
         }
         else {
            src <- sapply(strsplit(src,split="&"),function(x)
               paste(.grep("^style(s)*=",x,invert=TRUE,value=TRUE),collapse="&"))
         }
      }
      src <- paste0(src,"&styles=",as.character(styles))
   }
   ind1 <- .lgrep("&transparent=",src)==0
   ind2 <- nchar(as.character(transparent))>0
   if ((ind1)||(ind2)) {
      if ((ind1)&&(!ind2))
         transparent <- "true"
      if (!ind1) {
         src <- sapply(strsplit(src,split="&"),function(x)
            paste(.grep("^transparent=",x,invert=TRUE,value=TRUE),collapse="&"))
      }
      src <- paste0(src,"&transparent=",as.character(transparent))
   }
   ind <- .grep("&[cs]rs=",src)
   if (!length(ind)) {
      src1 <- gsub("\\?\\&","?",src1)
      if (isCache)
         dst <- .ursaCacheDownload(src1,mode="wt",cache=cache,quiet=!verbose)
      else {
         dst <- tempfile(fileext=".xml") 
         download.file(src1,dst,mode="wt",quiet=!verbose)
      }
      if (nchar(as.character(epsg))) {
         if (!nchar(version)) {
            md <- .parse_wms(dst,verbose=verbose)
            version <- .gsub("^.+(version=(\")*(\\d\\.\\d\\.\\d)(\")*)\\s.+$","\\3"
                            ,md[.grep("^<WMS_Capabilities",md)])
         }
         pref <- switch(version,'1.3.0'="crs",'x.x.x'="xxx","srs")
         src <- paste0(src,"&",pref,"=EPSG:",epsg)
      }
      else {
        # src1 <- paste0(sample(src,1),"&service=WMS&request=GetCapabilities")
        # if (!file.exists(dst))
        #    download.file(src1,dst,mode="wt",quiet=!verbose)
        # else if ((verbose)&&(!isMetadata))
        #    message(src1)
         if (!isMetadata)
            isMetadata <- TRUE
         md <- .parse_wms(dst,verbose=verbose)
         ind1 <- .grep("<Name(>|\\s)",md)
         layer <- unlist(strsplit(paste(layers,collapse=","),split=","))#[1]
         if (length(layer)==1) {
            ind2 <- match(layer,md[ind1+1])
            if (is.na(ind2))
               indL <- integer()
            else {
               ind1 <- ind1[ind2]
               ind2 <- .grep("</Layer>",md)
               ind2 <- ind2[ind2>ind1][1]
               md1 <- md[ind1:(ind2-1)]
               indL <- .grep("epsg:\\d+$",md1)#+ind1-1L
               epsgL <- unique(.gsub("^.*epsg:","",.grep("epsg",md1[indL],value=TRUE)))
            }
         }
         else
            indL <- integer()
         if ((TRUE)||(!length(ind1))) { ## search global [sc]rs
           # md1 <- md
            ind1 <- .grep("<[CS]RS",md)
            if (length(ind1)) {
               indG <- ind1+1L
               ind2 <- .grep("(epsg|[cs]rs):\\d+$",md[indG])
              # if (length(ind2)!=length(indG))
              #    indG <- integer()
               indG <- indG[ind2]
            }
         }
         if (length(indG))
            epsgG <- unique(.gsub("^.*epsg:","",.grep("epsg",md[indG],value=TRUE)))
         else
            epsgG <- character()
         ind1 <- c(indL,indG)
         cat("----------\n")
         if (length(ind1)) {
            epsgG <- unique(epsgG)
            if (length(indL)) {
               epsgL <- unique(epsgL)
               epsg <- unique(c(epsgL,epsgG))
               indM <- which(!is.na(match(epsg,epsgL)))
               epsgG <- epsg
               if (length(indM))
                  epsgG[indM] <- paste0(epsgG[indM],"*")
            }
            else
               indM <- integer()
           # epsg <- unique(.gsub("^.*epsg:","",.grep("epsg",md1[ind1],value=TRUE)))
            message("SRS/CRS is not specified. Available ESPG codes (argument 'epsg'):")
            print(epsgG,quote=FALSE)
            if (length(indM))
               message(paste("* is specified for layer",dQuote(layer)))
            if (length(epsgG))
               toStop <- toStop+2L
         }
      }
   }
   else {
      epsg <- sapply(strsplit(src,split="&"),function(x) 
                     .gsub(".+=EPSG:(.+)","\\1",.grep("^[cs]rs=",x,value=TRUE)))
   }
   layer <- unlist(strsplit(paste(layers,collapse=","),split=","))#[1]
   arglist <- list(...)
   if ((length(layer)==1)&&(extend | toStop)) {
     # if (!file.exists(dst))
     #    download.file(src1,dst,mode="wt",quiet=!verbose)
     # else if ((verbose)&&(!isMetadata))
     #    message(src1)
      src1 <- gsub("\\?\\&","?",src1)
      if (isCache)
         dst <- .ursaCacheDownload(src1,mode="wt",cache=cache,quiet=!verbose)
      else {
         dst <- tempfile(fileext=".xml") 
         download.file(src1,dst,mode="wt",quiet=!verbose)
      }
      if (!isMetadata)
         isMetadata <- TRUE
      md <- .parse_wms(dst,verbose=verbose)
      ind1 <- .grep("<Dimension",md)
      ind2 <- .grep("<Extent",md)
      if (length(ind2)>1) {
         ind1a <- .grep("<Name(>|\\s)",md)
         ind2a <- match(layer,md[ind1a+1])
         ind1a <- ind1a[ind2a]
         ind2a <- .grep("</Layer>",md)
         ind2a <- ind2a[ind2a>ind1a][1]
         md1 <- md[ind1a:(ind2a-1)]
         ind2a <- .grep("<Extent",md1)
         if (length(ind2a)==1) {
            if (FALSE) {
               ind2 <- ind1a+ind2a-1L
            }
            else {
               md <- md1
               ind1 <- .grep("<Dimension",md)
               ind2 <- .grep("<Extent",md)
            }
         }
      }
      if (length(ind2))
         dname2 <- tail(unlist(strsplit(md[ind2],split="\\s+")),-1)
      if ((length(ind1)>1)&&(length(layer)==1)) {
         ind7a <- grep(layer,md)
         ind1 <- ind1[ind1>min(ind7a)][1]
         ##~ print(ind1)
         ##~ print(md[ind1])
         ##~ q()
      }
      if (length(ind1)) {
         for (i in seq_along(ind1)) {
           # if (i==2)
           #    next
            dname <- tail(unlist(strsplit(md[ind1[i]],split="\\s+")),-1)
            isInline <- .lgrep("/>",tail(dname,1))
            if (!isInline)
               dname <- head(dname,-1)
            if (length(ind2)) {
              # dname2 <- tail(head(unlist(strsplit(md[ind2],split="\\s+")),-1),-1)
               if (length(na.omit(match(dname2,dname)))) {
                  dname <- unique(c(dname,dname2))
                  ind1[i] <- ind2
                  isInline <- FALSE
               }
            }
            dname <- unname(sapply(dname,strsplit,split="="))
            dname <- sapply(dname,function(x) gsub("(\"|>|/>)","",x))
            if (is.list(dname)) {
               dname <- dname[sapply(dname,length)==2]
               dname <- t(matrix(c(unlist(dname)),ncol=2,byrow=TRUE))
            }
           # dname <- do.call("rbind",t(lapply(dname,data.frame)))
            res <- dname[2,,drop=FALSE]
            colnames(res) <- dname[1,]
            rownames(res) <- " "
            if (!isInline)
               val <- unlist(strsplit(md[ind1[i]+1L],split=","))
            else
               val <- character()
           # val <- c(head(val,3),tail(val,3))
            varname <- res[,"name"]
            ind3a <- .lgrep(paste0("&",varname,"="),src)>0
            ind3b <- .lgrep(varname,names(arglist))>0
            if (ind3a+ind3b==0) {
               message(paste0("Optional attribute is found "
                             ,"(argument '",res[,"name"],"')"
                             ,ifelse(isInline,"",":")))
               if (length(val)>12) {
                  message(paste("From:",paste(head(val,3),collapse=" ")))
                  message(paste("To:  ",paste(tail(val,3),collapse=" ")))
               }
               else if (length(val))
                  message(paste("Values:",paste(val,collapse=" ")))
               if (nrow(res)==1)
                  message(paste0(colnames(res)[2],": ",res[2]))
            }
            toStop <- toStop+0L
         }
      }
   }
   defineGrid <- is.null(getOption("ursaSessionGrid"))
   if (toStop | defineGrid) {
     # src1 <- paste0(sample(src,1),"&service=WMS&request=GetCapabilities")
     # if (!file.exists(dst))
     #    download.file(src1,dst,mode="wt",quiet=!verbose)
     # else if ((verbose)&&(!isMetadata))
     #    message(src1)
      src1 <- gsub("\\?\\&","?",src1)
      if (isCache)
         dst <- .ursaCacheDownload(src1,mode="wt",cache=cache,quiet=!verbose)
      else {
         dst <- tempfile(fileext=".xml") 
         download.file(src1,dst,mode="wt",quiet=!verbose)
      }
      if (!isMetadata)
         isMetadata <- TRUE
      md <- .parse_wms(dst,verbose=verbose)
      tryGrid <- 0L
      layer <- unlist(strsplit(paste(layers,collapse=","),split=","))
      if (length(layer)==1) {
         ind1 <- .grep("<Name(>|\\s)",md)
         ind2 <- match(layer,md[ind1+1])
         if (is.na(ind2)) {
            message("Layer ",dQuote(layer)," is not found")
           # q()
         }
         else {
            ind1 <- ind1[ind2]
            ind2 <- .grep("</Layer>",md)
            ind2 <- ind2[ind2>ind1][1]
            md2 <- md[ind1:(ind2-1)]
            ind <- .grep("(boundingbox)",md2)
            if (length(ind))
               md <- md2
         }
      }
      ind1 <- .grep("<(EX_Geographic|LatLon)BoundingBox",md)
      bboxLL <- FALSE
      if (length(ind1)) {
         minxI <- .grep("<westBoundLongitude>",md)
         maxxI <- .grep("<eastBoundLongitude>",md)
         minyI <- .grep("<southBoundLatitude>",md)
         maxyI <- .grep("<northBoundLatitude>",md)
         ind <- c(minxI,maxxI,minyI,maxyI)
         if (length(na.omit(ind))==4) {
            minx <- md[minxI+1L]
            maxx <- md[maxxI+1L]
            miny <- md[minyI+1L]
            maxy <- md[maxyI+1L]
            a <- as.numeric(c(minx,miny,maxx,maxy))
            bboxLL <- TRUE
            tryGrid <- tryGrid+1L
         }
         else {
            if (length(ind1)>1) {
               minx <- md[minxI[1L]+1L]
               maxx <- md[maxxI[1L]+1L]
               miny <- md[minyI[1L]+1L]
               maxy <- md[maxyI[1L]+1L]
               a <- as.numeric(c(minx,miny,maxx,maxy))
               bboxLL <- TRUE
               if (length(na.omit(a))==4)
                  tryGrid <- tryGrid+1L
            }
            if (length(ind1)>1) {
               lim <- .gsub("[\"</>]"," ",md[ind1[1]])
               minx <- .gsub2("minx\\s*=\\s*(\\S+)\\s","\\1",lim)
               miny <- .gsub2("miny\\s*=\\s*(\\S+)\\s","\\1",lim)
               maxx <- .gsub2("maxx\\s*=\\s*(\\S+)\\s","\\1",lim)
               maxy <- .gsub2("maxy\\s*=\\s*(\\S+)\\s","\\1",lim)
               if (!tryGrid) {
                  a <- as.numeric(c(minx,miny,maxx,maxy))
                  tryGrid <- tryGrid+1L
               }
            }
         }
      }
      rot <- FALSE
      if ((tryGrid==0)||(toStop)) {
         if (length(epsg)>1) {
            if ("4326" %in% as.character(epsg))
               epsg <- "4326"
            else
               epsg <- sample(epsg,1)
         }
         if (!nchar(version)) {
            version <- .gsub("^.+(version=(\")*(\\d\\.\\d\\.\\d)(\")*)\\s.+$","\\3"
                            ,md[.grep("^<WMS_Capabilities",md)])
         }
         if ((version=="1.3.0")&&("4326" %in% as.character(epsg))) {
            ind1 <- .grep(paste0("BoundingBox CRS=\"*CRS:84"),md)
            if (length(ind1)) {
               lim <- .gsub("[\"</>]"," ",md[ind1[1]])
               minx3 <- .gsub2("minx\\s*=\\s*(\\S+)\\s","\\1",lim)
               miny3 <- .gsub2("miny\\s*=\\s*(\\S+)\\s","\\1",lim)
               maxx3 <- .gsub2("maxx\\s*=\\s*(\\S+)\\s","\\1",lim)
               maxy3 <- .gsub2("maxy\\s*=\\s*(\\S+)\\s","\\1",lim)
               a3 <- as.numeric(c(minx3,miny3,maxx3,maxy3))
               rot <- TRUE
            }
         }
        # else
        #    ind1 <- integer()
        # if (!length(ind1))
         ind1 <- .grep(paste0("BoundingBox [CS]RS=[\"]*EPSG:",epsg,"[\"]*"),md)
         if (length(ind1)) {
            lim <- .gsub("[\"</>]"," ",md[ind1[1]])
            minx2 <- .gsub2("minx\\s*=\\s*(\\S+)\\s","\\1",lim)
            miny2 <- .gsub2("miny\\s*=\\s*(\\S+)\\s","\\1",lim)
            maxx2 <- .gsub2("maxx\\s*=\\s*(\\S+)\\s","\\1",lim)
            maxy2 <- .gsub2("maxy\\s*=\\s*(\\S+)\\s","\\1",lim)
            a2 <- as.numeric(c(minx2,miny2,maxx2,maxy2))
            if ((rot)&&(all(a2[c(2,1,4,3)]==a3))) {
               message(paste("IMPORTANT NOTICE:"
                       ,"It is required to use argument/value 'transpose=TRUE'"
                       ,"or 'bbox={miny},{minx},{maxy},{maxx}'"))
               a <- a2[c(2,1,4,3)]
            }
            if (!tryGrid) {
               a <- if (rot) a3 else a2
            }
            tryGrid <- tryGrid+2L
         }
      }
      if ((FALSE)&&(tryGrid)&&(version %in% c("1.3.0"))) {
         ind1 <- .grep(paste0("BoundingBox CRS=\"*CRS:84"),md)
         lim <- .gsub("[\"</>]"," ",md[ind1[1]])
         minx3 <- .gsub2("minx\\s*=\\s*(\\S+)\\s","\\1",lim)
         miny3 <- .gsub2("miny\\s*=\\s*(\\S+)\\s","\\1",lim)
         maxx3 <- .gsub2("maxx\\s*=\\s*(\\S+)\\s","\\1",lim)
         maxy3 <- .gsub2("maxy\\s*=\\s*(\\S+)\\s","\\1",lim)
         a4 <- as.numeric(c(minx3,miny3,maxx3,maxy3))
         ind1 <- .grep(paste0("BoundingBox CRS=\"*EPSG:4326"),md)
         lim <- .gsub("[\"</>]"," ",md[ind1[1]])
         minx3 <- .gsub2("minx\\s*=\\s*(\\S+)\\s","\\1",lim)
         miny3 <- .gsub2("miny\\s*=\\s*(\\S+)\\s","\\1",lim)
         maxx3 <- .gsub2("maxx\\s*=\\s*(\\S+)\\s","\\1",lim)
         maxy3 <- .gsub2("maxy\\s*=\\s*(\\S+)\\s","\\1",lim)
         a5 <- as.numeric(c(minx3,miny3,maxx3,maxy3))
         rot <- TRUE
         if (all(a4[c(2,1,4,3)]==a5)) {
            message(paste("IMPORTANT NOTICE:"
                    ,"It is required to use argument/value 'transpose=TRUE'"
                    ,"or 'bbox={miny},{minx},{maxy},{maxx}'"))
         }
      }
      if (tryGrid) {
         if (!nchar(epsg)) {
            epsg <- 4326
         }
         code <- .gsub("\\D","",epsg)
         isLonLat <- code %in% "4326"
         p4s <- .epsg2proj4(code,force=TRUE,verbose=FALSE) # if (!isLonLat)
         if (any(abs(a)>360))
            xy <- matrix(a,ncol=2,byrow=TRUE)
         else {
            n <- 3
            if (code %in% c("3857","900913")) {
               elat <- 85.051129
               a[2][a[2]<(-elat)] <- -elat
               a[4][a[4]>(+elat)] <- +elat
            }
            if (a[2]<=-90)
               a[2] <- -90+1e-6
            if (a[4]>=+90)
               a[4] <- +90-1e-6
            if (abs(a[3]-a[1])>360) {
               a[1] <- -180
               a[3] <- +180
            }
            if (TRUE) {
               a <- matrix(a[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
               x <- a[,1]
               y <- a[,2]
               n <- 256
               x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                     ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
               y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                     ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
               ll <- cbind(x,y)
            }
            else
               ll <- expand.grid(seq(a[1],a[3],len=n),seq(a[2],a[4],len=n))
            if (isLonLat)
               xy <- ll
            else {
               xy <- .project(ll,p4s)
            }
         }
         b <- c(min(xy[,1]),min(xy[,2]),max(xy[,1]),max(xy[,2]))
         if (any(is.infinite(b))) {
            print(b)
            stop(paste("Unable to define grid for layer",.dQuote(layer)))
         }
         res <- max(c(b[3]-b[1],b[4]-b[2])/size)
         g0 <- regrid(setbound=b,res=res,proj=p4s)
        # print(g0);q()
         if (defineGrid)
            session_grid(g0)
      }
      else {
         if (verbose)
            message(paste("Unable to define grid for layer",.dQuote(layer)))
      }
   }
   if (toStop) {
      message(paste("Suggested to use",.sQuote(paste0("version=",version))))
     # if ((file.exists(dst))&&(dirname(dst)==tempdir())) {
     #    file.remove(dst)
     # }
      if (TRUE)
         stop("Incomplete request")
      opW <- options(warn=1)
      warning("Incomplete request")
      options(opW)
      return(src)
   }
   ind1 <- .lgrep("&bbox=",src)==0
   ind2 <- nchar(as.character(bbox))>0
   if (ind2) {
      if (!ind1) {
         src <- sapply(strsplit(src,split="&"),function(x)
            paste(.grep("^bbox=",x,invert=TRUE,value=TRUE),collapse="&"))
      }
      src <- paste0(src,"&bbox=",as.character(bbox))
   }
   transpose <- transpose & version %in% c("","1.3.0") & as.character(epsg)=="4326"
   ind1 <- .lgrep("&bbox=",src)==0
   if ((ind1)&&(!ind2)) {
      bbox <- if (transpose) "{miny},{minx},{maxy},{maxx}"
                        else "{minx},{miny},{maxx},{maxy}"
      src <- paste0(src,"&bbox=",bbox)
   }
   if (length(arglist)) {
      if (length(ind <- grep("(^fileout$|^scale$)",names(arglist))))
         arglist <- arglist[-ind]
   }
   if (length(arglist))
      src <- paste0(src,paste(paste0("&",names(arglist)),as.character(arglist)
                             ,sep="=",collapse=""))
   names(src) <- if (multiple) sname else layers
   as.list(src)
}
'.get_wms' <- function(src
                          ,version=c("1.1.1","1.3.0","1.1.0","1.0.0")
                          ,layers=""
                          ,epsg=""
                          ,format=""
                          ,bbox="" #"{minx},{miny},{maxx},{maxy}"
                          ,styles=""
                          ,transparent=TRUE
                          ,legend="topright",gdalwarp=FALSE,tile=1024L
                          ,size=800L,fail180=FALSE,transpose=FALSE
                          ,extend=FALSE
                          ,cache=TRUE
                          ,verbose=FALSE,...) {
   arglist <- as.list(match.call()) ## try mget(names(match.call())[-1])
   if (TRUE) {
      argname <- names(as.list(args(.compose_wms)))
      argname <- argname[nchar(argname)]
      rname <- names(arglist)
      for (i in seq_along(arglist)[-1]) {
         if ((inherits(arglist[[i]],c("name","call")))&&(rname[i] %in% argname)) {
        # if (((is.language(arglist[[i]]))||(is.name(arglist[[i]])))&&
        #         (rname[i] %in% argname))
            if (isTRUE(getOption("ursaNoticeMatchCall")))
               message('try `mget(names(match.call())[-1])` instead of `as.list(match.call())`')
            arglist[[i]] <- eval.parent(arglist[[i]])
         }
      }
      arglist$extend <- FALSE
   }
   srclist <- unlist(do.call(".compose_wms",arglist[-1]))#[1]
   isCache <- (is.logical(cache))&&(isTRUE(cache))||(is.character(cache))
   g0 <- session_grid()
   res <- vector("list",length(src))
   names(res) <- names(src)
   src <- sample(srclist,1)[[1]]
   code <- .gsub(".+&[sc]rs=EPSG\\:(\\d+)(\\&.+|$)","\\1",src)
   p4epsg <- paste0("+init=epsg:",code)
   if (is.character(gdalwarp)) {
      resample <- gdalwarp
      gdalwarp <- TRUE
   }
   else
      resample <- "lanczos"
   if ((!FALSE)&&(gdalwarp)) {
      if (!nchar(Sys.which("gdalwarp")))
         message("'gdalwarp' is required to be in search paths")
   }
   proj4s <- unlist(strsplit(g0$crs,split="\\s+"))
   ind <- .grep("\\+(proj=merc|[ab]=6378137|[xy]_0=0|k=1|units=m|lat_ts=0)",proj4s)
   isMerc <- ((length(ind)==8)&&(!gdalwarp))
   isLonLat <- .lgrep("\\+proj=longlat",g0$crs)>0
   sc <- getOption("ursaPngScale")
   g3 <- g0
   if ((is.numeric(sc))&&(sc<1e11+0.75)) {
     # print(g0)
      g3 <- regrid(g0,mul=sc)
     # print(g3)
      if ((TRUE)||(isLonLat))
         g3 <- regrid(g3,setbound=with(g0,c(minx,miny,maxx,maxy))
                     ,dim=with(g3,c(rows,columns)))
     ## ********** NOTICE: generally, g3$resx!=g3$resy   ************** ##
     # print(g3,digits=15)
   }
   if (isMerc) {
      ind <- .grep("\\+lon_0",proj4s)
      lon0 <- as.numeric(.gsub(".+=(.+)","\\1",proj4s[ind]))
      if (lon0==0)
         isMerc <- FALSE
      else {
         B0 <- 6378137
         B <- B0*pi
         dx0 <- 6378137*lon0*pi/180
         proj4s[ind] <- "+lon_0=0.000000"
         g2 <- g3
         g3 <- regrid(g2,minx=g2$minx+dx0,maxx=g2$maxx+dx0
                     ,crs=paste(proj4s,collapse=" "),zero="keep")
      }
   }
   if (gdalwarp) {
      if (verbose)
         print(g3,digits=15)
      g2 <- g3
     # .elapsedTime("a")
      requireNamespace(c("sp","rgdal")[2],quietly=.isPackageInUse())
      dg <- 16
      xy <- with(g3,expand.grid(x=seq(minx,maxx,len=dg),y=seq(miny,maxy,len=dg)))
      ll <- .project(xy,g3$crs,inv=TRUE)
      p4s <- .epsg2proj4(p4epsg)
      xy <- .project(ll,p4s)
     # .elapsedTime("b")
      nm <- max(g3$columns,g3$rows)
      g3 <- regrid(ursa_grid()
                  ,setbound=c(min(xy[,1]),min(xy[,2]),max(xy[,1]),max(xy[,2]))
                  ,res=c(1,1)
                  ,crs=p4s)
     # print(g3)
      sc <- ifelse(isLonLat,3.3,1.5)*nm/max(g3$columns,g3$rows)
      g3 <- regrid(g3,mul=sc,border=5)
      if (verbose)
         print(g3)
      session_grid(g3)
   }
   tc <- seq(0L,g3$columns,by=tile)
   if (length(tc)==1)
      nc <- g3$columns
   else
      nc <- c(tail(tc,-1),g3$columns)-tc
   tr <- seq(0L,g3$rows,by=tile)
   if (length(tr)==1)
      nr <- g3$rows
   else
      nr <- c(tail(tr,-1),g3$rows)-tr
   x <- c(seq(g3$minx,g3$maxx,by=g3$resx*tile),g3$maxx)
   if (isLonLat)
      B <- 180
   if (isMerc) {
      xn <- sort(c(x,-B,-2*B,+B,+2*B))
      x <- unique(xn[xn>=min(x) & xn<=max(x)]) ## 20170723 ++ unique()
      nc <- diff(x)/g3$resx
      nc <- ceiling(nc)
     # if (length(nc)>1)
     #    nc[length(nc)] <- g3$columns-sum(nc[-length(nc)])
   }
   if ((!FALSE)&&(isLonLat)) {
      if (!fail180)
         xn <- sort(c(x,-B,-3*B,+B,+3*B))
      else {
         xn <- sort(c(x,0,2*B,4*B,-2*B))
      }
      x <- unique(xn[xn>=min(x) & xn<=max(x)]) ## 20170723 ++ unique()
      nc <- diff(x)/g3$resx
      nc <- ceiling(nc)
     # if (length(nc)>1)
     #    nc[length(nc)] <- g3$columns-sum(nc[-length(nc)])
   }
   if (length(nc)>1)
      nc[length(nc)] <- g3$columns-sum(nc[-length(nc)])
   else
      nc <- g3$columns
  # y <- c(seq(g3$miny,g3$maxy,by=g3$resy*tile),g3$maxy)
   y <- rev(c(seq(g3$maxy,g3$miny,by=-g3$resy*tile),g3$miny))
   tind <- .expand.grid(c=seq_along(nc),r=rev(seq_along(nr)))-1
  # tind <- cbind(tind,r2=length(nr)-tind$r-1)
   tcr <- .expand.grid(width=nc,height=rev(nr))
   txy1 <- .expand.grid(minx=head(x,-1),miny=head(y,-1))
   txy2 <- .expand.grid(maxx=tail(x,-1),maxy=tail(y,-1))
   tg <- cbind(tind,tcr,txy1,txy2)
   tg <- tg[tg$width>0 & tg$height>0,]
   if (isMerc | (isLonLat & !fail180)) {
      ind1 <- which(tg$minx>=B & tg$maxx>=B)
      ind2 <- which(tg$minx<=-B & tg$maxx<=-B)
      if (length(ind1)) {
         tg$minx[ind1] <- tg$minx[ind1]-2*B
         tg$maxx[ind1] <- tg$maxx[ind1]-2*B
      }
      if (length(ind2)) {
         tg$minx[ind2] <- tg$minx[ind2]+2*B
         tg$maxx[ind2] <- tg$maxx[ind2]+2*B
      }
   }
   if (isLonLat & fail180) {
      ind1 <- which(tg$minx>=2*B & tg$maxx>=2*B)
      ind2 <- which(tg$minx<=0 & tg$maxx<=0)
      if (length(ind1)) {
         tg$minx[ind1] <- tg$minx[ind1]-2*B
         tg$maxx[ind1] <- tg$maxx[ind1]-2*B
      }
      if (length(ind2)) {
         tg$minx[ind2] <- tg$minx[ind2]+2*B
         tg$maxx[ind2] <- tg$maxx[ind2]+2*B
      }
   }
   cellx <- (tg[,"maxx"]-tg[,"minx"])/tg[,"width"]
   celly <- (tg[,"maxy"]-tg[,"miny"])/tg[,"height"]
   if (verbose)
      print(tg)
   for (k in seq_along(srclist)) {
      src <- srclist[[k]]
      isPNG <- .lgrep("\\&format=image.+png",src)>0
      isJPEG <- .lgrep("\\&format=image.+(jpg|jpeg)",src)>0
      if (verbose)
         print(c(png=isPNG,jpg=isJPEG,GDAL=(!isPNG & !isJPEG)))
      i0 <- 0
      for (i in sample(seq(nrow(tg)))) {
         i0 <- i0+1
         src2 <- .gsub("{maxy}",format(tg$maxy[i],sci=FALSE)
                ,.gsub("{maxx}",format(tg$maxx[i],sci=FALSE)
                ,.gsub("{miny}",format(tg$miny[i],sci=FALSE)
                ,.gsub("{minx}",format(tg$minx[i],sci=FALSE),src))))
         if ((FALSE)&&(src2==src)) {
            version <- .gsub("^.+(\\?|\\&)version=(\\d\\.\\d\\.\\d)(\\&.+|$)","\\2",src)
            if ((version=="1.3.0")&&(code=="4326"))
               src2 <- paste0(src2,"&bbox=",tg$miny[i],",",tg$minx[i]
                                         ,",",tg$maxy[i],",",tg$maxx[i])
            else
               src2 <- paste0(src2,"&bbox=",tg$minx[i],",",tg$miny[i]
                                         ,",",tg$maxx[i],",",tg$maxy[i])
         }
         src2 <- paste0(src2,"&width=",tg$width[i],"&height=",tg$height[i])
         src2 <- paste0(src2,"&service=WMS&request=GetMap")
        # dst <- tempfile()
        # download.file(src2,dst,mode="wb",quiet=!verbose)
         src2 <- gsub("\\?\\&","?",src2)
         if (isCache)
            dst <- .ursaCacheDownload(src2,mode="wb",cache=cache,quiet=!verbose)
         else {
            dst <- tempfile() 
            download.file(src2,dst,mode="wb",quiet=!verbose)
         }
         if (isPNG)
            a <- try(png::readPNG(dst))
         else if (isJPEG)
            a <- try(jpeg::readJPEG(dst))
         else {
            a <- try(as.array(read_gdal(dst),permute=TRUE,flip=TRUE))
            session_grid(NULL)
         }
         if (inherits(a,"try-error")) {
            if (isPNG)
               a <- try(jpeg::readJPEG(dst))
            else if (isJPEG)
               a <- try(png::readPNG(dst))
         }
         if (inherits(a,"try-error")) {
            error <- paste(readLines(dst),collapse="\n")
         }
        # file.remove(dst)
         if (inherits(a,"try-error")) {
            message(error)
            if (i0==1)
               stop("Request 'GetMap' is declined")
            else
               next
         }
         if (i0==1) {
            dima <- dim(a)
            nb <- dima[3]
            b <- array(0,dim=c(g3$rows,g3$columns,nb))
         }
         br <- tg$r[i]*tile+seq_len(tg$height[i])
        # bc <- tg$c[i]*tile+seq_len(tg$width[i])
         bc <- sum(nc[seq_len(tg$c[i])])+seq_len(tg$width[i])
         if (FALSE) {
            print(tg[i,])
            print(range(sum(nc[seq_len(tg$c[i])])+seq_len(tg$width[i])))
            message("b - recepient")
            str(b)
            message("a - donor")
            str(a) 
            str(array(0,dim=c(length(br),length(bc),length(seq_len(nb)))))
            str(list(range(br),range(bc),range(seq_len(nb))))
            str(b[br,bc,seq_len(nb)])
         }
         b[br,bc,seq_len(nb)] <- a[seq_along(br),seq_along(bc),seq_len(nb)]
      }
     # b <- aperm(b,c(2,1,3))
     # d <- if (isPNG | isJPEG) as.ursa(b*255) else as.ursa(b)
      d <- if (isPNG | isJPEG) ursa_new(b*255,permute=TRUE,flip=TRUE)
           else as.ursa(b)
     # d <- as.integer(d)
      ursa_grid(d) <- if (isMerc) g2 else g3
      if (gdalwarp) {
         d <- .gdalwarp(d,grid=g0,resample=resample,verbose=verbose)
      }
      if ((is.null(legend))||(is.logical(legend))&&(!legend))
         logo2 <- NULL
      else if (FALSE) {
         a <- unlist(strsplit(src,split="&"))
         a <- .grep("^(request|servce|bbox)=",a,value=TRUE,invert=TRUE)
         a <- .gsub("^(layer)(s)*(=.+)","\\1\\3",a)
         reqL <- paste(c(a,"service=WMS","request=GetLegendGraphic"),collapse="&")
        # dst <- tempfile()
        # if (.try(download.file(reqL,dst,mode="wb",quiet=!verbose))) {
         reqL <- gsub("\\?\\&","?",reqL)
         if (isCache)
            dst <- try(.ursaCacheDownload(reqL,mode="wb",cache=cache,quiet=!verbose))
         else {
            dst <- tempfile()
            a <- try(download.file(reqL,dst,mode="wb",quiet=!verbose))
            if (inherits(a,"try-error"))
               dst <- a
         }
         if (!inherits(dst,"try-error")) {
            if (isPNG)
               logo2 <- try(png::readPNG(dst))
            else if (isJPEG)
               logo2 <- try(jpeg::readJPEG(dst))
            else {
               logo2 <- try(as.array(read_gdal(dst),permute=TRUE,flip=TRUE))
               session_grid(NULL)
            }
            if (inherits(logo2,"try-error")) {
               error <- paste(readLines(dst),collapse="\n")
            }
           # file.remove(dst)
            if (inherits(logo2,"try-error")) {
               message(error)
               opW <- options(warn=1)
               warning("Request 'GetLegendGraphic' is declined")
               options(opW)
            }
            if (!is.array(logo2))
               logo2 <- NULL
         }
      }
      else {
         a <- unlist(strsplit(src,split="&"))
         a <- .grep("^(request|servce|bbox)=",a,value=TRUE,invert=TRUE)
         ind <- .grep("^layer(s)*=",a)
         layer <- unlist(strsplit(.gsub(".+=(.+)","\\1",a[ind]),split=","))
         suff <- c("service=WMS","request=GetLegendGraphic")
         if (length(indV <- .grep("^version=",a))) {
            ver <- .gsub(".+=(.+)","\\1",a[indV])
            if (ver %in% c("1.3.0"))
               suff <- c(suff,"sld_version=1.1.0")
         }
         a <- paste(c(a[-ind],suff),collapse="&")
         reqL <- paste(a,paste0("layer=",layer),sep="&")
         logo <- vector("list",length(reqL))
        # dst <- tempfile()
         isOK <- TRUE
         reqL <- gsub("\\?\\&","?",reqL)
         for (i in seq_along(logo)) {
           # if (.try(download.file(reqL[i],dst,mode="wb",quiet=!verbose))) {
            if (isCache)
               dst <- try(.ursaCacheDownload(reqL,mode="wb",cache=cache,quiet=!verbose))
            else {
               dst <- tempfile()
               a <- try(download.file(reqL[i],dst,mode="wb",quiet=!verbose))
               if (inherits(a,"try-error"))
                  dst <- a
            }
            if (!inherits(dst,"try-error")) {
               if (isPNG)
                  logo[[i]] <- try(png::readPNG(dst))
               else if (isJPEG)
                  logo[[i]] <- try(jpeg::readJPEG(dst))
               else {
                  logo[[i]] <- try(as.array(read_gdal(dst),permute=TRUE,flip=TRUE))
                  session_grid(NULL)
               }
               if (inherits(logo[[i]],"try-error")) {
                  error <- paste(readLines(dst),collapse="\n")
               }
              # file.remove(dst)
               if (inherits(logo[[i]],"try-error")) {
                  if (verbose) {
                     message(error)
                     opW <- options(warn=1)
                     warning("Request 'GetLegendGraphic' is declined")
                     options(opW)
                  }
               }
               if (!is.array(logo[[i]]))
                  logo[[i]] <- NULL
            }
         }
         if (!length(logo))
            logo2 <- NULL
         else
            logo <- logo[sapply(logo,function(x) !is.null(x))]
         if (!length(logo))
            logo2 <- NULL
         else {
            a <- sapply(logo,dim)
            dima <- c(sum(a[1,]),max(a[2,]),max(a[3,]))
            adr1 <- 0
            adr2 <- 0
            adr3 <- 0
            logo2 <- array(1,dim=dima)
            for (i in seq_along(logo)) {
               dima <- dim(logo[[i]])
               adr1 <- max(adr1)+seq(dima[1])
               adr2 <- seq(dima[2])
               adr3 <- seq(dima[3])
              # str(lapply(list(adr1,adr2,adr3),range))
               logo2[adr1,adr2,adr3] <- logo[[i]]
            }
         }
      }
      attr(d,"copyright") <- "   "
      attr(d,"wmslegend") <- logo2
      res[[k]] <- d
   }
   session_grid(g0)
   res
}
'.panel_wms.alt' <- function(...) {
   if (.skipPlot(TRUE))
      return(NULL)
   g0 <- session_grid()
   arglist <- list(...)
   d <- .get_wms(...)
   alpha <- .getPrm(arglist,name="alpha",default=1)
   leg <- attr(d,"wmslegend")
   panel_raster(d,alpha=alpha)
   session_grid(g0)
   if (!is.null(leg)) {
      legend <- .getPrm(arglist,name="legend",default="topright"
                       ,class=c("character","integer"))
      if (is.logical(legend))
         legend <- "topright"
      panel_annotation(leg,pos=legend,alpha=alpha)
   }
}
'.panel_wms' <- function(src
                          ,version=c("1.1.1","1.3.0","1.1.0","1.0.0")
                          ,layers=""
                          ,epsg=""
                          ,format=""
                          ,bbox="" #"{minx},{miny},{maxx},{maxy}"
                          ,styles=""
                          ,transparent=TRUE
                          ,legend="topright",gdalwarp=FALSE,tile=1024L
                          ,size=800L,fail180=FALSE,transpose=FALSE
                          ,extend=FALSE,cache=TRUE
                          ,alpha=1,verbose=FALSE,...) {
   if (.skipPlot(TRUE))
      return(NULL)
   g0 <- session_grid()
   arglist <- as.list(match.call()) ## try mget(names(match.call())[-1])
   arglist[[2]] <- eval.parent(arglist[[2]])
   res <- do.call(".get_wms",arglist[-1])
   n <- length(res)
   for (i in seq_along(res)) {
      leg <- attr(res[[i]],"wmslegend")
      panel_raster(res[[i]],alpha=alpha)
      session_grid(g0)
      if (!is.null(leg)) {
         if (is.logical(legend))
            legend <- "topright"
         panel_annotation(leg,pos=legend,alpha=alpha)
      }
   }
   invisible(NULL)
}
'.is.wms' <- function(dsn) {
   if (is.list(dsn))
      return(FALSE)
   s <- unlist(strsplit(dsn,split="&"))
   ind <- .lgrep("^[sc]rs=",s)+
          .lgrep("^layer(s)*=",s)+
          .lgrep("^version=",s)+
          .lgrep("^format=",s)+
          .lgrep("^http(s)*\\://",s)+
          0
  # s <- unlist(strsplit(.grep("http(s)*://",dsn,value=TRUE),split="&"))
  # print(.grep("^(version=|format=|[cs]rs=|layer(s)*=)",s))
   ind>=4
}
'.parse_wms' <- function(dst,verbose=FALSE) {
   opW <- options(warn=ifelse(verbose,1,-1))
   md <- readLines(dst)
   options(opW)
   md <- paste(md,collapse=" ")
   md <- .gsub("\\<","___<<",md)
   md <- .gsub("\\>",">>___",md)
   md <- unlist(strsplit(md,split="(___<|>___)"))
   md <- md[nchar(md)>0]
   md <- .grep("^\\s+$",md,value=TRUE,invert=TRUE)
   md
}
