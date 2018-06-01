'legend_align' <- function(obj){
  # c(unlist(sapply(obj,function(x){names(x$colortable)})))
   c(unlist(sapply(obj,function(x) names(ursa_colortable(x)))))
}
