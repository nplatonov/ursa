invisible({
   file.remove(ursa:::.dir("\\.(o|dll|rds)$"))
   system2("R",c("CMD","SHLIB","ursa.c"))
   file.remove(ursa:::.dir("\\.(o)$"))
})
