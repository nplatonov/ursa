plutil::ursula(TRUE)
session_grid(NULL)
n <- 1e2
x <- runif(n,min=25,max=65)
y <- runif(n,min=55,max=65)
z <- runif(n,min=1,max=10)
da <- data.frame(x=x,y=y,z=z)
if (requireNamespace("sp")) {
   da.sp <- da
   sp::coordinates(da.sp) <- ~x+y
   sp::proj4string(da.sp) <- "+init=epsg:4326"
   print(spatial_bbox(da.sp))
   print(spatial_crs(da.sp))
}
if (requireNamespace("sf")) {
   da.sf <- sf::st_as_sf(da,coords=c("x","y"),crs=4326)
   print(spatial_bbox(da.sf))
   print(spatial_crs(da.sf))
}
