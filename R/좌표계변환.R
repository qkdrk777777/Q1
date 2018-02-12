#' UTM-K(GRS-80)->WGS84
#'
#' @export
tranCoord<-function(lon,lat,
                    from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
                      ,to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"){
  xy<-data.frame(long=lon,lat=lat)
  from.crs<-CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)

  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")

  return(changed)

}
