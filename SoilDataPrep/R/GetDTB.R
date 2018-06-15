GetDTB<-
  function(catch){
    dir.create("Pelletier_DTB")
    
    #Adjust catchment projection to WGS84 longlat
    wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    catch <- spTransform(catch, wgs)
    
    #d5 - FLOAT32
    url<-paste0("https://webmap.ornl.gov/ogcbroker/wcs?originator=SDAT&&daac_usernum=45306&service=WCS&version=1.0.0&request=GetCoverage&coverage=1304_5&crs=EPSG:4326&bbox=", 
                floor(bbox(catch)[1]), ",", floor(bbox(catch)[2]), ",", ceiling(bbox(catch)[3]), ",", ceiling(bbox(catch)[4]),
                "&resx=0.008333333333333&resy=0.008333333333333&format=GeoTIFF_FLOAT32&interpolation=NEAREST")
    
    download.file(url, destfile="Pelletier_DTB/depth_5.tif", mode="wb")
    
    #d6 - BYTE
    url<-paste0("https://webmap.ornl.gov/ogcbroker/wcs?originator=SDAT&&daac_usernum=45306&service=WCS&version=1.0.0&request=GetCoverage&coverage=1304_6&crs=EPSG:4326&bbox=",
                floor(bbox(catch)[1]), ",", floor(bbox(catch)[2]), ",", ceiling(bbox(catch)[3]), ",", ceiling(bbox(catch)[4]),
                "&resx=0.008333333333333&resy=0.008333333333333&format=GeoTIFF_BYTE&interpolation=NEAREST")
    download.file(url, destfile="Pelletier_DTB/depth_6.tif", mode="wb") 
  }  