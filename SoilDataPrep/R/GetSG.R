GetSG<-
  function(catch){
    dir.create("SoilGrids")
    load("SoilDataPrep/R/sysdata.rda")
    
  for (i in 1:length(SG_layers)){ 
    
    url<- paste0("http://85.214.241.121:8080/geoserver/ows?service=WCS&version=2.0.1&request=GetCoverage&CoverageId=",
                 SG_layers[i], "_250m&subset=Long(", 
                 bbox(catch)[1,1],",", 
                 bbox(catch)[1,2], ")",
                 "&subset=Lat(",
                 bbox(catch)[2,1],",",
                 bbox(catch)[2,2], ")")
    
    download.file(url, destfile=paste0("SoilGrids/", SG_layers[i], ".tif"), mode = "wb") }
  }
