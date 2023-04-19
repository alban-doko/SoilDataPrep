GetSG<-
  function(catch){
  
    dir.create("SoilGrids")
    
    data("sysdata", package="SoilDataPrep")
    
    
    #Adjust catchment projection to WGS84 longlat
    wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    catch <- spTransform(catch, wgs)
    
    
    # Define soil types
    soil_types <- SG_layers
    soil_types
    # Loop through soil types and download data for each
    for (soil_type in soil_types) {
      
      family_name <- strsplit(soil_type, "_")[[1]][1]
      
      # Construct URL for current soil type
      url <- paste0("https://maps.isric.org/mapserv?map=/map/", paste0(family_name, ".map"), "&",
                    "SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&",
                    "COVERAGEID=", soil_type, "&FORMAT=image/tiff&",
                    "SUBSET=long(", bbox(catch)[1,1], ",", bbox(catch)[1,2], ")&",
                    "SUBSET=lat(", bbox(catch)[2,1], ",", bbox(catch)[2,2], ")&",
                    "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&",
                    "OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
      
      # Extract soil type name from URL
      old_filename <- gsub(".*COVERAGEID=|&.*", "", url)
      
      # Download file and save to disk
      download.file(url, destfile = paste0("SoilGrids/", old_filename, ".tif"), mode = "wb")
      
      # Print confirmation message
      cat("Downloaded", old_filename, "to", file.path(getwd(), "SoilGrids"), "\n")
      
      # Define new suffixes for file names
      new_suffixes <- c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6")
      
      # Define old suffixes to be replaced
      old_suffixes <- c("0-5cm_mean", "5-15cm_mean", "15-30cm_mean", "30-60cm_mean", "60-100cm_mean", "100-200cm_mean")
      
      # Loop through old suffixes and replace them with new suffixes
      for (i in seq_along(old_suffixes)) {
        
        # Define new file name by replacing old suffix with new suffix
        new_filename <- gsub(old_suffixes[i], paste0(new_suffixes[i]), old_filename)
        
        # Rename file
        file.rename(from = paste0("SoilGrids/",old_filename, ".tif"), to = paste0("SoilGrids/",new_filename, ".tif"))
        
        # Print confirmation message
        cat("Renamed", paste0("SoilGrids/",old_filename, ".tif"), "to", paste0("SoilGrids/",new_filename, ".tif"), "\n")
        
        # Update old filename for next iteration
        old_filename <- new_filename
      }
    }
    
    
    url <- paste0("https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&",
                  "COVERAGEID=MostProbable&FORMAT=image/tiff&",
                  "SUBSET=long(", bbox(catch)[1,1], ",", bbox(catch)[1,2], ")&",
                  "SUBSET=lat(", bbox(catch)[2,1], ",", bbox(catch)[2,2], ")&",
                  "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&",
                  "OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
    
    
    # Extract soil taxonomy from URL
    filename1 <- 'wrb'
    
    # Download file and save to disk
    download.file(url, destfile = paste0("SoilGrids/",filename1, ".tif"), mode = "wb")
    
    # Print confirmation message
    cat("Downloaded", filename1, "to", "SoilGrids/", "\n")

  }


