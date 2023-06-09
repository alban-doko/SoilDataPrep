MapSoils_Results <- function() {
  #euptf_attributes # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  dir.create("euptf_attributes", showWarnings = FALSE)
  
  for (soillayer in c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6")) {
    print(paste("- soillayer", soillayer, Sys.time()))
    
    clay <- raster(paste0("SoilGrids/clay_", soillayer, ".tif"))
    silt <- raster(paste0("SoilGrids/silt_", soillayer, ".tif"))
    sand <- raster(paste0("SoilGrids/sand_", soillayer, ".tif"))
    coarse <- raster(paste0("SoilGrids/cfvo_", soillayer, ".tif"))
    bulkD <- raster(paste0("SoilGrids/bdod_", soillayer, ".tif"))
    om <- raster(paste0("SoilGrids/soc_", soillayer, ".tif"))
    ph <- raster(paste0("SoilGrids/phh2o_", soillayer, ".tif"))
    cec <- raster(paste0("SoilGrids/cec_", soillayer, ".tif"))
    
    topsoil <- ifelse(soillayer == "sd1", "top", "sub")
    
    sand_raster <- raster(sand)
    silt_raster <- raster(silt)
    clay_raster <- raster(clay)
    om_raster <- raster(om)
    bulkD_raster <- raster(bulkD)
    ph_raster <- raster(ph)
    cec_raster <- raster(cec)
    topsoil_raster <- raster(clay)
    
    values(sand_raster) <- as.numeric(getValues(sand)) / 10
    values(silt_raster) <- as.numeric(getValues(silt)) / 10
    values(clay_raster) <- as.numeric(getValues(clay)) / 10
    values(om_raster) <- (1.74 * as.numeric(getValues(om))) / 100
    values(bulkD_raster) <- as.numeric(ifelse(getValues(bulkD) <= 0, NA, getValues(bulkD)/100)) 
    values(ph_raster) <- as.numeric(getValues(ph)) / 10
    values(cec_raster) <- as.numeric(getValues(cec)) / 10
    values(topsoil_raster) <- as.numeric(topsoil)
    
    sand_output_file <- paste0("euptf_attributes/USSAND_", soillayer, ".tif")
    silt_output_file <- paste0("euptf_attributes/USSILT_", soillayer, ".tif")
    clay_output_file <- paste0("euptf_attributes/USCLAY_", soillayer, ".tif")
    om_output_file <- paste0("euptf_attributes/OC_", soillayer, ".tif")
    bulkD_output_file <- paste0("euptf_attributes/BD_", soillayer, ".tif")
    ph_output_file <- paste0("euptf_attributes/PH_H2O_", soillayer, ".tif")
    cec_output_file <- paste0("euptf_attributes/CEC_", soillayer, ".tif")
    
    writeRaster(sand_raster, sand_output_file, format = "GTiff")
    writeRaster(silt_raster, silt_output_file, format = "GTiff")
    writeRaster(clay_raster, clay_output_file, format = "GTiff")
    writeRaster(om_raster, om_output_file, format = "GTiff")
    writeRaster(bulkD_raster, bulkD_output_file, format = "GTiff")
    writeRaster(ph_raster, ph_output_file, format = "GTiff")
    writeRaster(cec_raster, cec_output_file, format = "GTiff")
    
    
  }
  
  ###############################################################################
  # Create a directory to store the output rasters
  dir.create("euptf_rasters", showWarnings = FALSE)
  
  for (soillayer in c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6")) {
    print(paste("- soillayer", soillayer, Sys.time(), "Memory in use:", memory.size(max = FALSE)))
    
    clay <- raster(paste0("SoilGrids/clay_", soillayer, ".tif"))
    silt <- raster(paste0("SoilGrids/silt_", soillayer, ".tif"))
    sand <- raster(paste0("SoilGrids/sand_", soillayer, ".tif"))
    coarse <- raster(paste0("SoilGrids/cfvo_", soillayer, ".tif"))
    bulkD <- raster(paste0("SoilGrids/bdod_", soillayer, ".tif"))
    om <- raster(paste0("SoilGrids/soc_", soillayer, ".tif"))
    ph <- raster(paste0("SoilGrids/phh2o_", soillayer, ".tif"))
    cec <- raster(paste0("SoilGrids/cec_", soillayer, ".tif"))
    
    soil_attributes <- data.frame(
      clay = getValues(clay) / 10,
      silt = getValues(silt) / 10,
      bulkD = ifelse(getValues(bulkD) <= 0, NA, getValues(bulkD)/100),
      om = (1.74 * getValues(om)) / 100,
      coarse_frag = getValues(coarse) / 10,
      topSoil = as.numeric(soillayer == "sd1")
    )
    
    euptf_attributes <- data.frame(
      TOPSOIL = ifelse(soillayer == "sd1", "top", "sub"),
      USSAND = getValues(sand) / 10,
      USSILT = getValues(silt) / 10,
      USCLAY = getValues(clay) / 10,
      OC = (1.74 * getValues(om)) / 100,
      BD = ifelse(getValues(bulkD) <= 0, NA, getValues(bulkD)/100),
      PH_H2O = getValues(ph) / 10,
      CEC = getValues(cec) / 10
    )
    
    ptf_props <- data.frame(theta_pwp = predict.ptf(newdata = euptf_attributes, ptf = "PTF12"))
    ptf_props$theta_s <- predict.ptf(newdata = euptf_attributes, ptf = "PTF06")
    ptf_props$ks <- (10^(predict.ptf(newdata = euptf_attributes, ptf = "PTF17"))) * 10
    
    tt <- ptf.rawls(soilprop = soil_attributes, h = 316, parameters = c("theta", "S_f", "theta_r", "h_b", "lambda"))
    
    ptf_props$theta_r <- tt[,"theta_r"]
    ptf_props$fk <- tt[,"theta"]
    ptf_props$fk63 <- ptf.rawls(soilprop = soil_attributes, h = 63, parameters = "theta")[,"theta"]
    ptf_props$suction <- tt[,"S_f"]
    ptf_props$bubb_pres <- tt[,"h_b"]
    ptf_props$pore_size_i <- tt[,"lambda"]
    
    # Save the rasters for each parameter
    for (param in c("theta_s", "theta_pwp", "ks", "theta_r", "fk", "fk63", "suction", "bubb_pres", "pore_size_i")) {
      param_raster <- raster(clay)
      values(param_raster) <- ptf_props[[param]]
      
      param_output_file <- paste0("euptf_rasters/", param, "_", soillayer, ".tif")
      writeRaster(param_raster, param_output_file, format = "GTiff")
    }
  }
}
