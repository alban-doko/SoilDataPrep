MapSoils_Results <- function() {
  # Specify the output folder name
  output_folder <- "MapSoils_Results"
  
  # Delete the output folder if it exists
  if (file.exists(output_folder)) {
    unlink(output_folder, recursive = TRUE)
  }
  
  # Create a new output folder
  dir.create(output_folder)
  
  # Read the raster file using a relative path
  raster_file <- "MapSoils/soils_catchment.tif"
  raster_data <- raster(raster_file)
  
  # Read the table data from .dat file
  table_data <- read.table("horizons.dat", header = TRUE)
  
  print(table_data)
  
  # Get unique positions in the table
  positions <- unique(table_data$position)
  
  # Subset the table data to include only the desired parameters
  desired_parameters <- c("theta_r", "theta_pwp", "fk", "fk63", "nfk", "theta_s", "ks", "suction")
  subset_table_data <- table_data[, c("soil_id", "position", desired_parameters)]
  
  # Iterate over each position
  for (pos in positions) {
    # Subset the table for the current position
    subset_data <- subset_table_data[subset_table_data$position == pos, ]
    
    # Iterate over each parameter
    for (param in desired_parameters) {
      # Create a new raster to store the modified values
      new_raster_data <- raster_data
      
      # Iterate over each row in the subset data and assign the corresponding parameter value
      for (i in 1:nrow(subset_data)) {
        soil_id <- subset_data$soil_id[i]
        param_value <- subset_data[[param]][i]
        
        # Assign the parameter value to the corresponding soil_id in the raster
        new_raster_data[raster_data == soil_id] <- param_value
      }
      
      # Save the new raster to the output folder
      new_raster_file <- file.path(output_folder, paste0(param, "_sd", pos, ".tif"))
      writeRaster(new_raster_data, new_raster_file, format = "GTiff")
      
      # Print the path to the new raster file
      print(new_raster_file)
    }
  }
}
