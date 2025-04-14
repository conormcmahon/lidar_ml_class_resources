
# Load and tile LiDAR CHM and DEM

library(terra)
library(tidyverse)

# Directories for input files
input_chm_directory <- "D:/santa_clara_birds/LiDAR_2024/raster_data/"
input_dem_directory <- "D:/santa_clara_birds/LiDAR_2024/raster_data/"

# Directories for output files
output_chm_directory <- "D:/santa_clara_birds/LiDAR_2024/raster_data_tiled/chm/"
output_dem_directory <- "D:/santa_clara_birds/LiDAR_2024/raster_data_tiled/dem/"

# Get list of CHM files
chm_file_list <- list.files(input_chm_directory, full.names=TRUE, pattern="CHM.tif$")
dem_file_list <- list.files(input_chm_directory, full.names=TRUE, pattern="GEG_01M.tif$")

# For each file in the list, load and tile it
tileImage <- function(input_image_filepath, filename_suffix)
{
  
  new_tiles <- terra::makeTiles(terra::rast(input_image_filepath),
                                y = 1000, 
                                na.rm = TRUE,
                                buffer = 0, 
                                overwrite = TRUE,
                                filename = filename_suffix)
}
chm_tile_list <- lapply(chm_file_list, tileImage, filename_suffix = paste(output_chm_directory, "chm_tile_.tif", sep="")) %>%
  unlist()
dem_tile_list <- lapply(dem_file_list, tileImage, filename_suffix = paste(output_dem_directory, "dem_tile_.tif", sep="")) %>%
  unlist()

# Rename image tiles based on upper left corner coordiantes
renameImages <- function(input_image_filepath)
{
  # For some reason, terra::makeTiles returns filepaths even when a file isn't created (because it has no non-NA pixels)
  # For those cases, filter out non-existent files
  if(!file.exists(input_image_filepath))
    return(NA)
  # Get coordinates of upper left corner of image
  image_extent <- terra::ext(terra::rast(input_image_filepath))
  x_min <- as.numeric(image_extent$xmin)
  y_min <- as.numeric(image_extent$ymax)
  # Generate new filename
  #   Get original filename without '.tif'
  new_filename <- substr(input_image_filepath, 1, nchar(input_image_filepath)-4)
  #   Add tile coordinate information
  new_filename <- paste(new_filename, 
                        "_", x_min, "_", y_min, ".tif", 
                        sep="")
  # Rename the file
  file.rename(input_image_filepath, new_filename)
  # Return the new filename
  return(new_filename)
}

chm_tile_list_final <- lapply(chm_tile_list, renameImages)
dem_tile_list_final <- lapply(chm_tile_list, renameImages)

# Count up the total number of valid pixels to get an area estimate for the survey
valid_pixel_count <- lapply(chm_tile_list_final,
                            function(image_filepath){
                              if(is.na(image_filepath)) 
                                return(0)
                              return(sum(!is.na(values(terra::rast(image_filepath)))))
                            }) %>%
  unlist() %>%
  sum()
