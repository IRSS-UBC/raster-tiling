######################################################
######################################################
############# Make boxes that you can use ############
#### to crop and process a raster or raster stack ####
######################################################
########## Chris Mulverhill - 4/20/2023 ;) ###########
######################################################
######################################################

make.boxes = function(rstr, # Input raster
                      size, # Box size (in m)
                      overlap = 30, # How much do you want the boxes to overlap (in m)?
                      progress.bar = T, # Progress bar? Need pbapply to be installed
                      keep.all = F){# Keep all of the boxes or just the ones that contain valid pixels?
  
  # Install packages if they aren't already installed
  if(length(find.package("pbapply", quiet = T)) == 0){cat("\nInstalling pbapply"); install.packages("pbapply")}
  if(length(find.package("exactextractr", quiet = T)) == 0){cat("\nInstalling exactextractr"); install.packages("exactextractr")}
  if(length(find.package("terra", quiet = T)) == 0){cat("\nInstalling terra"); install.packages("terra")}
  if(length(find.package("stringr", quiet = T)) == 0){cat("\nInstalling stringr"); install.packages("stringr")}
  if(length(find.package("sf", quiet = T)) == 0){cat("\nInstalling sf"); install.packages("sf")}
  if(length(find.package("purrr", quiet = T)) == 0){cat("\nInstalling purrr"); install.packages("purrr")}
  
  if(progress.bar == F){pbapply::pboptions(type = "none")} # For the losers who don't want a progress bar
  
  # Make a matrix of all possible boxes in the spatial extent of the raster
  xv = seq(xmin(rstr), xmax(rstr), by = size)
  yv = seq(ymin(rstr), ymax(rstr), by = size)

  box.ctrs = expand.grid(xv, yv) |> as.matrix()

  # Pad the box names so that they are the same length
  n.pad = nrow(box.ctrs) |> stringr::str_length()
  
  # Make all of the boxes - obviously if you have many boxes to make, this might take a bit
  boxes = pbapply::pbapply(box.ctrs, 1, FUN = function(rw){
    x = rw[1]
    y = rw[2]
    
    n.name = which(box.ctrs[,1] == x & box.ctrs[,2] == y) |>
             stringr::str_pad(width = n.pad, side = "left", pad = "0")
    
    # Make a box
    box.sub <- cbind(c(x - overlap, x - overlap, x + size + overlap, x + size + overlap),
                     c(y - overlap, y + size + overlap, y + size + overlap, y - overlap)) |> 
               terra::vect(type="polygons", crs = crs(rstr))
    
    box.sub$name = n.name
    
    # If you only want to keep boxes with valid pixels (helpful for non-rectangular rasters)
    if(keep.all == F){
      # See if the box overlaps the raster
      test1 = sf::st_as_sf(box.sub) %>% exact_extract(rstr, .) |> dplyr::bind_rows() |> dplyr::pull(value) |> na.omit()
      
      if(length(test1) > 0){return(box.sub)}
      else(return())
    }

    # If you want to return all boxes, including potentially blank ones
    return(box.sub)

  })
  return(vect(purrr::compact(boxes)))
}

######################################################
######################################################
# Now, test it out
library(terra); library(dplyr); library(exactextractr); library(pbapply)

# Raster of the area, or could also be the first layer of a stack 
r = rast("inputs/romeo_blank.tif")

# Somewhere in the neighborhood of 100-200 boxes is probably ideal, 
# depending on the type of function you're hoping to run. However, 
# you should experiment with the size so that this function 
# doesn't take a long time to run. If it does, then look into making the
# boxes a little bit larger. As an example, I have the size parameter
# set to 10000 (10 km) for an area that is approximately 
# 12 x 12 boxes in either direction.
bxs = make.boxes(r, size = 10000)

plot(r, col = "darkgreen", ext = ext(bxs))
plot(bxs, add = T)
text(bxs, bxs$name)

######################################################
######################################################
# Now, run some function on it, sending the outputs to a common folder
# The function below is very simple but obviously this can be something much more complex

# Need an empty folder to store the outputs
folder.path = "outputs"
if(!dir.exists(folder.path)){dir.create(folder.path)} # Create it if it doesn't exist

# This could also be a for loop, but the same concept would stil apply
pblapply(1:length(bxs), FUN = function(b){
  nm <- bxs$name[b]
  
  # What will be the output file path?
  out.fp = paste0(folder.path, "/box", nm, "_output.tif")
  
  # Skip it if it already exists
  if(file.exists(out.fp)){return(NA)}

  r.sub <- crop(r, bxs[b]) # This can also be a stack if needed, or whatever you need to process something on
  
  # Run some function on the subset
  r.out = runif(1, min = 10, max = 100) %>% floor() %>% `*`(r.sub) %>% as.int()
  
  writeRaster(r.out, out.fp, overwrite = T)
  return(NA)
})

# Now, combine them
# If you expect overlapping values to be the same, you can use terra::vrt
fl = list.files(folder.path, pattern = "_output.tif$", full.names = T)

r.final = vrt(fl)

plot(r.final)

writeRaster(r.final, paste0(folder.path, "/_final.tif"), overwrite = T)
































