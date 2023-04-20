# Custom Raster Tiling
##### _Chris Mulverhill_
#
#
Let's say you are running some functions on a raster or stack of rasters. This raster stack is huge and the functions will either take a long time or are very memory intensive. 

You can use `terra::makeTiles` if you'd like to cut out the tiles and process them, but to do that you either need another shapefile or a coarser raster. How do you do that?

This is a custom function to make a shapefile of these tiles. With this shapefile, you can either:
- Crop the raster(s) yourself and perform some function on them (example provided), then combine the outputs at the end
- Use the shapefile as an input to `terra::vrt`

Either way, the code and some examples are provided. Contact Chris if you have questions about its use. 

Happy tiling!
