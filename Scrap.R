#### VARIABLES

#list WDs
wd_depth <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Depth'

#read layer in .nc 
setwd(wd_depth)
depth_copernicus <- brick('GLO-MFC_001_024_mask_bathy.nc')
depth_copernicus2 <- unstack(depth_copernicus)

#each layer of the brick is a mask of areas above a specific depth, these values are in the Z of the layer, transform the values of each layer into this 
depth_copernicus3 <- depth_copernicus2

for(i in 1:length(depth_copernicus3))
{
  layer_depth <- getZ(depth_copernicus2[[i]])
  depth_copernicus3[[i]][] <- ifelse(depth_copernicus3[[i]][] == 1, layer_depth, 0)
  print(i)
}

#restack layers
depth_copernicus4 <- stack(depth_copernicus3)

# calculate max depth
depth_copernicus5 <- calc(depth_copernicus4, fun = max)

#save depth raster
setwd(wd_depth)
writeRaster(depth_copernicus5, filename = 'Depth_copernicus.asc', format = 'ascii')


#each layer of the brick has the values of the variable at a specific depth, these values are in the Z of the layer. For each cell, I need to keep the value of the deepest layer that is not an NA so that I have bottom values.

#create an empty raster
salinity_copernicus_bottom <- salinity_copernicus2[[1]]
salinity_copernicus_bottom[] <- NA
salinity_copernicus_bottom <- setZ(salinity_copernicus_bottom, NA, name = 'Salinity_bottom')

#loop through each depth layer from the deepest to the shallowest
for(i in length(salinity_copernicus2):1)
{
  #make a copy of the layer
  copy_layer <- salinity_copernicus2[[i]]
  
  #transform any values already input into the new raster into NAs (those do not insterest us because they are not the bottom values)
  a <- which(!is.na(salinity_copernicus_bottom[]))
  if(length(a) > 0){
    copy_layer[a] <- NA
  }
  
  #identify which cells of the copy_layers have values and input the values into the bottom layer
  b <- which(!is.na(copy_layer[]))
  if(length(b) > 0){
    salinity_copernicus_bottom[b] <- copy_layer[b]
  }
  
  print(i)
}

#save raster
setwd(wd_variables)
writeRaster(salinity_copernicus_bottom, filename = 'Salinity_copernicus_bottom.asc', format = 'ascii')



#### Temperature ###

#read layer in .nc 
setwd(wd_variables)

temp_copernicus <- brick('cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m_1700347640709.nc')
temp_copernicus2 <- unstack(temp_copernicus)

#each layer of the brick has the values of the variable at a specific depth, these values are in the Z of the layer. For each cell, I need to keep the value of the deepest layer that is not an NA so that I have bottom values.

#create an empty raster
temp_copernicus_bottom <- temp_copernicus2[[1]]
temp_copernicus_bottom[] <- NA
temp_copernicus_bottom <- setZ(temp_copernicus_bottom, NA, name = 'Temp_bottom')

#loop through each depth layer from the deepest to the shallowest
for(i in length(temp_copernicus2):1)
{
  #make a copy of the layer
  copy_layer <- temp_copernicus2[[i]]
  
  #transform any values already input into the new raster into NAs (those do not insterest us because they are not the bottom values)
  a <- which(!is.na(temp_copernicus_bottom[]))
  if(length(a) > 0){
    copy_layer[a] <- NA
  }
  
  #identify which cells of the copy_layers have values and input the values into the bottom layer
  b <- which(!is.na(copy_layer[]))
  if(length(b) > 0){
    temp_copernicus_bottom[b] <- copy_layer[b]
  }
  
  print(i)
}

#save raster
setwd(wd_variables)
writeRaster(temp_copernicus_bottom, filename = 'Temperature_copernicus_bottom.asc', format = 'ascii')


#### Current_velocity ###

#read layer in .nc 
setwd(wd_variables)

current_copernicus <- brick('cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m_1700347253914.nc')
current_copernicus2 <- unstack(current_copernicus)

#each layer of the brick has the values of the variable at a specific depth, these values are in the Z of the layer. For each cell, I need to keep the value of the deepest layer that is not an NA so that I have bottom values.

#create an empty raster
current_copernicus_bottom <- current_copernicus2[[1]]
current_copernicus_bottom[] <- NA
current_copernicus_bottom <- setZ(current_copernicus_bottom, NA, name = 'Current_bottom')

#loop through each depth layer from the deepest to the shallowest
for(i in length(current_copernicus2):1)
{
  #make a copy of the layer
  copy_layer <- current_copernicus2[[i]]
  
  #transform any values already input into the new raster into NAs (those do not insterest us because they are not the bottom values)
  a <- which(!is.na(current_copernicus_bottom[]))
  if(length(a) > 0){
    copy_layer[a] <- NA
  }
  
  #identify which cells of the copy_layers have values and input the values into the bottom layer
  b <- which(!is.na(copy_layer[]))
  if(length(b) > 0){
    current_copernicus_bottom[b] <- copy_layer[b]
  }
  
  print(i)
}

#save raster
setwd(wd_variables)
writeRaster(current_copernicus_bottom, filename = 'Current_velocity_copernicus_bottom.asc', format = 'ascii')





