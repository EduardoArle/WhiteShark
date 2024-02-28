#load packages
library(sf); library(rnaturalearth); library(raster)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Occurrences'
wd_depth <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Depth'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Variables'
wd_projections <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Projections'
wd_plot_layers <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Projections/Plot_layers'
wd_plot_layers_depth <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Projections/Plot_layers_depth'

Plot_layers_depth

#load world map (package rnaturalearth)
world <- ne_countries(scale = 10, returnclass = "sf")
world <- st_make_valid(world)

#read table
setwd(wd_occ)
occ <- read.csv('C_carcharias_clean.csv')

#make spatial object (uses function crs from raster)
occ_sf <- st_as_sf(occ, coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(world))

#load depth layers
setwd(wd_depth)
depth <- brick('GLO-MFC_001_024_mask_bathy.nc')

#load variable layers
setwd(wd_variables)
sal <- stack('Salinity_20240306.tif')
temp <- stack('Temperature_20240306.tif')

#columns in occ_sf indicating layer, depth, delta in the variables for each point
occ_sf$layerID <- NA
occ_sf$depthVariables <- NA
occ_sf$depthDelta <- NA

#identify which environmental layer is closes to the variable
for(i in 1:length(occ_sf$depthVariables))
{
  #indicate layer ID
  occ_sf$layerID[i] <- as.numeric(which.min(abs(occ_sf$depth[i] - getZ(depth))))
  
  #indicate the depth layer for each point
  occ_sf$depthVariables[i] <- getZ(depth)[occ_sf$layerID[i]]
  
  #calculate the diff between occ$depth and depth layer for each point
  occ_sf$depthDelta[i] <- occ_sf$depth[i] - occ_sf$depthVariables[i]
}

#extract varible values at each occ point (lat, lon, depth)
occ_sf$salinity <- NA
occ_sf$temperature <- NA

for(i in 1:length(occ_sf$salinity))
{
  #extract salinity values
  occ_sf$salinity[i] <- extract(sal[[occ_sf$layerID[i]]], occ_sf[i,])
  
  #extract temperature values
  occ_sf$temperature[i] <- extract(temp[[occ_sf$layerID[i]]], occ_sf[i,])
  
  print(i)
}

#exclude points with NA in the variables
occ_sf_2 <- occ_sf[complete.cases(occ_sf$salinity),]

#make enveloppe (min and max of each variable)
min_sal <- min(occ_sf_2$salinity)
max_sal <- max(occ_sf_2$salinity)
min_temp <- min(occ_sf_2$temperature)
max_temp <- max(occ_sf_2$temperature)

#duplicate one of the variables to project the model
model <- sal

#project model in each cell of each layer
for(i in 1:nlayers(model))
{
    model[[i]][] <- ifelse(sal[[i]][] >= min_sal &
                              sal[[i]][] <= max_sal &
                              temp[[i]][] >= min_temp &
                              temp[[i]][] <= max_temp,
                            1, 0)

    print(paste0('Layer ', (i)))
}

#save model projections
setwd(wd_projections)
writeRaster(model, filename = 'Naive_model.tif', format = 'GTiff')

#visualise models

#make break points to plot 0s in one colour and 1s in another colour
br <- c(0, 0.5, 1)
col <- c('#6baed6', '#f03b20', '#f03b20')

#plot and save each layer of visualisation
for(i in 1:nlayers(model))
{
  #setwd(wd_res)
  setwd(wd_plot_layers)
  pdf(file = paste0(round(getZ(depth[[i]]), 2),' m.pdf'))
  
  #set parametres for plotting
  par(mar = c(5,5,5,5))
  
  #plot layer
  plot(model[[i]], breaks = breakpoints, col = colours,
       main = paste0(round(getZ(depth[[i]]), 2), ' m'))
  
  dev.off()
}

#make ensembles for every 5 depth layers from bottom to top

#step 1: transform all NA cells that are not NAs in the upper levels into 0s

# 50_46
mod_50 <- model[[50]]
mod_49 <- model[[49]]
mod_48 <- model[[48]]
mod_47 <- model[[47]]
mod_46 <- model[[46]]

mod_50[is.na(model[[50]][]) & !is.na(model[[46]][])] <- 0
mod_49[is.na(model[[49]][]) & !is.na(model[[46]][])] <- 0
mod_48[is.na(model[[48]][]) & !is.na(model[[46]][])] <- 0
mod_47[is.na(model[[47]][]) & !is.na(model[[46]][])] <- 0

# 45_41
mod_45 <- model[[45]]
mod_44 <- model[[44]]
mod_43 <- model[[43]]
mod_42 <- model[[42]]
mod_41 <- model[[41]]

mod_45[is.na(model[[45]][]) & !is.na(model[[41]][])] <- 0
mod_44[is.na(model[[44]][]) & !is.na(model[[41]][])] <- 0
mod_43[is.na(model[[43]][]) & !is.na(model[[41]][])] <- 0
mod_42[is.na(model[[42]][]) & !is.na(model[[41]][])] <- 0

# 40_36
mod_40 <- model[[40]]
mod_39 <- model[[39]]
mod_38 <- model[[38]]
mod_37 <- model[[37]]
mod_36 <- model[[36]]

mod_40[is.na(model[[40]][]) & !is.na(model[[36]][])] <- 0
mod_39[is.na(model[[39]][]) & !is.na(model[[36]][])] <- 0
mod_38[is.na(model[[38]][]) & !is.na(model[[36]][])] <- 0
mod_37[is.na(model[[37]][]) & !is.na(model[[36]][])] <- 0

# 35_31
mod_35 <- model[[35]]
mod_34 <- model[[34]]
mod_33 <- model[[33]]
mod_32 <- model[[32]]
mod_31 <- model[[31]]

mod_35[is.na(model[[35]][]) & !is.na(model[[31]][])] <- 0
mod_34[is.na(model[[34]][]) & !is.na(model[[31]][])] <- 0
mod_33[is.na(model[[33]][]) & !is.na(model[[31]][])] <- 0
mod_32[is.na(model[[32]][]) & !is.na(model[[31]][])] <- 0

# 30_26
mod_30 <- model[[30]]
mod_29 <- model[[29]]
mod_28 <- model[[28]]
mod_27 <- model[[27]]
mod_26 <- model[[26]]

mod_30[is.na(model[[30]][]) & !is.na(model[[26]][])] <- 0
mod_29[is.na(model[[29]][]) & !is.na(model[[26]][])] <- 0
mod_28[is.na(model[[28]][]) & !is.na(model[[26]][])] <- 0
mod_27[is.na(model[[27]][]) & !is.na(model[[26]][])] <- 0

# 25_21
mod_25 <- model[[25]]
mod_24 <- model[[24]]
mod_23 <- model[[23]]
mod_22 <- model[[22]]
mod_21 <- model[[21]]

mod_25[is.na(model[[25]][]) & !is.na(model[[21]][])] <- 0
mod_24[is.na(model[[24]][]) & !is.na(model[[21]][])] <- 0
mod_23[is.na(model[[23]][]) & !is.na(model[[21]][])] <- 0
mod_22[is.na(model[[22]][]) & !is.na(model[[21]][])] <- 0

# 20_16
mod_20 <- model[[20]]
mod_19 <- model[[19]]
mod_18 <- model[[18]]
mod_17 <- model[[17]]
mod_16 <- model[[16]]

mod_20[is.na(model[[20]][]) & !is.na(model[[16]][])] <- 0
mod_19[is.na(model[[19]][]) & !is.na(model[[16]][])] <- 0
mod_18[is.na(model[[18]][]) & !is.na(model[[16]][])] <- 0
mod_17[is.na(model[[17]][]) & !is.na(model[[16]][])] <- 0

# 15_11
mod_15 <- model[[15]]
mod_14 <- model[[14]]
mod_13 <- model[[13]]
mod_12 <- model[[12]]
mod_11 <- model[[11]]

mod_15[is.na(model[[15]][]) & !is.na(model[[11]][])] <- 0
mod_14[is.na(model[[14]][]) & !is.na(model[[11]][])] <- 0
mod_13[is.na(model[[13]][]) & !is.na(model[[11]][])] <- 0
mod_12[is.na(model[[12]][]) & !is.na(model[[11]][])] <- 0

# 10_06
mod_10 <- model[[10]]
mod_09 <- model[[9]]
mod_08 <- model[[8]]
mod_07 <- model[[7]]
mod_06 <- model[[6]]

mod_10[is.na(model[[10]][]) & !is.na(model[[6]][])] <- 0
mod_09[is.na(model[[9]][]) & !is.na(model[[6]][])] <- 0
mod_08[is.na(model[[8]][]) & !is.na(model[[6]][])] <- 0
mod_07[is.na(model[[7]][]) & !is.na(model[[6]][])] <- 0

# 05_01
mod_05 <- model[[5]]
mod_04 <- model[[4]]
mod_03 <- model[[3]]
mod_02 <- model[[2]]
mod_01 <- model[[1]]

mod_05[is.na(model[[5]][]) & !is.na(model[[1]][])] <- 0
mod_04[is.na(model[[4]][]) & !is.na(model[[1]][])] <- 0
mod_03[is.na(model[[3]][]) & !is.na(model[[1]][])] <- 0
mod_02[is.na(model[[2]][]) & !is.na(model[[1]][])] <- 0

#step 2: make the ensemble (majority rule) 
mod_50_46 <- sum(mod_50, mod_49, mod_48, mod_47, mod_46)
mod_50_46[] <- ifelse(mod_50_46[] >= 2.5, 1, 0)

mod_45_41 <- sum(mod_45, mod_44, mod_43, mod_42, mod_41)
mod_45_41[] <- ifelse(mod_45_41[] >= 2.5, 1, 0)

mod_40_36 <- sum(mod_40, mod_39, mod_38, mod_37, mod_36)
mod_40_36[] <- ifelse(mod_40_36[] >= 2.5, 1, 0)

mod_35_31 <- sum(mod_35, mod_34, mod_33, mod_32, mod_31)
mod_35_31[] <- ifelse(mod_35_31[] >= 2.5, 1, 0)

mod_30_26 <- sum(mod_30, mod_29, mod_28, mod_27, mod_26)
mod_30_26[] <- ifelse(mod_30_26[] >= 2.5, 1, 0)

mod_25_21 <- sum(mod_25, mod_24, mod_23, mod_22, mod_21)
mod_25_21[] <- ifelse(mod_25_21[] >= 2.5, 1, 0)

mod_20_16 <- sum(mod_20, mod_19, mod_18, mod_17, mod_16)
mod_20_16[] <- ifelse(mod_20_16[] >= 2.5, 1, 0)

mod_15_11 <- sum(mod_15, mod_14, mod_13, mod_12, mod_11)
mod_15_11[] <- ifelse(mod_15_11[] >= 2.5, 1, 0)

mod_10_06 <- sum(mod_10, mod_09, mod_08, mod_07, mod_06)
mod_10_06[] <- ifelse(mod_10_06[] >= 2.5, 1, 0)

mod_05_01 <- sum(mod_05, mod_04, mod_03, mod_02, mod_01)
mod_05_01[] <- ifelse(mod_05_01[] >= 2.5, 1, 0)

#step 3: plot ensemble for the layers and upper layers with transparency
par(mar = c(1,1,1,1))

#set colours for the transparency layers
br <- c(0, 0.5, 1)
col_trans <- '#6baed61a'
col_trans_mod <- c('#6baed61a', '#f03b20', '#f03b20')


#mod_50_46
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[45]]), 2), '_to_',
                  round(getZ(depth[[50]]), 2), '_m.pdf'))

#here comes the layer with the data
plot(mod_50_46, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[45]]), 2), ' to ',
                     round(getZ(depth[[50]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()

#mod_45_41
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[40]]), 2), '_to_',
                  round(getZ(depth[[45]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[40]]), 2), ' to ',
                   round(getZ(depth[[45]]), 2), ' m'))

#here comes the layer with the data
par(new = T)
plot(mod_45_41, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_40_36
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[35]]), 2), '_to_',
                  round(getZ(depth[[40]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[35]]), 2), ' to ',
                   round(getZ(depth[[40]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_40_36, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_35_31, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_35_31
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[30]]), 2), '_to_',
                  round(getZ(depth[[35]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[30]]), 2), ' to ',
                   round(getZ(depth[[35]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_35_31, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_30_26
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[25]]), 2), '_to_',
                  round(getZ(depth[[30]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[25]]), 2), ' to ',
                   round(getZ(depth[[30]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_30_26, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_25_21
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[20]]), 2), '_to_',
                  round(getZ(depth[[25]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[20]]), 2), ' to ',
                   round(getZ(depth[[25]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_25_21, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_20_16
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[15]]), 2), '_to_',
                  round(getZ(depth[[20]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[15]]), 2), ' to ',
                   round(getZ(depth[[20]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_20_16, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_15_11
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[11]]), 2), '_to_',
                  round(getZ(depth[[15]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[11]]), 2), ' to ',
                   round(getZ(depth[[15]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_15_11, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

par(new = T)
plot(mod_10_06, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_10_06
setwd(wd_plot_layers_depth)
pdf(file = paste0(round(getZ(depth[[05]]), 2), '_to_',
                  round(getZ(depth[[10]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0(round(getZ(depth[[05]]), 2), ' to ',
                   round(getZ(depth[[10]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_10_06, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()


#mod_06_01
setwd(wd_plot_layers_depth)
pdf(file = paste0('0_to_',
                  round(getZ(depth[[05]]), 2), '_m.pdf'))

plot(mod_50_46, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F,
     main = paste0('0 to ',
                   round(getZ(depth[[10]]), 2), ' m'))
par(new = T)
plot(mod_45_41, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31,  col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = col_trans,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = col_trans, 
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

#here comes the layer with the data
par(new = T)
plot(mod_05_01, breaks = br, col = col_trans_mod,
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)


dev.off()

#############

# #step 0: make transparency rasters to plot on top depicting depth layers
# transp_45_41 <- 
# transp_40_36
# transp_35_31
# transp_30_26
# transp_25_21
# transp_20_16
# transp_15_11
# transp_10_06
# transp_05_01



pdf(file = 'Test2.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test3.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()

pdf(file = 'Test4.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test5.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test6.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test7.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test8.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test9.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()


pdf(file = 'Test10.pdf')

plot(mod_50_46, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_45_41, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_40_36, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_35_31, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_30_26, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_25_21, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_20_16, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_15_11, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_10_06, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)
par(new = T)
plot(mod_05_01, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F)

dev.off()



t <-  stack(mod_50_46, mod_45_41, mod_40_36, mod_35_31)

plot(mod_30_26, breaks = br, col = '#6baed61a',
     legend = F, frame = F, box = F, maxpixels = 5000000, axes = F, add = T)

stack(mod_50_46, mod_45_41, mod_40_36, mod_35_31)

breakpoints <- c(0, 0.5, 1)
colours <- c('#6baed61a', '#f03b20', '#f03b20')
plot(model[[1]], breaks = breakpoints, col = colours)
plot(model[[1]])
plot(st_geometry(occ_sf), add = T,
     pch = 21, cex = 0.6, col = 'black', bg = 'red')


