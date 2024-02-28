#load packages
library(raster); library(ncdf4)

#list WDs
wd_salinity <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Salinity'
wd_temperature <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Temperature'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/White shark/Environmental variables/Variables'

##### FIGURE OUT HOW TO DEAL WITH COPERNICUS LAYERS #####

#now I am using only one time snap (06/03/2024), we need an average
#perhaps extract values from specific times and project over an average?

#I need to download per parts because the files are too large


#### Salinity ###

#read layers in .nc 
setwd(wd_salinity)

#from 0.49 to 92.33 m
sal_cop_1 <- brick('cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m_1709015225783.nc')

sal_cop_1 <- unstack(sal_cop_1)

#from 92.33 to 2865.70 m
sal_cop_2 <- brick('cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m_1709016276940.nc')

sal_cop_2 <- unstack(sal_cop_2)

#from 3597.03 to 5727.92.70 m
sal_cop_3 <- brick('cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m_1709017444567.nc')

sal_cop_3 <- unstack(sal_cop_3)

#concatenate all depths
sal_cop <- c(sal_cop_1, sal_cop_2, sal_cop_3)

#stack and save
sal_cop_raster <- stack(sal_cop)

setwd(wd_variables)
writeRaster(sal_cop_raster, filename = 'Salinity_20240306.tif', format = 'GTiff')

#### Temperature ###

#read layers in .nc 
setwd(wd_temperature)

#from 0.49 to 92.33 m
temp_cop_1 <- brick('cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m_1709016899278.nc')

temp_cop_1 <- unstack(temp_cop_1)

#from 92.33 to 2865.70 m
temp_cop_2 <- brick('cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m_1709017226377.nc')

temp_cop_2 <- unstack(temp_cop_2)

#from 3597.03 to 5727.92.70 m
temp_cop_3 <- brick('cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m_1709017337201.nc')

temp_cop_3 <- unstack(temp_cop_3)

#concatenate all depths
temp_cop <- c(temp_cop_1, temp_cop_2, temp_cop_3)

#stack and save
temp_cop_raster <- stack(temp_cop)

setwd(wd_variables)
writeRaster(temp_cop_raster, filename = 'Temperature_20240306.tif', format = 'GTiff')
