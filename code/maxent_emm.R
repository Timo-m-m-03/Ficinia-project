##### SDM using Maxent 




# Install and load packages -----------------------------------------------------------------

if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}

if(!require(sf)){remotes::install_github(repo = "r-spatial/sf", ref = "93a25fd8e2f5c6af7c080f92141cb2b765a04a84"); library(sf)}

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-24")
library(rJava)

CRS_WGS84 <- "+proj=longlat +datum=WGS84 +no_defs"

p_load(virtualspecies, dismo, terra, rJava, tidyverse, sf, mapview, rgbif)

file.exists("data/maxent/maxent.jar") 
system.file("java", package="dismo")

if (file.exists(jar) & require(rJava)){print("GOOD TO GO")} else print("NO GO")

#Check if Maxent is working...should give a version number
maxent()

# Read in the border of southern Africa (from working directory) and plot the border with your occ points
ZA_border <- sf::st_read(dsn = "data/enviro_layers", layer = "southern_africa")
ZA_border <- as(st_union(ZA_border), "Spatial")
plot(ZA_border)

# First load the current environmental variables as a raster stack
current_names <- sort(list.files(path="data/enviro_layers/Bioclim_asc_SA_model", pattern=".asc$", full.names = TRUE,recursive = FALSE, include.dirs = FALSE))
cur_predictors <- stack(current_names)
proj4string(cur_predictors) <- CRS_WGS84
names(cur_predictors)
extent(cur_predictors)

#This WKT string is used with ABAP data to define region of interest
WKT_string <- bounding_wkt(min_x = extent(cur_predictors)[1], min_y=extent(cur_predictors)[3], max_x=extent(cur_predictors)[2], max_y=extent(cur_predictors)[4])
#This has the format: "POLYGON((16 -35,16 -21.7083,33.375 -21.7083,33.375 -35,16 -35))"

# Decide whether you want on-screen display or pdf written to disk and run one:
pdf <- "on"
pdf <- "off"


# Load species occurrence data -------------------------------------------------

# Clean species OCC data ----------------------------------------------------

# Convert occ dataframe to a spatial points layer
occ_sp <- st_as_sf(occ, coords = c("lon", "lat"), crs = CRS_WGS84)

#plot...just to check
dev.off()
plot(ZA_border)
points(occ_sp$geometry)

# 1) Only keep points that lie inside Southern Africa (remember those GBIF errors: zero lon or lat, or georeff locations in the sea etc.)
occ_sp <- st_crop(occ_sp, ZA_border)
plot(ZA_border)
points(occ_sp)

#Write it out for checking, for example,in QGIS
st_write(occ_sp, paste0(getwd(), "/occ_sp.shp"), append=F)

#Read the modified data back in
occ_sp <- st_read(paste0(getwd(), "/occ_sp.shp"))

dev.off()
plot(ZA_border)
points(occ_sp$geometry)
mapview(occ_sp)

# 2) Only keep points that lie on raster grid cells with information (not on NA's)

# Extract data from the raster stack and remove rows where there are NA's.
na.points <- cbind.data.frame(st_coordinates(occ_sp),raster::extract(cur_predictors, occ_sp))
names(na.points)[1:2] <- c("lon", "lat")
na.points <- na.omit(na.points) # number of records may or may not decrease

# Select required lon+lat columns
occ <- na.points[,1:2]

# Save the cleaned dataframe to disk (so that you don't need to repeat the above working again)
write.csv(data.frame(species, occ), "occ_clean.csv", row.names=F)

# CURRENT env predictors ---------------------------------------------------

# Climate data come from WORLDCLIM (http://worldclim.org/version2): averaged between 1970-2000 at 2.5 minutes. Soils data come from SoilGrids1km (https://soilgrids.org) and have been manually averaged across soil depths/layers. Other environmental variables are derived from a digital elevation model (DEM, e.g. Shuttle Radar Topography Mission) and calculated using the raster::terrain function in R.

# Reload OCC data and convert to spatial points layer (CAN START FROM HERE)
occ <- read.csv("occ_clean.csv")[,c(2,3)]

#Subsample data to limit the number of records (for processing speed)
sample_num <- 10000
if (nrow(occ)>sample_num) {occ_sp <- sample_n(occ, sample_num, replace=F)} else {occ_sp <- occ}
occ_sp <- st_as_sf(occ, coords = c("lon", "lat"), crs = CRS_WGS84)


# Check the current environmental conditions are loaded as a raster stack (if not, see above - line 144 - and re-load)

# Steps:
# 1) rescale some variables/predictors
# 2) rename raster layers
# 3) check collinearity
# 4) split the variables/predictors into climate versus all

# 1)
# Temperature variables and soil pH need to be divided by 10 (a relic from the downloaded data). These are the layers that need to be corrected:
correct_list <- c("bio__1","bio__2","bio__3","bio__4","bio__5","bio__6","bio__7","bio__8","bio__9","bio__10","bio__11","PHIHOX_d_mean")
# Find them in the raster stack and then correct them
for (i in match(correct_list, names(cur_predictors))){cur_predictors[[i]]<- cur_predictors[[i]]/10}


# 2)
names(cur_predictors)

# Generate a data frame of old Codes and new Names (something more informative than "bio...") for the climate and other enriconmental variables (using dplyr package)
all_env_codes <- data.frame("Code" = names(cur_predictors), stringsAsFactors=F) %>%
  mutate(Name = Code) %>%
  mutate(Name = recode(Name, Aspect="Aspect", BDRICM_d_mean="Depth.To.Bedrock", bio__1="MAT", bio__2="Mean.Diurnal.Range", bio__3="Isothermality", bio__4="T.Seasonality", bio__5="Max.T.Warmest.Month", bio__6="Min.T.Coldest.Month", bio__7="T.Annual.Range", bio__8="Mean.T.Wettest.Q", bio__9="Mean.T.Driest.Q", bio__10="Mean.T.Warmest.Q", bio__11="Mean.T.Coldest.Q", bio__12="MAP", bio__13="P.Wettest.Month", bio__14="P.Driest.Month", bio__15="P.Seasonality", bio__16="P.Wettest.Q", bio__17="P.Driest.Q", bio__18="P.Warmest.Q", bio__19="P.Coldest.Q", BLD_d_mean="Bulk.Density", CEC_d_mean="Cation.Exchange.Capacity", CLYPPT_d_mean="Clay", CRFVOL_d_mean="Course.Fragments", Distance_coast="Distance.Coast", Elevation="Elevation", ORCDRC_d_mean="Organic.C.Content", PHIHOX_d_mean="pH", Ruggedness="Ruggedness", Slope="Slope", SLTPPT_d_mean="Silt", SNDPPT_d_mean="Sand"))

# Use this dataframe of names to rename the raster layers
names(cur_predictors) <- all_env_codes$Name[match(names(cur_predictors), all_env_codes$Code)]
names(cur_predictors)


# 3) 
# Crop the predictors to an area around the points, but inflated by 50% (this becomes the study extent).
cur_predictors_crop <- crop(cur_predictors, extent(occ_sp)*1.5)

# Determine the number of background points used to extract data from the raster layers, based on the area of the study extent. This env data is then fed into a function that checks for collinearity
if(ncell(cur_predictors_crop)<12000) {bg_points <- ncell(cur_predictors_crop)/2} else {bg_points <- 10000}

# Info on the virtualspecies::removeCollinearity function
help(removeCollinearity)

# Determine which predictors are collinear, produce a list of those that are NOT collinear and keep these. The Pearson's correlation coefficient cutoff is 0.7. This particular function automatically chooses 1 predictor within a group of correlated variables. You can, however, compute the collinearity function, return the plot output and use this as a guide to manually select the predictors yourself based on "sound" ecological knowledge. 
retained_predictors <- removeCollinearity(stack(cur_predictors_crop), multicollinearity.cutoff = 0.7, select.variables = TRUE, sample.points = TRUE, nb.points = bg_points, plot = TRUE)


# Specify the names of the predictors to be kept manually. For example:
names(cur_predictors_crop)
# retained_predictors <- c("Aspect", "BDRLOG_d_mean", "MAT", "Mean.T.Warmest.Quarter", "MAP", "P.Driest.Quarter", "P.Seasonality", "T.Annual.Range", "Isothermality", "Mean.T.Driest.Quarter", "Mean.T.Wettest.Quarter", "BLD_d_mean", "CEC_d_mean", "CLYPPT_d_mean", "Distance_coast", "ORCDRC_d_mean", "Slope")

# Create a list of the predictors (based on their numbered position in the raster stack) that will be kept and select only these
retained_predictors_id <- which(names(cur_predictors_crop) %in% retained_predictors)
cur_predictors_crop <- subset(cur_predictors_crop, retained_predictors_id, drop=FALSE)

# Plot maps of the current predictors -------------------------------------

if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "predictors_current", ".pdf", sep=""), width=20, height=20)}
par(oma=c(1,2,1,1)) # increase outer margin so plots have enough space
plot(cur_predictors_crop, maxnl=20) # check how many predictors you have in your stack, if more than 20 then change this number
if (pdf=="on"){dev.off()} # switch pdf off


# 4) 
# Create a list of the climate predictors (based on their numbered position in the raster stack) that will be kept and select only these

bioclim_codes <- all_env_codes %>%
  filter(str_detect(Code, "bio")) %>%
  dplyr::select(Name) %>%
  pull()

climate_predictors_id <- which(names(cur_predictors_crop) %in% bioclim_codes)
cur_predictors_crop_clim <- subset(cur_predictors_crop, climate_predictors_id, drop=FALSE)

rm(cur_predictors)

#### At the end of this section you should have two raster stacks to be used in the Maxent models: 
#### "cur_predictors_crop" (all current predictors) and "cur_predictors_crop_clim" (only current climate predictors)



# FUTURE env predictors ---------------------------------------------------

# Future climate data is projected for 2050 under the RCP6 (Representative Concentration Pathway Scenario 6; from the Community Climate System Model (CCSM), produced by NCAR in Boulder, Colorado (USA) and used in IPPC5).

# The code below is simpler than for the CURRENT env predictors because we use some of the previous working

# Load the future climate predictions as rasters
future_names <- sort(list.files(path="SDM_Maxent_cloud/Maxent_enviro_layers/CCSM4_2050_rcp60_asc_SA", pattern=".asc$", full.names = TRUE,recursive = FALSE, include.dirs = FALSE))
future_predictors <- stack(future_names)
proj4string(future_predictors) <- "+proj=longlat +datum=WGS84 +no_defs"

# Crop the future rasters to the extent of the current rasters
future_predictors_crop <- crop(future_predictors, extent(cur_predictors_crop))

# Divide temperature variables and soil pH by 10 (a relic from the downloaded data)
for (i in match(correct_list, names(future_predictors_crop))){future_predictors_crop[[i]]<- future_predictors_crop[[i]]/10}

# Rename the predictors
names(future_predictors_crop) <- all_env_codes$Name[match(names(future_predictors_crop), all_env_codes$Code)]

# Drop the predictors that are highly collinear in the current raster stack from the future raster stack
future_predictors_crop <- subset(future_predictors_crop, retained_predictors_id, drop=FALSE)

# Make a subset of future_predictors with only the climate variables
future_predictors_crop_clim <- subset(future_predictors_crop, climate_predictors_id, drop=FALSE)

names(future_predictors_crop)
names(future_predictors_crop_clim)

# Plot maps of the future predictors --------------------------------------
# (obviously only the climate predictors will change into the future)
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "predictors_future", ".pdf", sep=""), width=20, height=20)}
par(oma=c(1,2,1,1))
plot(future_predictors_crop, maxnl=20)
if (pdf=="on"){dev.off()}


#### At the end of this section you should have two more raster stacks to be used in the Maxent models: 
#### "future_predictors_crop" (all future predictors) and "future_predictors_crop_clim" (only future climate predictors)


# Compare current and future predictors ----------------------------------------------

# What changed in climate variables between 2050 and current? Do the substraction:
clim_change <- future_predictors_crop_clim - cur_predictors_crop_clim

# Plot the actual change
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "climate_change", ".pdf", sep=""), width=10, height=10)}
par(oma=c(1,2,1,1))
plot(clim_change, xlab="Lon", ylab="Lat", las=1, col=rev( rainbow(10, start=0,end=1)), legend=TRUE, maxnl=10)
if (pdf=="on"){dev.off()}

# Normalise the change to a scale between -1 and 1 and plot all on same scale so you can compare between the variables
clim_change_norm <- ((clim_change-min(clim_change))/(max(clim_change)-min(clim_change)))*2-1
brks<- round(seq(-1, 1,length.out=10),2)
names(clim_change_norm) <- names(clim_change)

# Plot the normalised change
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "climate_change_norm", ".pdf", sep=""), width=10, height=10)}
par(oma=c(1,2,1,1))
plot(clim_change_norm, xlab="Lon", ylab="Lat", las=1, col=rev(rainbow(10, start=0,end=1)), breaks=brks, maxnl=10)
if (pdf=="on"){dev.off()}



# Build Maxent model ------------------------------------------------------

# Withold a 10% sample of occ points for testing the model to make sure it is doing a decent job (see later)
k=10
fold <- kfold(occ, k=k)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]
#Note: Here we are selecting just one random training/test data set for use in the evaluation of the models. This is NOT optimal. This basic version of maxent implemented here does not allow proper cross-validation without going to a huge amount of trouble. One needs to use other packages to run proper cross-validation. 

# NB Information on the Maxent model: 
# "x" = raster stack 
# "p" = presence/occurence dataframe (here we used the "occtrain" data from the kfold split to "train" the model) 
# "a" = background points dataframe (in this model we have not specified "a". In this case Maxent randomly scatters 10000 background points over the extent of the environmental predictors raster stack. For future SDMs you should carefully consider where the background points are drawn from. See Elith et al. 2011)
# removeDuplicates = selects only one point in each raster grid cell

# args/arguments to specify:
# randomtestpoints = 30% of the data used for testing, 70% used for training (note we split the data twice, once kfold and once inside the model run) 
# betamultiplier = regularisation coefficient = 1
# features = linear, quadratic and hinge
# Asks for response curves and jackknife (measure importance of each predictor variable by training with each predictor variable first omitted, then used in isolation)

# There are many parameters under the maxent function which can be changed/specified. Here we are sticking to most of the default parameters. If you need to generate SDMs in the future you must interrogate these parameters and choose the best for YOUR model. 

#Note this "housecleaning" only needed to save memory on Rstudio cloud
rm(p_atlas)
rm(future_predictors)
rm(na.points)
rm(ZA_border)
rm(clim_change)
rm(clim_change_norm)

# Run the Maxent model for current climate predictors only

me_clim <- maxent(x = cur_predictors_crop_clim, 
                  p = occtrain,
                  replicates = 5,
                  removeDuplicates=TRUE, 
                  path = paste0(getwd(), "/SDM_Maxent_cloud/Maxent_out_clim"), 
                  args = c("randomtestpoints=30", "betamultiplier=1", "linear=true", "quadratic=true", "product=true", "threshold=false", "hinge=false", "threads=2", "responsecurves=true", "jackknife=true",  "askoverwrite=false")) # Note that the jacknife takes a longer time to run

# Run the Maxent model for all current predictors
me <- maxent(x = cur_predictors_crop, 
             p = occtrain,  
             removeDuplicates=TRUE, 
             path = paste0(getwd(), "/SDM_Maxent_cloud/Maxent_out"), 
             args = c("randomtestpoints=30", "betamultiplier=1", "linear=true", "quadratic=true", "product=true", "threshold=true", "hinge=false", "threads=2", "responsecurves=true", "jackknife=true",  "askoverwrite=false")) # Note that the jacknife takes a longer time to run


#The saving and reloading of the workspace is only to save on memory and is not needed if you are not running short of RAM in the indicator on the top ribbon. If you are running short of RAM (indicator is orange or red) then save the workspace now, relaunch the session (by clicking ... on top ribbon) and then restart from here by loading the workspace and then continuing...
#save.image("/cloud/project/SDM_Maxent_cloud/Maxent_Analysis_workspace.RData")
#load("/cloud/project/SDM_Maxent_cloud/Maxent_Analysis_workspace.RData")
#setwd(paste0(getwd()))
#if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
#p_load(rgdal,dismo,maptools,raster,rgeos,rJava,tidyverse, sf)

# Find the file "species.html" in both the "maxent_out" or "maxent_out_clim" directories, and open it in your browser = results

# Testing the model -------------------------------------------------------

# scatter background points (use the number of bg points determined above, from the sizestudy extent)
bg_clim <- randomPoints(cur_predictors_crop_clim, bg_points)
bg <- randomPoints(cur_predictors_crop, bg_points)

# Use "evaluate" to get the reliability of the model (generated with the "occtrain" data) against the "occtest" data. Note that an AUC of 0.8 is acceptable
help(evaluate)
(eval_me_clim <- evaluate(me_clim, x = cur_predictors_crop_clim, p = occtest, a = bg_clim))
me_clim_thresh <- as.numeric(threshold(eval_me_clim)[2])
(eval_me <- evaluate(me, x = cur_predictors_crop, p = occtest, a = bg))
me_thresh <- as.numeric(threshold(eval_me)[2])


# A receiver operating characteristic (ROC), or ROC curve, is a graphical plot that illustrates the performance of a binary classifier system as its discrimination threshold is varied. The curve is created by plotting the true positive rate (TPR) against the false positive rate (FPR) as the model is developed. The Area Under Curve (AUC) is 1 for a perfect identification of all true positives. You may be wondering where the name "Reciever Operating Characteristic" came from. ROC analysis is part of a field called "Signal Detection Theory" developed during World War II for the analysis of radar images. Radar operators had to decide whether a blip on the screen represented an enemy target, a friendly ship, or just noise. Signal Detection Theory measures the ability of radar receiver operators to make these important distinctions. Their ability to do so was called the Receiver Operating Characteristics. It was not until the 1970's that signal detection theory was recognized as useful for interpreting other test results. 

# Plot the ROC curves and AUC values
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "ROC_plots", ".pdf", sep=""), width=12, height=6)}
par(mfrow=c(1,2))
plot(eval_me_clim, "ROC", sub="Climate Only")
plot(eval_me, "ROC", sub="All Predictors")
if (pdf=="on"){dev.off()}

# SDM predicted distributions -------------------------------------------------------------

# Make 2 raster layers each with a "Relative Occurence Rate - ROR - (i.e. Relative Habitat Suitability)" (values from 0 to 1) for your species based on the Maxent model (one for the climate-only model and one for the all-predictors model).
# Input is the current environmental conditions:
current_r_clim <- predict(me_clim, cur_predictors_crop, args='outputformat=cloglog')
current_r <- predict(me, cur_predictors_crop, args='outputformat=cloglog') 
plot(current_r)

# The mean probability over the study extent the could be used as the threshold (above which species is considered "present" and below which species is considered "absent"). raster::reclassify = reclassify ROR raster using the calculated threshold value as the "zero" point.  A better way is to use the threshold from above at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest 
current_r_clim_stat <- cellStats(current_r_clim, stat="min", na.rm=TRUE)
current_r_clim_stat_m <- matrix(c(current_r_clim_stat, as.numeric(me_clim_thresh), 0), ncol=3, byrow=TRUE)
current_r_clim_prob <- reclassify(current_r_clim, rcl=current_r_clim_stat_m)
plot(current_r_clim)
plot(current_r_clim_prob)

current_r_stat <- cellStats(current_r, stat='min', na.rm=TRUE)
current_r_stat_m <- matrix(c(current_r_stat, as.numeric(me_thresh), 0), ncol=3, byrow=TRUE)
current_r_prob <- reclassify(current_r, current_r_stat_m)
plot(current_r)
plot(current_r_prob)

# As above, predict rasters for the future environmental conditions:
future_r_clim <- predict(me_clim, future_predictors_crop)
future_r <- predict(me, future_predictors_crop)

# As above, calculate a threshold and reclassify the raster: 
future_r_clim_stat <- cellStats(future_r_clim, stat='min', na.rm=TRUE)
future_r_clim_stat_m <- matrix(c(future_r_clim_stat, as.numeric(me_clim_thresh), 0), ncol=3, byrow=TRUE)
future_r_clim_prob <- reclassify(future_r_clim, future_r_clim_stat_m)
plot(future_r_clim)
plot(future_r_clim_prob)

future_r_stat <- cellStats(future_r, stat='min', na.rm=TRUE)
future_r_stat_m <- matrix(c(future_r_stat, as.numeric(me_thresh), 0), ncol=3, byrow=TRUE)
future_r_prob <- reclassify(future_r, future_r_stat_m)
plot(future_r)
plot(future_r_prob)

dev.off()
# Open a pdf document for plotting and set up for 4 plots on same page:
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "distributions_predicted", ".pdf", sep=""), width=10, height=10)}
par(mfrow=c(2,2))
par(oma=c(1,2,1,1))

# Plot the current modelled distrubution based on climate alone
plot(current_r_clim_prob, xlab="Lon", ylab="Lat", las=1, main="Current (climate only)")
points(occ, pch=16, cex=0.5)

#Plot the current modelled distrubution based on all environmental data
plot(current_r_prob, xlab="Lon", ylab="Lat", las=1, main="Current (all predictors)")
points(occ, pch=16, cex=0.5)

# Plot the future distribution based on climate alone
plot(future_r_clim_prob, xlab="Lon", ylab="Lat", las=1, main="Future (climate only)")
points(occ, pch=16, cex=0.5)

# Plot the future distribution based on all environmental data
plot(future_r_prob, xlab="Lon", ylab="Lat", las=1, main="Future (all predictors)")
points(occ, pch=16, cex=0.5)

if (pdf=="on"){dev.off()} # switch pdf off

# Save raster predictions to disk as GTiffs
writeRaster(current_r_clim_prob, "SDM_Maxent_cloud/Raster_out/Species_model_curr_clim", format="GTiff", overwrite=TRUE) 
writeRaster(current_r_prob, "SDM_Maxent_cloud/Raster_out/Species_model_curr", format="GTiff", overwrite=TRUE) 
writeRaster(future_r_clim_prob, "SDM_Maxent_cloud/Raster_out/Species_model_future_clim", format="GTiff", overwrite=TRUE) 
writeRaster(future_r_prob, "SDM_Maxent_cloud/Raster_out/Species_model_future", format="GTiff", overwrite=TRUE) 
writeRaster(future_r_prob, "SDM_Maxent_cloud/Raster_out/Species_model_future", format="GTiff", overwrite=TRUE) 

#To plot response curve of all variables used. 
#Response plots can be produced a number of ways, depending on the settings supplied to the function response (see help(response)). With at=mean, the responses are those with all variables set to their mean except the one being modified in the model. Note that range="p" restricts consideration to just the area in which the species does actually occur. range="pa" considered the whole area modelled. The same data is available in the html file that is written to disc in the folders Maxent_out and Maxent_out_clim. 
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "response_clim", ".pdf", sep=""), width=10, height=10)}
response(me_clim, at=mean, range="p")
if (pdf=="on"){dev.off()}

if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "response_all", ".pdf", sep=""), width=10, height=10)}
response(me, at=mean, range="p")
if (pdf=="on"){dev.off()}

#To plot predictor values for all variables used
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "predictor_clim", ".pdf", sep=""), width=10, height=10)}
plot(me_clim)
if (pdf=="on"){dev.off()}

if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "predictor_all", ".pdf", sep=""), width=10, height=10)}
plot(me)
if (pdf=="on"){dev.off()}

#To view response curve of just four variable that you choose, do somthing like this:
response(me, var=c("MAP","P.Seasonality", "Distance.Coast"), at=mean, range="p", rug=T)

#Create a density plots of presence and absence data
#A density plot. Presence data are in red, and absence data (if available) are in blue.
help(density)
dev.off()
density(me_clim)
density(me)

# Compare predicted distributions (current vs future) ---------------------------------

# Note: the aim of the following three sections is to derive some metrics to look at species distribution changes and associated environmental change. There could very well be some already-developed packages with functions that do these sorts of analyses automatically and in a different way to how they are done here. Naturally, some investigation into the matter might give different or better answers but the code here is just to get you thinking about "change". 

# It is not too easy to see the differences between the current and future, so we subtract current from future to see what will change. A positive value indicates an increase in the future, and negative value indicates a future decrease
change_clim <- (future_r_clim_prob - current_r_clim_prob)
change <- (future_r_prob - current_r_prob)
plot(change_clim)
plot(change)

# Open a pdf document for plotting and set up for 2 plots on same page:
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "distribution_change", ".pdf", sep=""), width=12, height=6)}
par(mfrow=c(1,2))
par(oma=c(1,1,1,2))
par(mar=c(6, 6, 5, 4))

# Plot change due to climate alone
brks <- round(seq(min(minValue(round(change_clim,2))),max(maxValue(round(change_clim,2))),length.out=7),2)
#Or set breaks manually
#brks <- round(seq(-0.6, 0.5,length.out=10),2)
plot(change_clim, xlab="Lon", ylab="Lat", las=1, col=rev( rainbow(7, start=0,end=1)), breaks=brks, main="Distribution change (climate only)")
points(occ, pch=16, cex=.2)

# Plot change to climate but with soils and another env variables (use breaks from above to plot on same scale)
plot(change, xlab="Lon", ylab="Lat", las=1, col=rev(rainbow(7, start=0,end=1)), breaks=brks, main="Distribution change (all predictors)")
points(occ, pch=16, cex=.2)

if (pdf=="on"){dev.off()}


# Save rasters of distribution change to disk as GTiffs
writeRaster(change_clim, "SDM_Maxent_cloud/Raster_out/Species_dist_change_clim", format="GTiff", overwrite=TRUE) 
writeRaster(change, "SDM_Maxent_cloud/Raster_out/Species_dist_change", format="GTiff", overwrite=TRUE) 



# Compare predicted distributions cont. (probabilities, residuals, mean % change) --------------------------------------------------------------------

# What proportion of sampled regions are no longer suitable for the species? Work out the ROR at each sampled site
cur_prob <- raster::extract(current_r, occ_sp)
cur_prob_clim <- raster::extract(current_r_clim, occ_sp)
future_prob <- raster::extract(future_r, occ_sp)
future_prob_clim <- raster::extract(future_r_clim, occ_sp)

# Plot comparison of the current probability of species occurrence against that in the future (climate only vs all predictors)
if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "current_vs_future_probability", ".pdf", sep=""), width=12, height=6)}
par(mfrow=c(2,2))
plot(cur_prob_clim, future_prob_clim, main="Current versus future (climate only)", xlab="Current (probability)", ylab="Future (probability)")
abline(0,1, col="blue")

plot(cur_prob, future_prob, main="Current versus future (all predictors)", xlab="Current (probability)", ylab="Future (probability)")
abline(0,1, col="blue")
if (pdf=="on"){dev.off()}

# Calculate and plot residuals to y=x. The difference between the future probability of occurrence and the current probability of occurrence (which in this case substitutes as the sample mean). Negative values = higher probability of occurence currently. Positive values = higher probability of occurence in the future.
summaries <- list()
cur_future_clim_resid <- future_prob_clim-cur_prob_clim
cur_future_resid <- future_prob-cur_prob

if (pdf=="on"){pdf(paste("SDM_Maxent_cloud/Figures/", "residuals", ".pdf", sep=""), width=10, height=10)}
par(mfrow=c(2,2))
plot(cur_prob_clim, cur_future_clim_resid, main="Current versus future (climate only)", xlab="Current (probability)", ylab="Future (residuals)")
abline(0,0, col="red")
summaries[[1]] <- summary(cur_future_clim_resid)

plot(cur_prob, cur_future_resid, main="Current versus future (all predictors)", xlab="Current (probability)", ylab="Future (residuals)")
abline(0,0, col="red")
summaries[[2]] <- summary(cur_future_resid)
if (pdf=="on"){dev.off()}


# Tabulate the stat summaries of the residuals, including the "mean percent change". Write this dataframe to disk
summaries_df <- data.frame(matrix(unlist(summaries), nrow=2, byrow=T))
names(summaries_df)[1:6] <- names(summaries[[1]])
summaries_df$mean_perc_change <- round(summaries_df$Mean*100,0)
rownames(summaries_df)<- c("cur_future_clim_resid", "cur_future_resid")
summaries_df

write.csv(summaries_df, "SDM_Maxent_cloud/Table_out/summaries_df.csv")


# Looking at environmental change again -------------------------------------------------

# Do a binary classification on the significance raster so that it can become a binary raster (present or absent)
# Create a matrix of reclassification data (must have 3 columns, first two columns are the "from" and "to" input values and the third column "becomes" has the new value for that range. In this case we say that anything between 0.X and 1 becomes "1". Zero stays "0")


cellStats(current_r_prob,mean)
#Either use a threshold of 0.5
m <- c(0.5, 1, 1, 0, 0.5, 0)
#Or set up the threshold using the threshold
m <- c(me_clim_thresh, 1, 1, 0, me_clim_thresh, 0)
clim_rclmat <- matrix(m, ncol=3, byrow=TRUE)

# Reclassify each ROR raster to a binary. Look up the corresponding environmental information, and take a mean of that info
current_r_clim_bin <- reclassify(current_r_clim_prob, clim_rclmat)
dev.off()
plot(current_r_clim_bin)
current_r_clim_p <- data.frame(rasterToPoints(current_r_clim_bin, fun=function(x){x>0}))
coordinates(current_r_clim_p) <- ~x+y
proj4string(current_r_clim_p) <- "+proj=longlat +datum=WGS84 +no_defs"
current_r_clim_enviro <- raster::extract(cur_predictors_crop_clim, current_r_clim_p)
current_r_clim_enviro_mean <- colMeans(current_r_clim_enviro, na.rm=T)

#Or set up the threshold using the threshold
m <- c(me_thresh, 1, 1, 0, me_thresh, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

current_r_bin <- reclassify(current_r_prob, rclmat)
dev.off()
plot(current_r_bin)
current_r_p <- data.frame(rasterToPoints(current_r_bin, fun=function(x){x>0}))
coordinates(current_r_p) <- ~x+y
proj4string(current_r_p) <- "+proj=longlat +datum=WGS84 +no_defs"
current_r_enviro <- raster::extract(cur_predictors_crop, current_r_p)
current_r_enviro_mean <- colMeans(current_r_enviro, na.rm=T)
writeRaster(current_r_bin, "SDM_Maxent_cloud/Raster_out/Species_model_curr_bin", format="GTiff", overwrite=TRUE) 

future_r_clim_bin <- reclassify(future_r_clim_prob, clim_rclmat)
dev.off()
plot(future_r_clim_bin)
future_r_clim_p <- data.frame(rasterToPoints(future_r_clim_bin, fun=function(x){x>0}))
coordinates(future_r_clim_p) <- ~x+y
proj4string(future_r_clim_p) <- "+proj=longlat +datum=WGS84 +no_defs"
future_r_clim_enviro <- raster::extract(future_predictors_crop_clim, future_r_clim_p)
future_r_clim_enviro_mean <- colMeans(future_r_clim_enviro, na.rm=T)

future_r_bin <- reclassify(future_r_prob, rclmat)
dev.off()
plot(future_r_bin)
future_r_p <- data.frame(rasterToPoints(future_r_bin, fun=function(x){x>0}))
coordinates(future_r_p) <- ~x+y
proj4string(future_r_p) <- "+proj=longlat +datum=WGS84 +no_defs"
future_r_enviro <- raster::extract(future_predictors_crop, future_r_p)
future_r_enviro_mean <- colMeans(future_r_enviro, na.rm=T)

# This is the average environment at the places where the species occurs currently and in the future
(mean_enviro_occ <-data.frame(bind_rows(current_r_clim_enviro_mean, current_r_enviro_mean, future_r_clim_enviro_mean, future_r_enviro_mean)))
rownames(mean_enviro_occ) <- c("current_clim", "current", "future_clim", "future")
write.csv(mean_enviro_occ, "SDM_Maxent_cloud/Table_out/mean_enviro_occ.csv")

# This is the regional average and shows what is expected to change
(mean_enviro <- rbind(cellStats(cur_predictors_crop, "mean"), cellStats(future_predictors_crop, "mean")))
rownames(mean_enviro) <- c("current", "future")
write.csv(mean_enviro, "SDM_Maxent_cloud/Table_out/mean_enviro.csv")


