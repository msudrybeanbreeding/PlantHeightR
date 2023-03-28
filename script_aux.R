##Supplementary File
## Plant height method 1 - Using Digital Surface Model (DSM)
###################Libraries
library(raster)
library(FIELDimageR)
library(sf)
library(doParallel)
library(parallel)
library(foreach)
library(rasterVis)

rm(list=ls())

dir_dsm <- "C:/PH2_R/DSM"
dir_dtm <- "C:/PH2_R/DTM"

imgFiles.dsm <-list.files(path = dir_dsm, pattern="*.tif$",full.names = T) #get the DSM 
imgFiles.dtm <-list.files(path = dir_dtm, pattern="*.tif$",full.names = T) #get the DSM 

imgFiles.dsm.names <-list.files(path = dir_dsm, pattern="*.tif$") # Aux file names
imgFiles.dsm.names <- gsub(".tif", "", imgFiles.dsm.names, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE) # Removing the extension
imgFiles.dsm.names

#DSM1= "C:\\PH2_R\\DSM\\7_14_21_SVREC_RGB_P1_dsm.tif"
#DSM0= "C:\\PH2_R\\DSM\\6_8_21_SVREC_RGB_DTM_dsm.tif"

#folder_shp="C:\\PH2_R\\PlantHeightR\\Shapefile\\2022_SVREC_Shp_Plots_final01.shp"

indPlots <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\Shapefile_SVREV_P01_03_test.shp")

colorPal <- terrain.colors(255)
DSM1 <- stack(imgFiles.dsm[1])
p <- sample(1:nrow(indPlots), 1) # randomly select a value for p
print(p)
DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
DSM2.c <-  crop(DSM1, st_bbox(indPlots[p,]))
levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "PlotID")

##########
EPH.Extract<-extract(x = DSM1.c, y = indPlots[p,])
EPH_soil <-  lapply(EPH.Extract, quantile, probs = 0.1, na.rm=TRUE)
DSM1.c[DSM1.c <= EPH_soil[[1]][[1]]] = NA 
DSM2.c[DSM2.c > EPH_soil[[1]][[1]]] = NA 

levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "PlotID")
levelplot(DSM2.c[[1]], col.regions = colorPal, margin = FALSE, main = "PlotID")

csm_rem <- quantile(DSM1.c,(0.99))
DSM1.c <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
EPH1 <- cellStats(DSM1.c,mean) #get the mean of the top quantile

csm_rem2 <- quantile(DSM2.c,(1-0.99))
DSM2.c <- reclassify(DSM2.c, cbind(csm_rem2, Inf, NA)) # Remove values higher than the quantile threshold and set them to NA.
EPH2 <- cellStats(DSM2.c,mean) #get the mean of the top quantile
# Compute difference and round to four decimal places
EPH <- round(EPH1 - EPH2, 4)

EPH1 <- cellStats(DSM1.c,mean) 
EPH2 <- cellStats(DSM2.c,mean) 
# Compute difference and round to four decimal places
EPH <- round(EPH1 - EPH2, 4)

####

# Subsetting plots to speed up the loop
# indPlots<- indPlots[1:5, ]
# indPlots[1:5, 1:5 ]

gc() #Cleaning unusual memmory
#>           used  (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 4579540 244.6    8014052 428.0  8014052 428.0
#> Vcells 6658941  50.9   23408984 178.6 45720669 348.9

# Number of cores
n.core<-detectCores() # or detectCores()

## Data frame with plot names
names(indPlots)
Plot_ID<- as.data.frame(indPlots[["Global_ID_"]])

ExtMet = "quantile"
qthresh = as.numeric(0.01) 

dataset_img = "time-series data flights"
#dataset_img = "ind. data flight"

PlantHeigh_est<- function(imgFiles.dtm, dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh) {
  
  if (dataset_img == "ind. data flight") { 
    DSM0 <- stack(imgFiles.dtm)
    DSM1 <- stack(dataset_img_sel) #ind. DSM selected by the user
    
    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
  results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                    .packages = c("raster", "sf"), 
                    .combine = rbind) %dopar% {
                      
                      #p=1
                      DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                      DSM0.c <-  crop(DSM0, st_bbox(indPlots[p,]))
                      
                      # Canopy Surface Model (CSM):
                      DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                      csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                      #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                      #message("assume no negative values. Assign NA")
                      csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                      
                      EPH.Extract<-extract(x = csm, y = indPlots[p,])
                      
                      #Removing soil based on the lower 10% pixels values
                      EPH.10<-lapply(EPH.Extract, quantile, probs = 0.1, na.rm=TRUE)
                      csm[csm <=EPH.10[[1]][[1]]] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                      
                      # Extracting the estimate plant height average (EPH):
                      if (ExtMet == "mean") {
                        EPH <- cellStats(csm,mean) 
                        
                      } else if (ExtMet == "median"){
                        EPH <- cellStats(csm,median) 
                        
                      } else if (ExtMet == "quantile"){
                        csm_rem <- quantile(csm,(1-qthresh))
                        csm[csm < csm_rem] <- NA #Remove values lower than the 75% quantile threshold. set them to NA.
                        EPH <- cellStats(csm,mean) #get the mean of the top quantile
                        
                      }
                      
                    }
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  # Add "EPH" as a column name
  colnames(results) <- "EPH"
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID = Plot_ID$indPlots, results_df)
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  
  } else if (dataset_img == "time-series data flights") {
    
    for(i in 1:length(imgFiles.dsm)){ #loop through images
      
      message("Processing ortho: ",paste(imgFiles.dsm[i]))
      
      DSM0 <- stack(imgFiles.dtm)
      DSM1 <- stack(imgFiles.dsm[i])
      
      # Starting parallel
      cl <- makeCluster(n.core, output = "")
      registerDoParallel(cl)
      getDoParWorkers()
    
      results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                        .packages = c("raster", "sf"), 
                        .combine = rbind) %dopar% {
                          p=1
                          DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                          DSM0.c <-  crop(DSM0, st_bbox(indPlots[p,]))
                          
                          # Canopy Surface Model (CSM):
                          DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                          csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                          #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                          #message("assume no negative values. Assign NA")
                          csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                          
                          EPH.Extract<-extract(x = csm, y = indPlots[p,])
                          
                          #Removing soil based on the lower 10% pixels values
                          EPH.10<-lapply(EPH.Extract, quantile, probs = 0.1, na.rm=TRUE)
                          csm[csm <=EPH.10[[1]][[1]]] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                          
                          # Extracting the estimate plant height average (EPH):
                          if (ExtMet == "mean") {
                            EPH <- cellStats(csm,mean) 
                            
                          } else if (ExtMet == "median"){
                            EPH <- cellStats(csm,median) 
                            
                          } else if (ExtMet == "quantile"){
                            csm_rem <- quantile(csm,(1-qthresh))
                            csm[csm < csm_rem] <- NA #Remove values lower than the 75% quantile threshold. set them to NA.
                            EPH <- cellStats(csm,mean) #get the mean of the top quantile
                            
                          }
                          
                        }
      
      parallel::stopCluster(cl) # Stopping the parallel function
      
      # Add "EPH" as a column name
      colnames(results) <- "EPH"
      
      # Convert results to a data frame
      results_df <- as.data.frame(results)
      
      # Add row names from Plot_ID as a new column to results data frame
      results_df <- cbind(Plot_ID = Plot_ID$indPlots, results_df)
      
      results_df$DSM_data<-imgFiles.dsm.names[i]
      
      if(i==1){results_df.1<-results_df}else{results_df.1<-rbind(results_df.1, results_df)}
      
      }
  }
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  results_df.1   
  
  return(results_df.1)
  
}

restuls.2<- PlantHeigh_est(imgFiles.dtm = imgFiles.dtm,
                           dataset_img_sel = dataset_img_sel,
                           n.core=n.core,
                           indPlots=indPlots,
                           Plot_ID = Plot_ID,
                           imgFiles.dsm.names = imgFiles.dsm.names,
                           ExtMet =ExtMet,
                           qthresh = qthresh)






# Set color palette
colorPal <- terrain.colors(255)

# Plot raster
levelplot(DSM1[[1]], col.regions = colorPal, margin = FALSE, main = "Field DSM")
levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "PlotID")
levelplot(csm[[1]], col.regions = colorPal, margin = FALSE, main = "PlotID")


############################
dsm_list <- "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\HURON\\DSM\\7_20_22_HURON_RGB_dsm.tif"
dsm_list <- stack(dsm_list)
shape<- "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\HURON_PH\\b._Shapefiles\\22_HUR_SHP_area.shp"
shape<- st_read(shape)
n.core<-detectCores() 

sf::st_crs(shape) == sf::st_crs(dsm_list)

st_crs(shape)$input
st_crs(dsm_list)$input

st_crs(shape)$epsg
st_crs(dsm_list)$epsg

epsg_value<- sf::st_crs(shape)$epsg
raster::projection(DSM) <- epsg_value

projection_value<- raster::projection(shape)
raster::projection(DSM) <- projection_value

projection(shape)
projection(DSM)

# Auxiliary function for clipping DSM by shapefile (features)
clipper.area.dsm <- function(dsm_list, shape, n.core) {
  
  if (length(dsm_list) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }
  
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  tryCatch(
    foreach(i = 1:length(dsm_list), 
            .packages = c("raster", "sf"),
            .combine='c') %dopar% {
              
              # message("Processing DSM - Clipping: ", paste(dsm_list[[i]]))
              
              # loading the DSM files
              i=1
              DSM <- stack(dsm_list[[i]])
              
              tryCatch({
                if(is.na(sf::st_crs(DSM))) {
                  sf::st_crs(shape) <- NA
                  message("Shapefiles have been adjusted to NA CRS.")
                  return(shape)
                  
                }
                if(!is.na(sf::st_crs(shape)$epsg)) {
                  epsg_value<- sf::st_crs(shape)$epsg
                  raster::projection(DSM) <- epsg_value
                  message("CRS DSM have been adjusted using shapefile CRS. ")
                  return(DSM)
                } 
                
                if(!is.na(raster::projection(shape))) {
                  projection_value<- raster::projection(shape)
                  raster::projection(DSM) <- projection_value
                  message("CRS projection DSM have been adjusted using shapefile CRS projection. ")
                  return(DSM)
                } 
                
                if(!isTRUE(sf::st_crs(shape) == sf::st_crs(DSM))) {
                  
                  stop("The CRS for DSM must be the same as the CRS for shapefiles")
                  
                }
                
              },
              error = function(e) {
                #showNotification("The CRS for data must be the same as the CRS for shapefiles", type = "error", duration = 10)
                message(paste("The CRS for data must be the same as the CRS for shapefiles.", e))
              })
              
              # clip the point cloud by the polygon shapefile
              DSM.c <-  crop(DSM, st_bbox(shape))
              
              # save the clipped point cloud to a new DSM file
              DSM.c_names <- sapply(dsm_list[i], function(x) gsub(".tif", "", x))
              writeRaster(DSM.c, paste0(DSM.c_names,"_clip.dsm"), format = "GTiff")
              
            },
    error = function(e) {
      showNotification(paste("Error in clipper.area.dsm:", e), type = "error")
    }
  )
  
  stopCluster(cl)
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message")
}




#############

###############################
library(rlas)
library(lidR)
library(sf)


cloud <- "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\HURON\\point_cloud\\7_20_22_HURON_RGB_group1_densified_point_cloud.laz"
cloud_veg <- readLAS(files = cloud, select = "xyz")

shape<- "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\HURON_PH\\b._Shapefiles\\22_HUR_SHP_area.shp"
shape<- st_read(shape)

if(is.na(lidR::st_crs(cloud_veg))) {
  lidR::st_crs(shape) <- NA
  message("Shapefiles have been adjusted to NA CRS.")
  cloud_veg
} else if(!is.na(lidR::st_crs(shape)$epsg)) {
  epsg_value<- lidR::st_crs(shape)$epsg
  lidR::projection(cloud_veg) <- epsg_value
  message("CRS Point Cloud have been adjusted using shapefile CRS. ")
  cloud_veg
} else if(!is.na(lidR::projection(shape))) {
  projection_value<- lidR::projection(shape)
  lidR::projection(cloud_veg) <- projection_value
  message("CRS projection Point Cloud have been adjusted using shapefile CRS projection. ")
  cloud_veg
} else if(!isTRUE(lidR::st_crs(shape) == lidR::st_crs(cloud_veg))) {
  stop("The CRS for Point Cloud must be the same as the CRS for shapefiles")
}


p <- shape[k,]
p_sp <- as(p, "Spatial")
c_veg <- clip_rectangle(cloud_veg, xleft = p_sp@bbox['x','min'], 
                        ytop = p_sp@bbox['y','max'], 
                        xright = p_sp@bbox['x','max'], 
                        ybottom = p_sp@bbox['y','min'])
















