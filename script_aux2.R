
###############################
# plant Height via cloud points
###############################
library(data.table)
library(rlas)
library(lidR)
library(foreach)
library(doParallel)
library(sf)
#####################

rm(list=ls())
## Data setup
setwd("C:/PH2_R")

# # loading the LAS files
# las0 <- readLAS(files = "C:\\PH2_R\\PC\\6_8_21_SVREC_RGB_DTM_group1_densified_point_cloud.laz", select = "xyz")
# las60 <- readLAS(files = "C:\\PH2_R\\PC\\7_14_21_SVREC_RGB_P1_group1_densified_point_cloud.laz", select = "xyz")
# 
# # importing shapefiles (area)
# plots_area <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_area.shp")
# print(plots_area)
# plot(plots_area, 
#      #add = T, 
#      col = "Gray")
# 
# # clip the point cloud by the polygon shapefile
# clipped_las0 <- clip_roi(las0, plots_area)
# clipped_las60 <- clip_roi(las60, plots_area)
# 
# # save the clipped point cloud to a new LAS file
# writeLAS(clipped_las0, "C:/PH2_R/PC/Clip/6_8_21_SVREC_RGB_DTM.laz")
# writeLAS(clipped_las60, "C:/PH2_R/PC/Clip/7_14_21_SVREC_RGB_P1.laz")


# loading the LAS files
#las0 <- readLAS(files = "C:/PH2_R/PC_soil/6_8_21_SVREC_RGB_DTM.laz", select = "xyz")
#las60 <- readLAS(files = "C:/PH2_R/PC_veg/7_14_21_SVREC_RGB_P1.laz", select = "xyz")

#plot(las60)
#plot(las0)

# importing shapefiles (plots)
indPlots <- st_read("E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2020\\SVREC_PH\\b._Shapefiles\\SVREC_2020_shapefile_area.shp")
print(indPlots)
# plot(indPlots, 
#      #add = T, 
#      col = "Red")

## Auxiliary function for clipping point cloud by shapefile (features)
# clipper.cloud <- function(cloud, shape, id_col) {
#   pc <- list()
#   for(i in 1:nrow(shape)) {
#     p <- shape[i,]
#     p_sp <- as(p, "Spatial")
#     c <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
#                         ytop = p_sp@bbox['y','max'], 
#                         xright = p_sp@bbox['x','max'], 
#                         ybottom = p_sp@bbox['y','min'])
#     if(!is.null(c)) {
#       pc[[i]] <- c
#       names(pc)[i] <- paste0(shape[[id_col]][i])
#     }
#   }
#   pc <- pc[!unlist(lapply(pc, is.null))]
#   return(pc)
# }

# Set up parallel processing
gc()
n.core <- detectCores()


###########
shape = indPlots
id_col = "Global_ID_"
qthresh= 0.99
qthreshSoil = 0.5
ExtMet = "mean"
Plot_ID = indPlots$Global_ID_
cloud_names = list("C:/PH2_R/PC_veg/7_14_21_SVREC_RGB_P1.laz", "C:/PH2_R/PC_veg/7_18_21_SVREC_RGB_P1.laz")
##########

cloud_soil <- "C:/PH2_R/PC_soil/6_8_21_SVREC_RGB_DTM.laz"

#cloud <- "C:/PH2_R/PC_veg/7_14_21_SVREC_RGB_P1.laz"

cloud_list <- list("C:/PH2_R/PC_veg/7_14_21_SVREC_RGB_P1.laz", "C:/PH2_R/PC_veg/7_18_21_SVREC_RGB_P1.laz")

## Auxiliary function for clipping point cloud by shapefile (features)
#clipper.cloud.est.ind <- function(cloud, cloud_s, shape, id_col, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names) {
clipper.cloud.par <- function(cloud_list, cloud_s, shape, id_col, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names) {

  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  # loading the LAS files
  cloud_s <- readLAS(files = cloud_soil, select = "xyz")
  
  for(i in 1:length(cloud_list)){ #loop through images
i=1
    message("Processing Point Cloud - Vegetation: ",paste(cloud_list[[i]]))
    message("Processing Point Cloud - Soil: ", paste(cloud_soil))
    
    # loading the LAS files
  cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  setProgress(message = "Running EPH analysis", detail = "")
  
  results <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                .packages = c("lidR", "sf"), 
                .combine = rbind) %dopar% {
k=1
                  p <- shape[k,]
                  p_sp <- as(p, "Spatial")
                  c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                      ytop = p_sp@bbox['y','max'], 
                                      xright = p_sp@bbox['x','max'], 
                                      ybottom = p_sp@bbox['y','min'])
                  
                  # Create the plot
                  plot(c_veg)
                  
                  # Add labels and adjust the plot view
                  title3d(xlab = "X", ylab = "Y", zlab = "Z")
                  bg3d("white")
                  par3d(windowRect = c(0, 0, 800, 800))
                  
                  # Disable interactive 3D window and display the plot in the RStudio plot pane
                  options(rgl.useNULL = TRUE)
                  
                  # Remove outliers in c_veg
                  mean_veg <- mean(c_veg@data$Z)
                  std_veg <- sd(c_veg@data$Z)
                  kout = 3 
                  c_veg2 <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
                  plot(c_veg2)
                  
                  # Remove outliers in c_soil
                  mean_soil <- mean(c_soil@data$Z)
                  std_soil <- sd(c_soil@data$Z)
                  c_soil <- c_soil[c_soil@data$Z >= mean_soil - kout * std_soil & c_soil@data$Z <= mean_soil + kout * std_soil,]
                  
                  # Filter out noise points
                  las <- classify_noise(c_veg, sor(k = 0.00001, m = 3))
                  plot(las)
                  las2 <- classify_noise(c_veg, ivf(5,2))
                  plot(las2)
                  
                  EPH.Extract.veg<- c_veg@data$Z
                  EPH.Extract.soil<- c_veg@data$Z
                  
                  #qthreshsingle= 0.1
                  EPH_soil <-  quantile(EPH.Extract.soil, probs = qthreshsingle, na.rm=TRUE)
                  
                  EPH.Extract.veg[EPH.Extract.veg <= EPH_soil[[1]][[1]]] = NA 
                  EPH.Extract.soil[EPH.Extract.soil > EPH_soil[[1]][[1]]] = NA
                  
                  # Extracting the estimate plant height average (EPH):
                  if (ExtMet == "mean") {
                    EPH1 <- mean(EPH.Extract.veg, na.rm=TRUE)
                    EPH2 <- mean(EPH.Extract.soil, na.rm=TRUE)
                    
                    # Compute difference and round to two decimal places
                    EPH <- round(EPH1 - EPH2, 4)
                    
                  } else if (ExtMet == "median") {
                    EPH1 <- median(EPH.Extract.veg, na.rm=TRUE)
                    EPH2 <- median(EPH.Extract.soil, na.rm=TRUE)
                    
                    # Compute difference and round to two decimal places
                    EPH <- round(EPH1 - EPH2, 4)
                    
                  } else if (ExtMet == "quantile"){
                    
                    #qthresh=0.9
                    
                    EPH_rem1 <-  quantile(EPH.Extract.veg, probs = qthresh, na.rm=TRUE)
                    EPH.Extract.veg[EPH.Extract.veg <= EPH_rem1[[1]][[1]]] = NA #Remove values lower than the quantile threshold. set them to NA.
                    EPH1 <- mean(EPH.Extract.veg, na.rm=TRUE) #get the mean of the top quantile
                    
                    EPH_rem2 <- quantile(EPH.Extract.soil, probs = (1-qthresh), na.rm=TRUE)
                    EPH.Extract.soil[EPH.Extract.soil > EPH_rem2[[1]][[1]]] = NA #Remove values lower than the quantile threshold. set them to NA.
                    EPH2 <- mean(EPH.Extract.soil, na.rm=TRUE) #get the mean of the top quantile
                    # Compute difference and round to four decimal places
                    EPH <- round(EPH1 - EPH2, 4)

                  }
                }
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  setProgress(message = "EPH Analysis Complete", detail = paste0("Processing plot - running loop not available when use parallel cores"))
  
  # Add "EPH" as a column name
  colnames(results) <- "EPH"
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  #results_df$PC_data<-cloud_names[i]
  results_df$PointCloud_data<-sapply(cloud_names[i], function(x) gsub(".laz", "", basename(x)))
  
  if(i==1){results_df.1<-results_df}else{results_df.1<-rbind(results_df.1, results_df)}
  
  j= j+1
  setProgress(message = "Running EPH analysis", detail = paste0("Processing file ", i), value = as.numeric(j)/length(dataset_img_sel))

  }
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  return(results_df.1)
                  
  
}
  

# visualising shapes
#plot(c_veg@data$Y, c_veg@data$X) # nadir seeing 
plot(c_veg@data$Y, c_veg@data$Z) #vista lateral
plot(c_veg@data$X, c_veg@data$Z) #vista frontal
abline(h = EPH1, col = "Blue")

#plot(c_soil@data$Y, c_soil@data$X) # nadir seeing 
plot(c_soil@data$Y, c_soil@data$Z) #vista lateral
plot(c_soil@data$X, c_soil@data$Z) #vista frontal
abline(h = EPH2, col = "Blue")

# Compute the average value of Z across Y
avg_Zy <- tapply(c_veg@data$Z, c_veg@data$Y, mean)
plot(avg_Zy, type="l", xlab="EPH", ylab="Lateral view")
abline(h = mean(c_veg@data$Z), col = "red")


# Compute the average value of Z across Y
avg_Zx <- tapply(c_veg@data$Z, c_veg@data$X, mean)
plot(avg_Zx, type="l", xlab="EPH", ylab="Frontal view")
abline(h = mean(c_veg@data$Z), col = "blue")

library(plotly)
library(viridis)
df <- data.frame(x = c_veg@data$Y,
                 y = c_veg@data$X,
                 z = c_veg@data$Z)
plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers")
 plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers",
             marker = list(size = 2, color = "blue")) %>%
  layout(scene = list(xaxis = list(title = "Y", showticklabels = FALSE),
                      yaxis = list(title = "X", showticklabels = FALSE),
                      zaxis = list(title = "Z", showticklabels = FALSE)))



# Run the function to clip the shapefile plots
plot.cloud0 <- clipper.cloud.par(las0, indPlots, "Global_ID_")
plot.cloud60 <- clipper.cloud.par(las60, indPlots, "Global_ID_")

#applying percentile to canopy height
p90.60 <- lapply(plot.cloud60, function(x) { quantile(x@data$Z, .90) }) #90th percentile
#applying percentile to soil height
p50.0 <- lapply(plot.cloud0, function(x) { quantile(x@data$Z, .50) }) #50th percentile

#######
p90.60$`21021025`

# visualising shapes
plot(plot.cloud60$`21021025`@data$Y, plot.cloud60$`21021025`@data$X) # nadir seeing 
plot(plot.cloud60$`21021025`@data$Y, plot.cloud60$`21021025`@data$Z) #vista lateral
plot(plot.cloud60$`21021025`@data$X, plot.cloud60$`21021025`@data$Z) #vista frontal
abline(h = p90.60$`21021025`, col = "Blue")


p50.0$`21021025`
plot(plot.cloud0$`21021025`@data$Y, plot.cloud0$`21021025`@data$Z) #vista lateral
abline(h = p50.0$`21021025`, col = "Red")

####

#estimating plant height for all plots
EPH <- data.frame()

for (i in 1:length(p90.60)) {
  cat("plot", i, " ")
  EPH <- rbind(EPH, data.frame(
    PlotID = names(p90.60)[[i]],        
    Height = round(as.numeric(p90.60[[i]]) - as.numeric(p50.0[[i]]), 2)
  ))  
}

head(EPH)
tail(EPH)

##### The End #######

#install.packages("plotly")
library(plotly)

df <- data.frame(x = plot.cloud60$'21021025'@data$Y,
                 y = plot.cloud60$'21021025'@data$X,
                 z = plot.cloud60$'21021025'@data$Z)


plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers")

#########

dataset_img_sel<- list("C:\\PH2_R\\PC_veg/7_14_21_SVREC_RGB_P1.laz" ,"C:\\PH2_R\\PC_veg/7_18_21_SVREC_RGB_P1.laz")

## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ts <- function(dataset_img_sel, shape, id_col, n.core) {
  
  pc_list <- list() # create a list to store pc objects
  
  for(i in 1:length(dataset_img_sel)){ #loop through images
    
    # loading the LAS files
    pc_data <- readLAS(files = dataset_img_sel[[i]], select = "xyz")
    
    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
    pc_local <- list()
    
    pc <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                  .packages = c("lidR", "sf"), 
                  .combine = rbind) %dopar% {
                    
                    p <- shape[k,]
                    p_sp <- as(p, "Spatial")
                    c <- clip_rectangle(pc_data, xleft = p_sp@bbox['x','min'], 
                                        ytop = p_sp@bbox['y','max'], 
                                        xright = p_sp@bbox['x','max'], 
                                        ybottom = p_sp@bbox['y','min'])
                    if(!is.null(c)) {
                      pc_local[[1]] <- c
                    }
                    
                    pc_local
                  }
    
    names(pc) <- paste0(shape[[id_col]])
    
    pc <- pc[!unlist(lapply(pc, is.null))]
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    pc_list <- append(pc_list, pc) # add pc object to pc_list
    
  }
  
  return(pc_list)
  
}



# Run the function to clip the shapefile plots
plot.cloud_ts <- clipper.cloud.est.ts(dataset_img_sel, indPlots, "Global_ID_", 12)


class(plot.cloud_ts)
class(plot.cloud60)

plot.cloud_ts[9]

plot.cloud_ts[3]

plot.cloud60[3]

point_cloud_veg_perc <- lapply(plot.cloud_ts, function(x) { quantile(x@data$Z, 0.9) })

#applying percentile to soil height
point_cloud_soil_perc2 <- lapply(plot.cloud_ts, function(x) { quantile(x@data$Z, 0.5) }) 

#estimating plant height for all plots
EPH <- data.frame()

for (i in 1:length(point_cloud_veg_perc)) {
  cat("plot", i, " ")
  EPH <- rbind(EPH, data.frame(
    PlotID = names(point_cloud_veg_perc)[[i]],        
    EPH = round(as.numeric(point_cloud_veg_perc[[i]]) - as.numeric(point_cloud_soil_perc2[[i]]), 2)
  ))  

}


##########

library(rlas)
library(lidR)
library(sf)
library(dplyr)
library(sp)
library(foreach)
library(doParallel)


cloud<- "E:/OneDrive/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2020/SVREC_PH/8_14_20_Pix4D_group1_densified_point_cloud.laz"
cloud <- readLAS(files = cloud, select = "xyz")


shape <- st_read("E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2020\\SVREC_PH\\b._Shapefiles\\SVREC_2020_shapefile_area.shp")
shape_crs <- st_crs(shape)
shape_crs$input
st_crs(shape) <- NA

st_crs(cloud) 

shape <- st_read("E:/OneDrive/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2021/SVREC/Shapefile/2021_SVREC_P01/Shapefile_SVREV_P01_02.shp")
shape_crs2 <- st_crs(shape2)
shape_crs2$input
epsg_value2<- st_crs(shape_crs2)$epsg
projection(cloud) <- epsg_value2

########
if(!is.na(st_crs(shape)$epsg)) {
  epsg_value<- st_crs(shape)$epsg
  projection(cloud) <- epsg_value
}


############
rm(list=ls())
library(sf)
library(raster)

shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_area.shp")
shape2 <- st_read("C:\\Users\\leoag\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\SVREC_PH\\b._Shapefiles\\2022_SVREC_Shp_area.shp")

imgFiles.dsm <- "C:\\PH2_R\\DSM\\7_14_21_SVREC_RGB_P1_dsm.tif"
imgFiles.dtm <- "C:\\PH2_R\\DTM\\6_8_21_SVREC_RGB_DTM_dsm.tif"
DSM1 <- stack(imgFiles.dsm)
DSM0 <- stack(imgFiles.dtm)

# imgFiles.dsm2 <- "E:/OneDrive/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2020/SVREC_PH/c._DSM/8_14_20_Pix4D_dsm_clip.tif"
# DSM2 <- stack(imgFiles.dsm2)
# 
# st_crs(DSM2)$epsg

st_crs(DSM1)$epsg


st_crs(shape)$epsg

st_crs(shape) <- NA
st_crs(DSM1) <- NA

# Define the new CRS
new_crs <- "+init=epsg:4326"

# Set the CRS of the RasterStack to the new CRS
crs(DSM1) <- new_crs

# tryCatch(
#   if(is.na(st_crs(DSM1))) {
#     st_crs(shape) <- NA
#     
#   } else if(!is.na(st_crs(shape)$epsg)) {
#     epsg_value<- st_crs(shape)$epsg
#     projection(DSM1) <- epsg_value
#     
#   } else if(!isTRUE(st_crs(shape) == st_crs(DSM1))) {
#     
#     stop("The CRS for point clouds must be the same as the CRS for shapefiles")
#     
#   } else {
#     stop("The CRS for point clouds must be the same as the CRS for shapefiles")
#   },
#   error = function(e) {
#     #showNotification(paste("Error in PlantHeigh_est_ind:", e), type = "error")
#     #showNotification("The CRS for point clouds must be the same as the CRS for shapefiles", type = "warning", duration = 20)
#     message(paste("Error in PlantHeigh_est_ind:", e))
#   })
# 

shp_test_crs<- function(data, shape){
  tryCatch({
    if(is.na(sf::st_crs(data))) {
      sf::st_crs(shape) <- NA
      message("Shapefile set to NA CRS")

    }
    if(!is.na(sf::st_crs(shape)$epsg)) {
      epsg_value<- sf::st_crs(shape)$epsg
      raster::projection(data) <- epsg_value
      message("CRS Data set to Shapefile CRS")
      
    } 
    
    if(!isTRUE(sf::st_crs(shape) == sf::st_crs(data))) {
      
      stop("The CRS for data must be the same as the CRS for shapefiles")
      
    }
    
    # Return the DSM and shapefile
    return(list(data = data, shape = shape))
    
    },
    error = function(e) {
      showNotification(paste("Error in shp_test_crs:", e), type = "error")
      showNotification("The CRS for data must be the same as the CRS for shapefiles", type = "warning", duration = 10)
      message(paste("Error in shp_test_crs:", e))
    })
}

st_crs(DSM1)

result <- shp_test_crs(DSM1, shape)
DSM1 <- result$data
shape <- result$shape

if (identical(DSM1, DSM1)) {
  stop("DSM and DTM are identical files")
  message("DSM and DTM are identical files")
  return(NULL)
}


#######

rm(list=ls())
library(sf)
library(raster)

shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_area.shp")
shape2 <- st_read("C:\\Users\\leoag\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\SVREC_PH\\b._Shapefiles\\2022_SVREC_Shp_area.shp")

imgFiles.dsm <- "C:\\PH2_R\\DSM\\7_14_21_SVREC_RGB_P1_dsm (2).tif"
DSM1 <- stack(imgFiles.dsm)
# Convert the shapefile to SpatialPolygonsDataFrame
#shape_sp <- sf::as(shape, "Spatial")

# Check if the DSM1 will overlay or cover the shapefile

overlay_data <-  function(data, shape){
  tryCatch({
terra::intersect(extent(data), shape)
if (is.null(overlay)) {
  stop("The data does not intersect the shapefile.")
} else {
  message("Shapefile overlay the data image -- OK")
}
  },
error = function(e) {
  showNotification(paste("Error in overlay_data:", e), type = "error")
  showNotification("The data does not intersect the shapefile.", type = "warning", duration = 10)
  message(paste("Error in overlay_data:", e))
})
}

overlay_data(DSM1, shape)





shape_sp <- sf::as_Spatial(shape)

  # Plot raster and shapefile together
  colorPal <- terrain.colors(255)
  rasterVis::levelplot(DSM1, col.regions = colorPal, margin = FALSE, main = "Selected plot view") + 
    latticeExtra::layer(sp.polygons(shape_sp, col = "red"))
  
  # Plot the raster and shapefile together using spplot
  spplot(DSM1, col.regions = colorPal, sp.layout = list(list(polygons = shape_sp, col = "red")), 
         main = "Selected plot view")
  
  
  ##########
  rm(list=ls())
  library(rlas)
  library(lidR)
  library(sf)
  library(sp)
  library(sf)


  
  cloud<- "C:/PH2_R/PC_orig/original/6_8_21_SVREC_RGB_DTM_group1_densified_point_cloud.laz"
  cloud <- readLAS(files = cloud, select = "xyz")

  # Select points randomly to reach an homogeneous density of 1
  cloud_plot <- decimate_points(cloud, homogenize(1,5))
  #plot(cloud_plot)
  

  shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_area.shp")
  
  # Remove outliers in c_soil
  mean_soil <- mean(cloud_plot@data$Z)
  std_soil <- sd(cloud_plot@data$Z)
  kout = 3 
  cloud_plot <- cloud_plot[cloud_plot@data$Z >= mean_soil - kout * std_soil & cloud_plot@data$Z <= mean_soil + kout * std_soil,]
  
  # Display p1 and p2 in the same plot region
  plot(cloud_plot@data$X, cloud_plot@data$Y, main = "Nadir view - Point cloud distribution") 
  
  # Plot shapefile and add to plot
  plot(shape, add = TRUE, col = "red")
  
  # Plot shapefile with no fill and red contour
  plot(shape, col = "red", fill = "transparent")
  
  
  ##########
  rm(list=ls())
  library(rlas)
  library(lidR)
  library(sf)
  library(sp)
  library(sf)
  
  
  
  cloud<- "C:/PH2_R/PC_orig/original/6_8_21_SVREC_RGB_DTM_group1_densified_point_cloud.laz"
  shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_area.shp")

  las_file<- cloud
  shapefile_clip<- shape
  # Function to intersect LAS file with shapefile
  intersect_las <- function(las_file, shapefile_clip) {
    
    # Read LAS file
    cloud_check <- readLAS(las_file, select = "xyz")
    
    # Intersect with shapefile
    las_intersect <- clip_roi(cloud_check, shapefile_clip)
    
    # Check if intersection is not empty
    if (is.empty(las_intersect)) {
      stop("The LAS file does not intersect the shapefile.")
    } else {
      message("Shapefile overlay the LAS data -- OK")
    }

  }
  
  intersect_las(cloud, shape)
  
  
  library(lidR)
  
  # Function to intersect LAS file with shapefile
  intersect_las <- function(las_file, shapefile_clip) {
    
    # Build spatial index from LAS file
    las_index <- createIndex(las_file)
    
    # Get spatial extent of shapefile
    shape_ext <- st_as_sfc(shapefile_clip) %>% st_bbox()
    
    # Retrieve points within shape extent using spatial index
    las_points <- readLAScatalog(las_file, filter = lasclipper::bbox_filter(shape_ext), index = las_index) %>% 
      lasclipper::clip_roi(shapefile_clip)
    
    # Check if intersection is not empty
    if (nrow(las_points) == 0) {
      stop("The LAS file does not intersect the shapefile.")
    } else {
      message("Shapefile overlay the LAS data -- OK")
    }
    
  }
  
  
  ###############
  ###############
  rm(list=ls())
  library(sf)
  library(raster)
  
  shape <- st_read("C:\\Users\\leoag\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2020\\SVREC_PH\\b._Shapefiles\\SVREC_2020_shapefile_plots_03.shp")
  
  imgFiles.dsm <- "C:\\Users\\leoag\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2020\\SVREC_PH\\c._DSM\\8_14_20_Pix4D_dsm_clip.tif"
  DSM1 <- stack(imgFiles.dsm)
  
  overlay_data <-  function(data, shape){
    tryCatch({
      for(p in 1:nrow(shape)){ 
       crop(data, st_bbox(shape[p,]))
      }
    },
    error = function(e) {
      if (grepl("extents do not overlap", e$message)) {
        stop("Error: extents do not overlap")
      } else {
        message("Shapefile overlay the data image -- OK")
      }
    })
  }
  

  overlay_data(DSM1, shape)  
  
  shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_03.shp")
  #shape2 <- st_read("C:\\Users\\leoag\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2022\\SVREC_PH\\b._Shapefiles\\2022_SVREC_Shp_area.shp")
  
  imgFiles.dsm <- "C:\\PH2_R\\DSM\\7_14_21_SVREC_RGB_P1_dsm (2).tif"
  DSM1 <- stack(imgFiles.dsm)
  
  overlay_data(DSM1, shape)  
  
  
  overlay_data <-  function(data, shape){
    tryCatch({
      overlay<- terra::intersect(extent(data), shape)
      if (is.null(overlay)) {
        stop("The data does not intersect the shapefile.")
      } else {
        message("Shapefile overlay the data image -- OK")
      }
    },
    error = function(e) {
      showNotification(paste("Error in overlay_data:", e), type = "error")
      showNotification("The data does not intersect the shapefile.", type = "warning", duration = 10)
      message(paste("Error in overlay_data:", e))
    })
  }
  
  
  #############
  
  
  rm(list=ls())
  library(sf)
  library(raster)
  
  shape <- st_read("C:\\PH2_R\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_03.shp")
  imgFiles.dsm <- "C:\\PH2_R\\DSM\\7_14_21_SVREC_RGB_P1_dsm.tif"
  
  DSM1 <- stack(imgFiles.dsm)
  
  DSM1.c <-  crop(DSM1, st_bbox(shape[1,]))
  
  plot(DSM1.c)
  
  
  DSM1.c <-  crop(DSM1, st_bbox(shape[1,]))
  DSM2.c <-  crop(DSM1, st_bbox(shape[1,]))
  
  ########## Interquartile Range (IQR) ############  
  # Calculate the first quartile (Q1), third quartile (Q3), and IQR
  Q1_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.25)
  Q3_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.75)
  IQR_DSM <- Q3_DSM - Q1_DSM
  
  # Define the lower and upper bounds for outliers
  kout = 1.5
  lower_bound <- Q1_DSM - kout * IQR_DSM
  upper_bound <- Q3_DSM + kout * IQR_DSM
  
  # Remove outliers in DSM1
  DSM1.c <- DSM1.c[DSM1.c >= lower_bound & DSM1.c <= upper_bound,]
  DSM2.c <- DSM2.c[DSM2.c >= lower_bound & DSM2.c <= upper_bound,]
  
  # EPH.Extract<-extract(x = DSM1.c, y = shape[1,])
  # EPH_soil <-  lapply(EPH.Extract, quantile, probs = 0.5, na.rm=TRUE)
  
  csm_rem <- quantile(DSM1.c,0.5)
  
  DSM1.c[DSM1.c <= csm_rem] = NA 
  DSM2.c[DSM2.c > csm_rem] = NA
  
 #  # Remove outliers in DSM1
 #  mean_DSM <- cellStats(DSM1.c,mean)
 #  std_DSM <- cellStats(DSM1.c,sd)
 #  kout = 3 
 #  c_DSM1<- DSM1.c[DSM1.c >= mean_DSM - kout * std_DSM & 
 #                    DSM1.c <= mean_DSM + kout * std_DSM,]
 # 
 # # plot(DSM1.c)  
 #  #plot(c_DSM1)
 #  
 #  mean(c_DSM1)
 #  
########## Interquartile Range (IQR) ############  
  # Calculate the first quartile (Q1), third quartile (Q3), and IQR
  Q1_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.25)
  Q3_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.75)
  IQR_DSM <- Q3_DSM - Q1_DSM
  
  # Define the lower and upper bounds for outliers
  kout = 1.5
  lower_bound <- Q1_DSM - kout * IQR_DSM
  upper_bound <- Q3_DSM + kout * IQR_DSM
  
  # Remove outliers in DSM1
  DSM1.c2 <- DSM1.c[DSM1.c >= lower_bound & DSM1.c <= upper_bound,]
  
  
  mean(DSM1.c2)
  
############
  
  # Create a new raster from the matrix
  c_DSM1_raster <- raster(DSM1.c2)
  
  # Set the extent and CRS of the new raster to match the original DSM
  extent(c_DSM1_raster) <- extent(DSM1.c)
  crs(c_DSM1_raster) <- crs(DSM1.c)
  
  # Plot the cleaned raster (c_DSM1_raster)
  plot(c_DSM1_raster, main = "Cleaned DSM")
  
  #################
  
  
  rm(list=ls())
  library(sf)
  library(raster)
  
  shape <- st_read("I:\\My Drive\\UAS_Beans\\Beans_PlantHeight\\PlantHeightR\\Shapefile\\shp\\Shapefile_SVREV_P01_03.shp")
  imgFiles.dsm <- "I:\\My Drive\\UAS_Beans\\Beans_PlantHeight\\DSM\\7_14_21_SVREC_RGB_P1_dsm.tif"
  
  DSM1 <- stack(imgFiles.dsm)
  # Adjust margins using par()
  par(mar = c(0, 0, 0, 0))
  raster::plot(DSM1)
  

  
  # Get the bounding box of the first shape
  bbox <- st_bbox(shape[1,])
  
  # Convert the bounding box to an sf object
  bbox_sf <- st_as_sfc(bbox)
  
  # Plot the sf object on top of the raster plot
  plot(bbox_sf, add=TRUE, border="red", lwd=2)
  
  #############
  
  
  library(sf)
  library(raster)
  
  shape <- st_read("E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2021\\SVREC_PH\\b._Shapefiles\\Shapefile_SVREV_P01_03.shp")
  DSM1 <- "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2021\\SVREC_PH\\c._DSM\\8_13_21_SVREC_RGB_P1_dsm_clip.tif"
  DSM0 <-  "E:\\OneDrive\\Michigan State University\\MSU Dry Bean Breeding Lab - General\\UAS_Beans\\2021\\SVREC_PH\\d._DTM\\6_8_21_SVREC_RGB_DTM_dsm_clip.tif"

  DSM1 <- stack(DSM1)
  
  DSM0 <- stack(DSM0)

  
  # print(selected_dsm())
  k <- 15
  
  
  DSM1.c <-  crop(DSM1, st_bbox(shape[k,]))
  DSM0.c <-  crop(DSM0, st_bbox(shape[k,]))
  
  plot(DSM1.c)
  plot(DSM0.c)
  
  
  DSM0.r <-resample(DSM0.c, DSM1.c)
  csm <- DSM1.c-DSM0.r 
  csm[csm <=0] = NA
  
  plot(csm)
  
  Q1_DSM <- quantile(csm, probs = 0.25, na.rm=TRUE)
  Q3_DSM <- quantile(csm, probs = 0.75, na.rm=TRUE)
  IQR_DSM <- Q3_DSM - Q1_DSM
  
  # Define thelower and upper bounds for outliers
  kout = 1
  lower_bound <- Q1_DSM - kout * IQR_DSM
  upper_bound <- Q3_DSM + kout * IQR_DSM
  
  # Remove outliers in DSM1.c using overlay() function
  csm <- overlay(csm, fun = function(x) {
    x[x < lower_bound | x > upper_bound] <- NA
    return(x)
  })
  
  plot(csm)
  
  EPH_rem = cellStats(csm,median, na.rm=TRUE)
  
  csm[csm <= EPH_rem[[1]][[1]]] = NA 
  
  plot(csm)
  
  levelplot(csm[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  DSM1.c <-  crop(DSM1, st_bbox(shape[k,]))
  
  Q1_DSM <- quantile(DSM1.c, probs = 0.25, na.rm=TRUE)
  Q3_DSM <- quantile(DSM1.c, probs = 0.75, na.rm=TRUE)
  IQR_DSM <- Q3_DSM - Q1_DSM
  
  # Define the lower and upper bounds for outliers
  kout = 1
  lower_bound <- Q1_DSM - kout * IQR_DSM
  upper_bound <- Q3_DSM + kout * IQR_DSM
  
  # Remove outliers in DSM1.c using overlay() function
  DSM1.c <- overlay(DSM1.c, fun = function(x) {
    x[x < lower_bound | x > upper_bound] <- NA
    return(x)
  })
  
  
  EPH.Extract<-extract(x = DSM1.c, y = shape[k,])

  EPH.soil<-lapply(EPH.Extract, quantile, probs = 0.5, na.rm=TRUE)
  DSM1.c[DSM1.c <= EPH.soil[[1]][[1]]] = NA 
  
  csm_rem <- quantile(DSM1.c,input$qthresh, na.rm=TRUE)
  csm_q <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
  
  EPH_rem = switch(input$analysis_type,
                   "mean" = cellStats(DSM1.c,mean, na.rm=TRUE), 
                   "median" = cellStats(DSM1.c,median, na.rm=TRUE),
                   "quantile" = cellStats(csm_q,median, na.rm=TRUE))
  
  DSM1.c[DSM1.c <= EPH_rem[[1]][[1]]] = NA 
  
  output$rasterPlotDSM_file_ind2 <- renderText(paste0("Plot CSM figure: ", indPlots_col()$PlotID_sel[k],
                                                      " - Assigning selected pixels."))
  levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")
  
  