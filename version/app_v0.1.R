# SHINY DASHBOARD - PlantHeight_R =========================================================================

## A tool to estimate the plant height using the digital surface model (DSM)

## Tab package =========================================================================

### Packages library =================================================
library("shiny")
library("shinyFiles")
library("shinydashboard")
library("dashboardthemes")
library("shinyjs")
#library("shinyWidgets")

options(shiny.maxRequestSize=100000*1024^2)

packages_to_check <- c( "shiny", "shinydashboard", "dashboardthemes", "data.table", "DT","readr", "inspectdf", 
                        "raster", "FIELDimageR", "ggplot2", "sf", "doParallel", "parallel", "foreach",
                        "rlas", "lidR", "rasterVis")


package_tab <-     tabItem(
  tabName = "Packages",
  fluidPage(
    titlePanel("Packages library"),
    actionButton("check_button", "Check packages"),
    actionButton("install_button", "Install and Load Packages"),
    verbatimTextOutput("result")
  )
)


## Tab Content =========================================================================

### Framework tab ----------------------------------------------

disclosure_tab <- tabItem(
  tabName = "Disclosure",
  img(src = "logo.png", height =200, width = 200,style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("PlantHeightR is a R Shiny application developed to help researchers obtain plant height or vegetation heights estimation (EPH) using the DSM (Digital Surface Model"),
  p("Developed by Dr. Volpato"),
  br(),
  h4("Tutorial"),
  p("A comprehensive evaluation of the tool and a walk-through tutorial
      on how to use PlantHeightR is ongoing", tags$a(href="https://github.com/volpatoo/PlantHeightR","available soon")),
  br(),
  br(),
  h4("Citation"),
  p("Volpato L, Pinto F, González-Pérez L, Thompson IG, Borém A, Reynolds M, Gérard B, Molero G and Rodrigues FA Jr (2021) High Throughput Field Phenotyping for Plant Height Using UAV-Based RGB Imagery in Wheat Breeding Lines: Feasibility and Validation. Front. Plant Sci. 12:591587. doi: 10.3389/fpls.2021.591587"),
  br(),
  h4("Credits"),
  p("Leonardo Volpato (volpato1@msu.edu) and Francisco Gomez (gomezfr1@msu.edu)"),
  br(),
  h4("Disclaimer"),
  p("We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness, reliability, or utility 
    of the results if incorrect selections are made during the steps of EPH estimation. PlantHeightR is freely accessible, and the source code is hosted at https://github.com/volpatoo/PlantHeightR")
)

### Snipping field tab ----------------------------------------------

clip_tab <- tabItem(
  tabName = "ClipIMG",
  fluidPage(
    titlePanel("Cropping image"),
    h4("Reducing ohomosaic or Point cloud data by the field polygon shapefile"),
    mainPanel(
      tabsetPanel(
         tabPanel("DSM",
                  br(),
                  actionButton("shapefile_button_clip.dsm", "Load Shp field area"),
                  br(),
                  br(),
                  DT::dataTableOutput("shapefile_name_clip.dsm"),
                  br(),
                  br(),
                  textInput(
                    inputId = "dsm_folder_area",
                    label = "Enter DSM folder path:"
                  ),
                  textOutput(outputId = "dsm_folder_name_area"),
                  textOutput(outputId = "imgFiles.dsm_area"),
                  br(),

                  actionButton("n.core_button3", "Check cores"),
                  textOutput(outputId = "n.core3"),
                  uiOutput("core.thresh.ui3"),
                  br(),

                  actionButton("Clip_DSM", "RUN DSM snipping"),
                  h4("Snipping DSM using field shp area and saving to folder")
         ),
         
         tabPanel("Point Cloud",
                  br(),
                  actionButton("shapefile_button_clip.pc", "Load Shp field area"),
                  br(),
                  br(),
                  DT::dataTableOutput("shapefile_name_clip.pc"),
                  br(),
                  br(),
                  textInput(
                    inputId = "laz_folder_area.pc",
                    label = "Enter LAZ folder path:"
                  ),
                  textOutput(outputId = "laz_folder_name_area"),
                  textOutput(outputId = "imgFiles.laz_area.pc"),
                  br(),
                  
                  actionButton("n.core_button2", "Check cores"),
                  textOutput(outputId = "n.core2"),
                  uiOutput("core.thresh.ui2"),
                  br(),
                  
                  actionButton("Clip_PC", "RUN Point Cloud snipping"),
                  h4("Snipping Point Cloud using field shp area and saving to folder")
         )
      )
    )
  )
)


### Upload file tab ----------------------------------------------

    upload_tab <- tabItem(
      tabName = "FileUpload",
      fluidPage(
        titlePanel("Phenotyping dataset"),
        mainPanel(
          tabsetPanel(
            tabPanel("DSM data",
                     br(),
          textInput(
            inputId = "dsm_folder",
            label = "Enter DSM folder path:"
          ),
          textOutput(outputId = "dsm_folder_name"),
          textOutput(outputId = "imgFiles.dsm"),
        ),
        tabPanel("DTM data",
                 br(),
                 textInput(
                   inputId = "dtm_folder",
                   label = "Enter DTM folder path:"
                 ),
                 textOutput(outputId = "dtm_folder_name"),
                 textOutput(outputId = "imgFiles.dtm"),
        ),
        tabPanel("LAZ veg data",
                 br(),
          textInput(
            inputId = "laz_folder",
            label = "Enter VEGETATION LAZ folder path:"
          ),
          
          textOutput(outputId = "laz_folder_name"),
          textOutput(outputId = "imgFiles.laz")
        ),
        
          tabPanel("LAZ soil data",
                   br(),
          textInput(
            inputId = "laz_folder.soil",
            label = "Enter SOIL LAZ folder path:"
          ),
          
          textOutput(outputId = "laz_folder_name_soil"),
          textOutput(outputId = "imgFiles.laz.soil")
        ),
        
      tabPanel("Shapefile plots",
               br(),
          actionButton("shapefile_button", "Load Shapefile"),
            textOutput(outputId = "shapefile_name"),
          selectInput("plotID",
                      "Select the Plot ID name:",
                      choices = NULL),
          verbatimTextOutput(outputId = "indPlotsName"),

        br(),
        h4("Shapefile features"),

        DT::dataTableOutput('contents'),

      sidebarPanel(
        # Display indPlots_df as table in UI
        br(),
        DT::dataTableOutput("indPlots_col_plot"),

        ))
        )
      )


    )
    )


### Plant Height ------------------------------------------------------

EPH_tab <- tabItem(
  tabName = "EPH",
  fluidPage(
    useShinyjs(), # enable the use of ShinyJS in your app
    titlePanel("Plant Height Estimation"),
    sidebarPanel(
      radioButtons("PH_engine",
                   "Select engine method:",
                   choices = c("CSM (DSM - DTM)", "Point Cloud")),
      checkboxInput("dsm_single", "Analysis based solely on DSM or Vegetation Point Cloud?"),
      
      conditionalPanel(
        condition = "input.dsm_single == true",
        numericInput("qthreshSoil_single", "Assign quantile threshold to soil:",
                     min = 0.01, max = 99.9, step = 0.01, value = 0.1),
      ),
      
      conditionalPanel(
        condition = "input.PH_engine.includes('Point Cloud') && input.dsm_single == false",
        numericInput("qthreshSoil", "Soil Quantile threshold:",
                     min = 0.01, max = 99.9, step = 0.01, value = 0.5),
      ),
      br(),
      
      selectInput("method",
                  "Select analysis method:",
                  choices = c("Time-Series Data", "Ind. Data Flight")),
      # Dynamic UI for Ind. Data Flight
      uiOutput("ind_data_flight_ui"),
      uiOutput("ind_data_flight_ui_laz"),

      radioButtons("analysis_type",
                   "Select analysis type:",
                   choices = c("mean", "median", "quantile")),
      
      conditionalPanel(
        condition = "input.analysis_type.includes('quantile')",
        numericInput("qthresh", "Vegetation Quantile threshold:",
                    min = 0.01, max = 99.9, step = 0.01, value = 0.99),
      ),
      br(),
      br(),
      
      actionButton("n.core_button", "Check cores"),
      textOutput(outputId = "n.core"),
      uiOutput("core.thresh.ui"),
      
      br(),
      br(),
      
      DT::dataTableOutput('met_time.series.table'),
      DT::dataTableOutput('met_ind_img.table'),
      DT::dataTableOutput('met_ind_img_single.table'),
      DT::dataTableOutput('met_ts_img_single.table'),
      DT::dataTableOutput('met_ts_img_laz_single.table'),
      DT::dataTableOutput('met_ind_img_laz_single.table'),
      

      actionButton("go_button", "Estimate",  class = "btn-primary", disabled = TRUE),
      downloadButton("downloadData", "Download Output"),
      
      div(id = "upload-shapefile-message",
          h4("Please upload a shapefile to enable analysis.")
      )

    ),
    conditionalPanel(
      condition = "input.PH_engine.includes('CSM (DSM - DTM)')",
    sidebarPanel(
      titlePanel("Field DSM"),
      plotOutput("rasterPlotDSM"),
      textOutput(outputId = "rasterPlotDSM_file"),
      
      plotOutput("rasterPlotDSM_ind"),
      textOutput(outputId = "rasterPlotDSM_file_ind"),
      
      plotOutput("rasterPlotDSM_ind2"),
      textOutput(outputId = "rasterPlotDSM_file_ind2")
      
    ),
    sidebarPanel(
      titlePanel("Field DTM"),
      plotOutput("rasterPlotDTM"),
      #textOutput(outputId = "rasterPlotDTM_file"),
      
    )),
      conditionalPanel(
        condition = "input.PH_engine.includes('Point Cloud')",
        sidebarPanel(
          titlePanel("Point cloud - Vegetation"),
          plotOutput("PlotLaz_ind"),
          textOutput(outputId = "PlotLaz_ind_file"),
          
          plotOutput("PlotLaz_ind2"),
          textOutput(outputId = "PlotLaz_ind_file2"),
          
    ),
    sidebarPanel(
      titlePanel("Point cloud - Soil"),
      plotOutput("PlotLaz_ind_s"),
      textOutput(outputId = "PlotLaz_ind_file_s"),
      
      plotOutput("PlotLaz_ind2_s"),
      textOutput(outputId = "PlotLaz_ind_file2_s"))
    )
  )
)



### SideBar content =========================================================================

sideBar_content <- dashboardSidebar(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  sidebarMenu(
    menuItem("Disclosure", tabName = "Disclosure"),
    menuItem("Install and Load Packages", tabName = "Packages"),
    menuItem("Snipping files", tabName = "ClipIMG"),
    menuItem("Uploading files", tabName = "FileUpload"),
    menuItem("Estimating Plant Height", tabName = "EPH")
  )
)

### BODY content ------------------------------------------------------------------------------

body_content <- dashboardBody(
  tabItems(
    disclosure_tab,
    package_tab,
    clip_tab,
    upload_tab,
    EPH_tab
  )
)

## UI =========================================================================

ui <-  dashboardPage(
  
  dashboardHeader(title = "PlantHeightR"),
  ## Sidebar content
  sideBar_content,
  ## Body content
  body_content
)

## Aux Functions =========================================================================

PlantHeigh_est_ind<- function(imgFiles.dtm, dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh) {

  if (length(dataset_img_sel) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }

  setProgress(message = "Running EPH analysis", detail = "")

    DSM0 <- stack(imgFiles.dtm)
    DSM1 <- stack(dataset_img_sel) #ind. DSM selected by the user

    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()

    tryCatch(
    results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                      .packages = c("raster", "sf"),
                      .combine = rbind) %dopar% {

                        DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                        DSM0.c <-  crop(DSM0, st_bbox(indPlots[p,]))

                        # Canopy Surface Model (CSM):
                        DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                        csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                        #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                        csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.

                        # EPH.Extract<-extract(x = csm, y = indPlots[p,])
                        #
                        # #Removing soil based on the lower 10% pixels values
                        # EPH.10<-lapply(EPH.Extract, quantile, probs = 0.1, na.rm=TRUE)
                        # csm[csm <=EPH.10[[1]][[1]]] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.

                        # Extracting the estimate plant height average (EPH):
                        if (ExtMet == "mean") {
                          EPH <- cellStats(csm,mean)

                        } else if (ExtMet == "median"){
                          EPH <- cellStats(csm,median)

                        } else if (ExtMet == "quantile"){
                          csm_rem <- quantile(csm,(qthresh))
                          csm <- reclassify(csm, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
                          EPH <- cellStats(csm,mean) #get the mean of the top quantile

                        }


                      },
    error = function(e) {
      showNotification(paste("Error in PlantHeigh_est_ind:", e), type = "error")
    }
    )

    parallel::stopCluster(cl) # Stopping the parallel function

    # Add "EPH" as a column name
    colnames(results) <- "EPH"

    # Convert results to a data frame
    results_df <- as.data.frame(results)

    # Add row names from Plot_ID as a new column to results data frame
    results_df <- cbind(Plot_ID, results_df)

    results_df$DSM_data<-sapply(imgFiles.dsm.names, function(x) gsub(".tif", "", basename(x)))

    # Assign row names
    row.names(results_df) <- paste0(1:nrow(results_df))

    return(results_df)

    # show a message when the function has finished running
    showNotification("Finished processing DSM", type = "message")

}


PlantHeigh_est_ts<- function(imgFiles.dtm, dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh) {
      
  if (length(dataset_img_sel) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }
  
      j=0
      setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
      
    for(i in 1:length(dataset_img_sel)){ #loop through images
      
      message("Processing DSM: ",paste(dataset_img_sel[i]))
      message("Processing DTM: ",paste(imgFiles.dtm))
      

      DSM0 <- stack(imgFiles.dtm)
      DSM1 <- stack(dataset_img_sel[i])
      
      # Starting parallel
      cl <- makeCluster(n.core, output = "")
      registerDoParallel(cl)
      getDoParWorkers()
      
      tryCatch(
      results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                        .packages = c("raster", "sf"), 
                        .combine = rbind) %dopar% {
                         
                          DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                          DSM0.c <-  crop(DSM0, st_bbox(indPlots[p,]))
                          
                          # Canopy Surface Model (CSM):
                          DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                          csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                          #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                          #message("assume no negative values. Assign NA")
                          csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                          
                          # EPH.Extract<-extract(x = csm, y = indPlots[p,])
                          # 
                          # #Removing soil based on the lower 10% pixels values
                          # EPH.10<-lapply(EPH.Extract, quantile, probs = 0.1, na.rm=TRUE)
                          # csm[csm <=EPH.10[[1]][[1]]] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                          
                          # Extracting the estimate plant height average (EPH):
                          if (ExtMet == "mean") {
                            EPH <- cellStats(csm,mean) 
                            
                          } else if (ExtMet == "median"){
                            EPH <- cellStats(csm,median) 
                            
                          } else if (ExtMet == "quantile"){
                            csm_rem <- quantile(csm,(qthresh))
                            csm <- reclassify(csm, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
                            EPH <- cellStats(csm,mean) #get the mean of the top quantile
                            
                          }
                          
                        },
      error = function(e) {
        showNotification(paste("Error in PlantHeigh_est_ts:", e), type = "error")
      }
      )
      
      parallel::stopCluster(cl) # Stopping the parallel function
      
      # Add "EPH" as a column name
      colnames(results) <- "EPH"
      
      # Convert results to a data frame
      results_df <- as.data.frame(results)
      
      # Add row names from Plot_ID as a new column to results data frame
      results_df <- cbind(Plot_ID, results_df)
      
      #results_df$DSM_data<-imgFiles.dsm.names[i]
      results_df$DSM_data<-sapply(imgFiles.dsm.names[i], function(x) gsub(".tif", "", basename(x)))
      
      if(i==1){results_df.1<-results_df}else{results_df.1<-rbind(results_df.1, results_df)}
      
      j= j+1
      setProgress(message = "Running EPH analysis", detail = paste0("Processing file ", i), value = as.numeric(j)/length(dataset_img_sel))
      
    }
      
      # show a message when the function has finished running
      showNotification("Finished processing DSM", type = "message")
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  return(results_df.1)
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ind <- function(cloud, cloud_s, shape, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names, n.core) {
  
  if (length(cloud) == 0) {
    showNotification("No LAZ files provided", type = "error")
    return()
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  # loading the LAS files
  cloud_soil <- readLAS(files = cloud_s, select = "xyz")
  cloud_veg <- readLAS(files = cloud, select = "xyz")
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  tryCatch(
  results <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                     .packages = c("lidR", "sf"), 
                     .combine = rbind) %dopar% {
                       
                       p <- shape[k,]
                       p_sp <- as(p, "Spatial")
                       c_veg <- clip_rectangle(cloud_veg, xleft = p_sp@bbox['x','min'], 
                                               ytop = p_sp@bbox['y','max'], 
                                               xright = p_sp@bbox['x','max'], 
                                               ybottom = p_sp@bbox['y','min'])
                       
                       c_soil <- clip_rectangle(cloud_soil, xleft = p_sp@bbox['x','min'], 
                                                ytop = p_sp@bbox['y','max'], 
                                                xright = p_sp@bbox['x','max'], 
                                                ybottom = p_sp@bbox['y','min'])
                       
                       # Remove outliers in c_veg
                       mean_veg <- mean(c_veg@data$Z)
                       std_veg <- sd(c_veg@data$Z)
                       kout = 3 
                       c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
                       
                       # Remove outliers in c_soil
                       mean_soil <- mean(c_soil@data$Z)
                       std_soil <- sd(c_soil@data$Z)
                       c_soil <- c_soil[c_soil@data$Z >= mean_soil - kout * std_soil & c_soil@data$Z <= mean_soil + kout * std_soil,]
                       
                       # Extracting the estimate plant height average (EPH):
                       if (ExtMet == "mean") {
                         EPH1 <- mean(c_veg@data$Z)
                         EPH2 <- mean(c_soil@data$Z)
                         
                         # Compute difference and round to two decimal places
                         EPH <- round(EPH1 - EPH2, 4)
                         
                       } else if (ExtMet == "median") {
                         EPH1 <- median(c_veg@data$Z)
                         EPH2 <- median(c_soil@data$Z)
                         
                         # Compute difference and round to two decimal places
                         EPH <- round(EPH1 - EPH2, 4)
                         
                       } else if (ExtMet == "quantile"){
                         
                         #applying percentile to canopy height
                         EPH1 <-  quantile(c_veg@data$Z, qthresh) 
                         #applying percentile to soil height
                         EPH2 <- quantile(c_soil@data$Z, qthreshSoil)  
                         
                         EPH = round(as.numeric(EPH1) - as.numeric(EPH2), 4)
                         
                       }
                     },
  error = function(e) {
    showNotification(paste("Error in clipper.cloud.est.ind:", e), type = "error")
  }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  # Add "EPH" as a column name
  colnames(results) <- "EPH"
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$PointCloud_data<-sapply(cloud_names, function(x) gsub(".laz", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  return(results_df)
  
  # show a message when the function has finished running
  showNotification("Finished processing LAZ", type = "message")
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ts <- function(cloud_list, cloud_soil, shape, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names, n.core) {
  
  if (length(cloud_list) == 0) {
    showNotification("No LAZ files provided", type = "error")
    return()
  }
  
  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  # loading the LAS files
  cloud_s <- readLAS(files = cloud_soil, select = "xyz")
  
  for(i in 1:length(cloud_list)){ #loop through images
    
    message("Processing Point Cloud - Vegetation: ",paste(cloud_list[[i]]))
    message("Processing Point Cloud - Soil: ", paste(cloud_soil))
    
    # loading the LAS files
    cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
    
    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
    setProgress(message = "Running EPH analysis", detail = "")
    
    tryCatch(
    results <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                       .packages = c("lidR", "sf"), 
                       .combine = rbind) %dopar% {
                         
                         p <- shape[k,]
                         p_sp <- as(p, "Spatial")
                         c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                                 ytop = p_sp@bbox['y','max'], 
                                                 xright = p_sp@bbox['x','max'], 
                                                 ybottom = p_sp@bbox['y','min'])
                         
                         c_soil <- clip_rectangle(cloud_s, xleft = p_sp@bbox['x','min'], 
                                                  ytop = p_sp@bbox['y','max'], 
                                                  xright = p_sp@bbox['x','max'], 
                                                  ybottom = p_sp@bbox['y','min'])
                         
                         # Remove outliers in c_veg
                         mean_veg <- mean(c_veg@data$Z)
                         std_veg <- sd(c_veg@data$Z)
                         kout = 3 
                         c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
                         
                         # Remove outliers in c_soil
                         mean_soil <- mean(c_soil@data$Z)
                         std_soil <- sd(c_soil@data$Z)
                         c_soil <- c_soil[c_soil@data$Z >= mean_soil - kout * std_soil & c_soil@data$Z <= mean_soil + kout * std_soil,]
                         
                         # Extracting the estimate plant height average (EPH):
                         if (ExtMet == "mean") {
                           EPH1 <- mean(c_veg@data$Z)
                           EPH2 <- mean(c_soil@data$Z)
                           
                           # Compute difference and round to two decimal places
                           EPH <- round(EPH1 - EPH2, 4)
                           
                         } else if (ExtMet == "median") {
                           EPH1 <- median(c_veg@data$Z)
                           EPH2 <- median(c_soil@data$Z)
                           
                           # Compute difference and round to two decimal places
                           EPH <- round(EPH1 - EPH2, 4)
                           
                         } else if (ExtMet == "quantile"){
                           
                           #applying percentile to canopy height
                           EPH1 <-  quantile(c_veg@data$Z, qthresh) 
                           #applying percentile to soil height
                           EPH2 <- quantile(c_soil@data$Z, qthreshSoil)  
                           
                           EPH = round(as.numeric(EPH1) - as.numeric(EPH2), 4)
                           
                         }
                       },
    error = function(e) {
      showNotification(paste("Error in clipper.cloud.est.ts:", e), type = "error")
    }
    )
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    
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
    setProgress(message = "Running EPH analysis", detail = paste0("Processing file ", i), value = as.numeric(j)/length(cloud_list))
    
  }
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  return(results_df.1)
  
  # show a message when the function has finished running
  showNotification("Finished processing LAz", type = "message")
  
}


# Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.area.pc<- function(cloud_list, shape, n.core) {
  
  if (length(cloud_list) == 0) {
    showNotification("No Point Cloud files provided", type = "error")
    return()
  }
  
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  tryCatch(
  foreach(i = 1:length(cloud_list), 
          .packages = c("lidR", "sf"),
          .combine='c') %dopar% {
    
    #message("Processing Point Cloud - Clipping: ",paste(cloud_list[[i]]))
    
    # loading the LAS files
    cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
    
    # clip the point cloud by the polygon shapefile
    clipped_las <- clip_roi(cloud, shape)
    
    # save the clipped point cloud to a new LAS file
    cloud_names <- sapply(cloud_list[i], function(x) gsub(".laz", "", x))
    writeLAS(clipped_las, paste0(cloud_names,"_clip.laz"))

          },
  error = function(e) {
    showNotification(paste("Error in clipper.cloud.area.pc:", e), type = "error")
  }
  )
  
  stopCluster(cl)
  
  # show a message when the function has finished running
  showNotification("Finished processing point clouds", type = "message")
}



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
              DSM <- stack(dsm_list[[i]])
              
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


PH_est_ind_single<- function(dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh, qthreshsingle) {
  
  if (length(dataset_img_sel) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  
  DSM1 <- stack(dataset_img_sel) #ind. DSM selected by the user
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  tryCatch(
    results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                      .packages = c("raster", "sf"), 
                      .combine = rbind) %dopar% {
                        
                        DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                        DSM2.c <-  crop(DSM1, st_bbox(indPlots[p,]))

                              EPH.Extract<-extract(x = DSM1.c, y = indPlots[p,])
                              EPH_soil <-  lapply(EPH.Extract, quantile, probs = qthreshsingle, na.rm=TRUE)
                              DSM1.c[DSM1.c <= EPH_soil[[1]][[1]]] = NA 
                              DSM2.c[DSM2.c > EPH_soil[[1]][[1]]] = NA

                        # Extracting the estimate plant height average (EPH):
                        if (ExtMet == "mean") {
                          EPH1 <- cellStats(DSM1.c,mean) 
                          EPH2 <- cellStats(DSM2.c,mean)
                          # Compute difference and round to four decimal places
                          EPH <- round(EPH1 - EPH2, 4)
                          
                        } else if (ExtMet == "median"){
                          EPH1 <- cellStats(DSM1.c,median) 
                          EPH2 <- cellStats(DSM2.c,median)
                          # Compute difference and round to four decimal places
                          EPH <- round(EPH1 - EPH2, 4)
                          
                        } else if (ExtMet == "quantile"){
                          csm_rem <- quantile(DSM1.c,(qthresh))
                          DSM1.c <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
                          EPH1 <- cellStats(DSM1.c,mean) #get the mean of the top quantile
                         
                          csm_rem2 <- quantile(DSM2.c,(1-qthresh))
                          DSM2.c <- reclassify(DSM2.c, cbind(csm_rem2, Inf, NA)) # Remove values higher than the quantile threshold and set them to NA.
                          EPH2 <- cellStats(DSM2.c,mean) #get the mean of the top quantile
                          # Compute difference and round to four decimal places
                          EPH <- round(EPH1 - EPH2, 4)
                        }
                        
                        
                      },
    error = function(e) {
      showNotification(paste("Error in PH_est_ind_single:", e), type = "error")
    }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  # Add "EPH" as a column name
  colnames(results) <- "EPH"
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$DSM_data<-sapply(imgFiles.dsm.names, function(x) gsub(".tif", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  return(results_df)
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message")
  
}


PH_est_ts_single<- function(dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh, qthreshsingle) {
  
  if (length(dataset_img_sel) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }
  
  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  for(i in 1:length(dataset_img_sel)){ #loop through images
    
    message("Processing DSM: ",paste(dataset_img_sel[i]))

    DSM1 <- stack(dataset_img_sel[i])
    
    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
    tryCatch(
      results<- foreach(p = 1:nrow(indPlots),  #loop through plots numbers
                        .packages = c("raster", "sf"), 
                        .combine = rbind) %dopar% {
                          
                          DSM1.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                          DSM2.c <-  crop(DSM1, st_bbox(indPlots[p,]))
                          
                          EPH.Extract<-extract(x = DSM1.c, y = indPlots[p,])
                          EPH_soil <-  lapply(EPH.Extract, quantile, probs = qthreshsingle, na.rm=TRUE)
                          DSM1.c[DSM1.c <= EPH_soil[[1]][[1]]] = NA 
                          DSM2.c[DSM2.c > EPH_soil[[1]][[1]]] = NA
                          
                          # Extracting the estimate plant height average (EPH):
                          if (ExtMet == "mean") {
                            EPH1 <- cellStats(DSM1.c,mean) 
                            EPH2 <- cellStats(DSM2.c,mean)
                            # Compute difference and round to four decimal places
                            EPH <- round(EPH1 - EPH2, 4)
                            
                          } else if (ExtMet == "median"){
                            EPH1 <- cellStats(DSM1.c,median) 
                            EPH2 <- cellStats(DSM2.c,median)
                            # Compute difference and round to four decimal places
                            EPH <- round(EPH1 - EPH2, 4)
                            
                          } else if (ExtMet == "quantile"){
                            csm_rem <- quantile(DSM1.c,(qthresh))
                            DSM1.c <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
                            EPH1 <- cellStats(DSM1.c,mean) #get the mean of the top quantile
                            
                            csm_rem2 <- quantile(DSM2.c,(1-qthresh))
                            DSM2.c <- reclassify(DSM2.c, cbind(csm_rem2, Inf, NA)) # Remove values higher than the quantile threshold and set them to NA.
                            EPH2 <- cellStats(DSM2.c,mean) #get the mean of the top quantile
                            # Compute difference and round to four decimal places
                            EPH <- round(EPH1 - EPH2, 4)
                          }
                          
                          
                        },
      error = function(e) {
        showNotification(paste("Error in PH_est_ts_single:", e), type = "error")
      }
    ) 
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    # Add "EPH" as a column name
    colnames(results) <- "EPH"
    
    # Convert results to a data frame
    results_df <- as.data.frame(results)
    
    # Add row names from Plot_ID as a new column to results data frame
    results_df <- cbind(Plot_ID, results_df)
    
    #results_df$DSM_data<-imgFiles.dsm.names[i]
    results_df$DSM_data<-sapply(imgFiles.dsm.names[i], function(x) gsub(".tif", "", basename(x)))
    
    if(i==1){results_df.1<-results_df}else{results_df.1<-rbind(results_df.1, results_df)}
    
    j= j+1
    setProgress(message = "Running EPH analysis", detail = paste0("Processing file ", i), value = as.numeric(j)/length(dataset_img_sel))
    
  }
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message")
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  return(results_df.1)
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ind.single <- function(cloud, shape, qthresh, qthreshsingle, ExtMet, Plot_ID, cloud_names, n.core) {
  
  if (length(cloud) == 0) {
    showNotification("No LAZ files provided", type = "error")
    return()
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  # loading the LAS files
  cloud_veg <- readLAS(files = cloud, select = "xyz")
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  tryCatch(
    results <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                       .packages = c("lidR", "sf"), 
                       .combine = rbind) %dopar% {

                         p <- shape[k,]
                         p_sp <- as(p, "Spatial")
                         c_veg <- clip_rectangle(cloud_veg, xleft = p_sp@bbox['x','min'], 
                                                 ytop = p_sp@bbox['y','max'], 
                                                 xright = p_sp@bbox['x','max'], 
                                                 ybottom = p_sp@bbox['y','min'])
                         # Remove outliers in c_veg
                         mean_veg <- mean(c_veg@data$Z)
                         std_veg <- sd(c_veg@data$Z)
                         kout = 3 
                         c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
                         
                         EPH.Extract.veg<- c_veg@data$Z
                         EPH.Extract.soil<- c_veg@data$Z
                         
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
                       },
    error = function(e) {
      showNotification(paste("Error in clipper.cloud.est.ind.single:", e), type = "error")
    }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  # Add "EPH" as a column name
  colnames(results) <- "EPH"
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$PointCloud_data<-sapply(cloud_names, function(x) gsub(".laz", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  return(results_df)
  
  # show a message when the function has finished running
  showNotification("Finished processing LAZ", type = "message")
  
}

## Time series data - PC single flight
clipper.cloud.est.ts.single <- function(cloud_list, shape, qthresh, qthreshsingle, ExtMet, Plot_ID, cloud_names, n.core) {
  
  if (length(cloud_list) == 0) {
    showNotification("No LAZ files provided", type = "error")
    return()
  }
  
  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  for(i in 1:length(cloud_list)){ #loop through images
    
    message("Processing Point Cloud - Vegetation: ",paste(cloud_list[[i]]))
    
    # loading the LAS files
    cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
    
    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
    setProgress(message = "Running EPH analysis", detail = "")
    
    tryCatch(
      results <- foreach(k = 1:nrow(shape),  #loop through plots numbers
                         .packages = c("lidR", "sf"), 
                         .combine = rbind) %dopar% {
                           
                           p <- shape[k,]
                           p_sp <- as(p, "Spatial")
                           c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                                   ytop = p_sp@bbox['y','max'], 
                                                   xright = p_sp@bbox['x','max'], 
                                                   ybottom = p_sp@bbox['y','min'])
                           # Remove outliers in c_veg
                           mean_veg <- mean(c_veg@data$Z)
                           std_veg <- sd(c_veg@data$Z)
                           kout = 3 
                           c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
                           
                           EPH.Extract.veg<- c_veg@data$Z
                           EPH.Extract.soil<- c_veg@data$Z
                           
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
                             
                             EPH_rem1 <-  quantile(EPH.Extract.veg, probs = qthresh, na.rm=TRUE)
                             EPH.Extract.veg[EPH.Extract.veg <= EPH_rem1[[1]][[1]]] = NA #Remove values lower than the quantile threshold. set them to NA.
                             EPH1 <- mean(EPH.Extract.veg, na.rm=TRUE) #get the mean of the top quantile
                             
                             EPH_rem2 <- quantile(EPH.Extract.soil, probs = (1-qthresh), na.rm=TRUE)
                             EPH.Extract.soil[EPH.Extract.soil > EPH_rem2[[1]][[1]]] = NA #Remove values lower than the quantile threshold. set them to NA.
                             EPH2 <- mean(EPH.Extract.soil, na.rm=TRUE) #get the mean of the top quantile
                             # Compute difference and round to four decimal places
                             EPH <- round(EPH1 - EPH2, 4)
                             
                           }
                         },
      error = function(e) {
        showNotification(paste("Error in clipper.cloud.est.ts.single:", e), type = "error")
      }
    )
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    
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
    setProgress(message = "Running EPH analysis", detail = paste0("Processing file ", i), value = as.numeric(j)/length(cloud_list))
    
  }
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  return(results_df.1)
  
  # show a message when the function has finished running
  showNotification("Finished processing LAz", type = "message")
  
}



## Server =========================================================================

server <- function(input, output, session) {
  
  # check if the packages are installed
  check_packages <- eventReactive(input$check_button, {
    missing_packages <- c()
    for (package in packages_to_check) {
      if (!require(package, character.only = TRUE)) {
        missing_packages <- c(missing_packages, package)
      }
    }
    if(length(missing_packages)>0) {
      return(paste("The following packages are not installed:", missing_packages))
    } else {
      return("All packages are already installed.")
    }
  })
  
  # Check the R packages 
  observeEvent(input$check_button, {
    output$result <- renderPrint(check_packages())
  })
  
  # install and load packages
  install_and_load_packages <- eventReactive(input$install_button, {
    missing_packages <- c()
    for (package in packages_to_check) {
      if (!require(package, character.only = TRUE)) {
        install.packages(package)
        library(package, character.only = TRUE)
        missing_packages <- c(missing_packages, package)
      }
    }
    if(length(missing_packages)>0) {
      return(paste("The following packages were installed and loaded:", missing_packages))
    } else {
      return("There are no packages to install. The packages were loaded")
    }
  })
  
  observeEvent(input$install_button, {
    output$result <- renderPrint(install_and_load_packages())
  })
  
  
  ## Load the DSM files
  observeEvent(input$dsm_folder, {
    if (!is.null(input$dsm_folder)) {
      if (dir.exists(input$dsm_folder)) {
        dsm_folder_name <- basename(input$dsm_folder)
        imgFiles.dsm <-list.files(path = input$dsm_folder, pattern="*.tif$",full.names = T) #get the DSM 
        imgFiles.dsm <- sapply(imgFiles.dsm, function(x) gsub(".tif", "", basename(x)))
        cat("Selected DSM folder:", dsm_folder_name, "\n")
        imgFiles.dsm.list <- as.list(imgFiles.dsm)
        imgFiles.dsm.list <<- sapply(imgFiles.dsm.list, function(x) gsub(".tif", "", basename(x)))
        output$dsm_folder_name <- renderText(paste0("Selected DSM folder: ", dsm_folder_name))
        output$imgFiles.dsm <- renderText(paste0("Selected DSM files: ", imgFiles.dsm))
      } else {
        cat("Invalid DSM folder path!\n")
        output$dsm_folder_name <- renderText("Invalid DSM folder path!")
        output$imgFiles.dsm <- renderText("Invalid DSM files path!")
      }
    }
  })
 
  ## Load the PC files - vegetation
  observeEvent(input$laz_folder, {
    if (!is.null(input$laz_folder)) {
      if (dir.exists(input$laz_folder)) {
        laz_folder_name <- basename(input$laz_folder)
        imgFiles.laz <-list.files(path = input$laz_folder, pattern="*.laz$",full.names = T) #get the laz only 
        imgFiles.laz <- sapply(imgFiles.laz, function(x) gsub(".laz", "", basename(x)))
        cat("Selected LAZ vegetation folder:", laz_folder_name, "\n")
        imgFiles.laz.list <- as.list(imgFiles.laz)
        imgFiles.laz.list <<- sapply(imgFiles.laz.list, function(x) gsub(".laz", "", basename(x)))
        output$laz_folder_name <- renderText(paste0("Selected LAZ vegetation folder: ", laz_folder_name))
        output$imgFiles.laz <- renderText(paste0("Selected LAZ vegetation files: ", imgFiles.laz))
      } else {
        cat("Invalid LAZ vegetation folder path!\n")
        output$laz_folder_name <- renderText("Invalid LAZ vegetation folder path!")
        output$imgFiles.laz <- renderText("Invalid LAZ vegetation files path!")
      }
    }
  })
  
  ## Load the DTM files
  observeEvent(input$dtm_folder, {
    if (!is.null(input$dtm_folder)) {
      if (dir.exists(input$dtm_folder)) {
        dtm_folder_name <- basename(input$dtm_folder)
        imgFiles.dtm <-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM
        imgFiles.dtm_all <<-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM 
        imgFiles.dtm <- sapply(imgFiles.dtm, function(x) gsub(".tif", "", basename(x)))
         cat("Selected DTM folder:", dtm_folder_name, "\n")
        output$dtm_folder_name <- renderText(paste0("Selected DTM folder: ", dtm_folder_name))
        output$imgFiles.dtm <- renderText(paste0("Selected DTM files: ", imgFiles.dtm))
      } else {
        cat("Invalid DTM folder path!\n")
        output$dtm_folder_name <- renderText("Invalid DTM folder path!")
        output$imgFiles.dtm <- renderText("Invalid DTM file path!")
      }
    }
  })
  
  ## Load the laz files - Point Cloud
  observeEvent(input$laz_folder.soil, {
    if (!is.null(input$laz_folder.soil)) {
      if (dir.exists(input$laz_folder.soil)) {
        laz_folder_name_soil <- basename(input$laz_folder.soil)
        imgFiles.laz.soil <-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) 
        imgFiles.laz.soil_all <<-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) 
        imgFiles.laz.soil <- sapply(imgFiles.laz.soil, function(x) gsub(".laz", "", basename(x)))
        cat("Selected LAZ soil folder:", laz_folder_name_soil, "\n")
        output$laz_folder_name_soil <- renderText(paste0("Selected LAZ soil folder: ", laz_folder_name_soil))
        output$imgFiles.laz.soil <- renderText(paste0("Selected LAZ soil files: ", imgFiles.laz.soil))
      } else {
        cat("Invalid LAZ soil folder path!\n")
        output$laz_folder_name_soil <- renderText("Invalid LAZ soil folder path!")
        output$imgFiles.laz.soil <- renderText("Invalid LAZ soil files path!")
      }
    }
  })


  ## Load the shapefile files
  observeEvent(input$shapefile_button, {
    shapefile_path <- tryCatch(file.choose(),
                               error = function(e) NULL)
    if (!is.null(shapefile_path)) {
      if (endsWith(shapefile_path, ".shp")) {
        # create global variable to store shapefile path
        shapefile_path_global <<- shapefile_path
        shapefile_name <- basename(shapefile_path)
        output$shapefile_name <- renderText(paste0("Selected shapefile: ", shapefile_name))
        indPlots <- st_read(shapefile_path)
        output$contents <- DT::renderDataTable(DT::datatable(indPlots, options = list(pageLength = 5, scrollX = T)))
        # Update choices of plotID select input
        updateSelectInput(session, "plotID",
                          label = "Select the Plot ID name:",
                          choices = colnames(indPlots),
                          selected = colnames(indPlots)[1])
        shinyjs::enable("go_button")
        shinyjs::hide("upload-shapefile-message")
      } else {
        output$shapefile_name <- renderText("Invalid file type! Please select a shapefile (.shp).")
        shinyjs::disable("go_button")
        shinyjs::show("upload-shapefile-message")
        }
    } else {
      output$shapefile_name <- renderText("Invalid shapefile file path!\n")
    }
  })
  
  shinyjs::useShinyjs()
  
  ## Selecting the plot name 
  indPlots <- reactive({
    updateSelectInput(session, "plotID",
                      choices = names(input$indPlots))
    
    input$plotID
  })
  
  output$indPlotsName <- renderPrint({
    if(!is.null(indPlots())) colnames(indPlots())
    indPlots()
  })
  
  ## Printing the data frame using the plotID selected
  indPlots_df <- reactive({
    if (is.null(shapefile_path_global)) {
      return(NULL)
    } else {
      # print(shapefile_path_global)
      st_read(shapefile_path_global) %>% as.data.frame()
    }
  })
  
  
  indPlots_col <- reactive({
    req(input$plotID, indPlots_df())
    if (input$plotID %in% colnames(indPlots_df())) {
      return(data.frame(PlotID_sel = indPlots_df()[, input$plotID]))
    } else {
      return(NULL)
    }
  })
  
  output$indPlots_col_plot <- DT::renderDataTable({
    DT::datatable(indPlots_col(), options = list(pageLength = 5))
  })
  
  ## Shapefile to use in the loop
  indPlots_2 <- reactive({
    if (is.null(shapefile_path_global)) {
      return(NULL)
    } else {
      st_read(shapefile_path_global) 
    }
  })
  

  ## Load and check the number of threads (core)
  n.core <- reactive({
    gc() #Cleaning unusual memmory
    detectCores()
  })
  
  observeEvent(input$n.core_button, {
    output$n.core <- renderText({
      paste0("Number of cores available:", n.core())
    })
    output$core.thresh.ui <- renderUI({
      sliderInput("core.thresh", "Select the number of cores:",
                  min = 1, max = n.core(), step = 1, value = n.core())
    })
  })
  
  ## Load and check the number of threads (core)
  n.core2 <- reactive({
    gc() #Cleaning unusual memmory
    detectCores()
  })
  
  ## Load and check the number of threads (core)
  n.core3 <- reactive({
    gc() #Cleaning unusual memmory
    detectCores()
  })
  
  ## Point Cloud
  observeEvent(input$n.core_button2, {
    output$n.core2 <- renderText({
      paste0("Number of cores available:", n.core2())
    })
    output$core.thresh.ui2 <- renderUI({
      sliderInput("core.thresh", "Select the number of cores:",
                  min = 1, max = n.core2(), step = 1, value = n.core2())
    })
  })
  
  ## DSM
  observeEvent(input$n.core_button3, {
    output$n.core3 <- renderText({
      paste0("Number of cores available:", n.core3())
    })
    output$core.thresh.ui3 <- renderUI({
      sliderInput("core.thresh", "Select the number of cores:",
                  min = 1, max = n.core3(), step = 1, value = n.core3())
    })
  })
  
## Selecting individual images to run - DSM
  
  imgFiles.dsm <- reactive({
    if (!is.null(input$dsm_folder)) {
      if (dir.exists(input$dsm_folder)) {
        files <- list.files(path = input$dsm_folder, pattern="*.tif$", full.names = T) #get the DSM 
        print(files)
        if (length(files) == 0) {
          print("No DSM files found in the selected folder!")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("Invalid DSM folder path!")
        return(NULL)
      }
    } else {
      print("No DSM folder selected!")
      return(NULL)
    }
  })
  
  output$ind_data_flight_ui <- renderUI({
    if (input$method == "Ind. Data Flight" && input$PH_engine == "CSM (DSM - DTM)" && !is.null(imgFiles.dsm())) {
      if (length(imgFiles.dsm()) == 0) {
        print("No flights available!")
        return(NULL)
      }
      radioButtons("flight_select", "Select flight:",
                   choices = imgFiles.dsm(),
                   selected = imgFiles.dsm()[1])
      
    } 
     
    
  })
  
  ## Selecting individual images to run - PC
  
  imgFiles.laz <- reactive({
    if (!is.null(input$laz_folder)) {
      if (dir.exists(input$laz_folder)) {
        files <- list.files(path = input$laz_folder, pattern="*.laz$", full.names = T) #get the PC only 
        print(files)
        if (length(files) == 0) {
          print("No laz files found in the selected folder!")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("Invalid laz folder path!")
        return(NULL)
      }
    } else {
      print("No laz folder selected!")
      return(NULL)
    }
  })
  
  output$ind_data_flight_ui_laz <- renderUI({
    if (input$method == "Ind. Data Flight" && input$PH_engine == "Point Cloud" &&  !is.null(imgFiles.laz())) {
      if (length(imgFiles.laz()) == 0) {
        print("No flights available!")
        return(NULL)
      }
      radioButtons("flight_select_laz", "Select flight:",
                   choices = imgFiles.laz(),
                   selected = imgFiles.laz()[1])
    }
  })
  
  ## Field area DSM
  imgFiles.dsm_area <- reactive({
    if (!is.null(input$dsm_folder_area)) {
      if (dir.exists(input$dsm_folder_area)) {
        files <- list.files(path = input$dsm_folder_area, pattern="*.tif$", full.names = T) #get the PC only 
        print(files)
        if (length(files) == 0) {
          print("No tif files found in the selected folder!")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("Invalid laz folder path!")
        return(NULL)
      }
    } else {
      print("No laz folder selected!")
      return(NULL)
    }
  })
  
  ## Field area PC
  imgFiles.laz_area.pc <- reactive({
    if (!is.null(input$laz_folder_area.pc)) {
      if (dir.exists(input$laz_folder_area.pc)) {
        files <- list.files(path = input$laz_folder_area.pc, pattern="*.laz$", full.names = T) #get the PC only 
        print(files)
        if (length(files) == 0) {
          print("No laz files found in the selected folder!")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("Invalid laz folder path!")
        return(NULL)
      }
    } else {
      print("No laz folder selected!")
      return(NULL)
    }
  })
  

 ####### Plant Height Estimation (EPH)  ####### 
 
  
  ##### DSM PHE using Time Series Data method #####
  met_time.series <- reactive({
    if(is.null(input$dsm_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
   PlantHeigh_est_ts(imgFiles.dtm = imgFiles.dtm_all,
                     dataset_img_sel = imgFiles.dsm(),
                     n.core=n.core(),
                     indPlots=indPlots_2(),
                     Plot_ID = indPlots_col(),
                     imgFiles.dsm.names = imgFiles.dsm.list,
                     ExtMet =input$analysis_type,
                     qthresh = input$qthresh)
   
    })
    
  })
  
  
  #### DSM PHE using individual flight data method ####
  met_ind_img <- reactive({
    if(is.null(input$dsm_folder)) {return("No data found")}
    
    withProgress(message = "Running indvidual flight Data method", {
      setProgress(message = "Running...")
      
      PlantHeigh_est_ind (imgFiles.dtm = imgFiles.dtm_all,
                     dataset_img_sel = input$flight_select,
                     n.core=n.core(),
                     indPlots=indPlots_2(),
                     Plot_ID = indPlots_col(),
                     imgFiles.dsm.names = input$flight_select,
                     ExtMet =input$analysis_type,
                     qthresh = input$qthresh)
    })
    
  })
  

  #### DSM PHE using individual flight data method ####
  met_ind_img_single <- reactive({
    if(is.null(input$dsm_folder)) {return("No data found")}
    
    withProgress(message = "Running indvidual flight Data method", {
      setProgress(message = "Running...")
      
      PH_est_ind_single(
                          dataset_img_sel = input$flight_select,
                          n.core=n.core(),
                          indPlots=indPlots_2(),
                          Plot_ID = indPlots_col(),
                          imgFiles.dsm.names = input$flight_select,
                          ExtMet =input$analysis_type,
                          qthresh = input$qthresh,
                          qthreshsingle = input$qthreshSoil_single)
    })
    
  })
 
  
  #### DSM PHE using time series flight data method ####
  met_ts_img_single <- reactive({
    if(is.null(input$dsm_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      PH_est_ts_single(
                        dataset_img_sel = imgFiles.dsm(),
                        n.core=n.core(),
                        indPlots=indPlots_2(),
                        Plot_ID = indPlots_col(),
                        imgFiles.dsm.names = imgFiles.dsm.list,
                        ExtMet =input$analysis_type,
                        qthresh = input$qthresh,
                        qthreshsingle = input$qthreshSoil_single)
    })
    
  })
  
  

  #### PC PHE using Time Series Data method - Point Cloud ####
  met_time.series_laz <- reactive({
    if(is.null(input$laz_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ts(
                      cloud_list =  imgFiles.laz(),
                      cloud_soil = imgFiles.laz.soil_all,
                      shape = indPlots_2(),
                      Plot_ID = indPlots_col(),
                      qthresh = input$qthresh,
                      qthreshSoil = input$qthreshSoil,
                      ExtMet =input$analysis_type,
                      cloud_names = imgFiles.laz.list,
                      n.core=n.core()
      )

      
    })
    
  })
  
  
  #### PC PHE using individual flight data method ####
  met_ind_img_laz <- reactive({
    if(is.null(input$laz_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ind(
                            cloud =  input$flight_select_laz,
                            cloud_s = imgFiles.laz.soil_all,
                            shape = indPlots_2(),
                            Plot_ID = indPlots_col(),
                            qthresh = input$qthresh,
                            qthreshSoil = input$qthreshSoil,
                            ExtMet =input$analysis_type,
                            cloud_names = input$flight_select_laz,
                            n.core=n.core()
      )
      
    
    })
  })
  
  ## Individual - PC single
  met_ind_img_laz_single <- reactive({
    if(is.null(input$laz_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ind.single(
        cloud =  input$flight_select_laz,
        shape = indPlots_2(),
        Plot_ID = indPlots_col(),
        qthresh = input$qthresh,
        ExtMet =input$analysis_type,
        cloud_names = input$flight_select_laz,
        n.core=n.core(),
        qthreshsingle = input$qthreshSoil_single
      )
      
      
    })
  })
  
  ## Time series - PC single
  met_ts_img_laz_single <- reactive({
    if(is.null(input$laz_folder)) {return("No data found")}
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ts.single(
        cloud =  imgFiles.laz(),
        shape = indPlots_2(),
        Plot_ID = indPlots_col(),
        qthresh = input$qthresh,
        ExtMet =input$analysis_type,
        cloud_names = imgFiles.laz.list,
        n.core=n.core(),
        qthreshsingle = input$qthreshSoil_single
      )
      
      
    })
  })
  
  
  
  observeEvent(input$go_button, {
      
    if(input$PH_engine == "CSM (DSM - DTM)") {
      if(is.null(input$dsm_folder)) {return("No data found")}
      
      if(input$method == "Time-Series Data") {
        
        if (input$dsm_single){
          output$met_ts_img_single.table <- DT::renderDataTable({
            DT::datatable(met_ts_img_single()) 
          })
          
          output$downloadData <- downloadHandler(
            filename = function() { paste0("ts_flights", "_",
                                           input$PH_engine,"_",
                                           input$analysis_type,"_",
                                           input$qthresh, "_",
                                           input$qthreshSoil_single,
                                           ".csv") },
            content = function(file) { write.csv(met_ts_img_single(), file, row.names = FALSE) }
          )
          
        } else {
    
        output$met_time.series.table <- DT::renderDataTable({
          DT::datatable(met_time.series()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ts_flights", "_",
                                         input$PH_engine,"_",
                                         input$analysis_type,"_",
                                         input$qthresh, "_",
                                         ".csv") },
          content = function(file) { write.csv(met_time.series(), file, row.names = FALSE) }
          
        )
        }
        
      } else if (input$method == "Ind. Data Flight"){
        
        if (input$dsm_single){
          output$met_ind_img_single.table <- DT::renderDataTable({
            DT::datatable(met_ind_img_single()) 
          })
          
          output$downloadData <- downloadHandler(
            filename = function() { paste0("ind_flight", "_",
                                           input$PH_engine,"_",
                                           input$analysis_type,"_",
                                           input$qthresh, "_",
                                           input$qthreshSoil_single,
                                           ".csv") },
            content = function(file) { write.csv(met_ind_img_single(), file, row.names = FALSE) }
          )
          
        } else {
        
        output$met_ind_img.table <- DT::renderDataTable({
          DT::datatable(met_ind_img()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ind_flight", "_",
                                         input$PH_engine,"_",
                                         input$analysis_type,"_",
                                         input$qthresh,
                                         ".csv") },
          content = function(file) { write.csv(met_ind_img(), file, row.names = FALSE) }
        )
        
        } 
      }
    
  } else if (input$PH_engine == "Point Cloud"){
    if(is.null(input$laz_folder)) {return("No data found")}
        
      if(input$method == "Time-Series Data") {
          
        if (input$dsm_single){
          output$met_ts_img_laz_single.table <- DT::renderDataTable({
            DT::datatable(met_ts_img_laz_single()) 
          })
          
          output$downloadData <- downloadHandler(
            filename = function() { paste0("ts_flights", "_",
                                           input$PH_engine,"_",
                                           input$analysis_type,"_",
                                           "veg", "_",
                                           input$qthresh, "_",
                                           "soil", "_",
                                           input$qthreshsingle,
                                           ".csv") },
            content = function(file) { write.csv(met_ts_img_laz_single(), file, row.names = FALSE) }
          )
          
        } else {
        
        output$met_time.series.table <- DT::renderDataTable({
          DT::datatable(met_time.series_laz()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ts_flights", "_",
                                         input$PH_engine,"_",
                                         input$analysis_type,"_",
                                         "veg", "_",
                                         input$qthresh, "_",
                                         "soil", "_",
                                         input$qthreshSoil,
                                         ".csv") },
          content = function(file) { write.csv(met_time.series_laz(), file, row.names = FALSE) }
          
        )
        }
        
    } else if (input$method == "Ind. Data Flight"){
      
      if (input$dsm_single){
        output$met_ind_img_laz_single.table <- DT::renderDataTable({
          DT::datatable(met_ind_img_laz_single()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ind_flight", "_",
                                         input$PH_engine,"_",
                                         input$analysis_type,"_",
                                         "veg", "_",
                                         input$qthresh, "_",
                                         "soil", "_",
                                         input$qthreshsingle,
                                         ".csv") },
          content = function(file) { write.csv(met_ind_img_laz_single(), file, row.names = FALSE) }
        )
        
      } else {
        
        output$met_ind_img.table <- DT::renderDataTable({
          DT::datatable(met_ind_img_laz()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ind_flight", "_",
                                         input$PH_engine,"_",
                                         input$analysis_type,"_",
                                         "veg", "_",
                                         input$qthresh, "_",
                                         "soil", "_",
                                         input$qthreshSoil,
                                         ".csv") },
          content = function(file) { write.csv(met_ind_img_laz(), file, row.names = FALSE) }
        )
        
      } 
      
    }
  }
    
  })
  

  ## DTM
  # Render plot
  output$rasterPlotDTM <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$dtm_folder)) {
        if (dir.exists(input$dtm_folder)) {
          imgFiles.dtm <-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM
          colorPal <- terrain.colors(255)
          DSM0 <- stack(imgFiles.dtm)
          levelplot(DSM0[[1]], col.regions = colorPal, margin = FALSE, main = "Field DTM")
        } else {
          stop("DSM folder does not exist.")
        }
      } else {
        stop("DSM folder input is NULL.")
      }
    }, error = function(e) {
      message("No DTM data available. Please upload a file.")
      #output$rasterPlotDTM_file <- renderText("DTM plot")
      showNotification(paste("No DTM Plot data available. Please upload a file."), type = "error")
      return(NULL)
    })
  })
  })
  
 
  ## DSM
  output$rasterPlotDSM <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$dsm_folder)) {
        if (dir.exists(input$dsm_folder)) {
          colorPal <- terrain.colors(255)
          DSM1 <- stack(input$flight_select)
          levelplot(DSM1[[1]], col.regions = colorPal, margin = FALSE, main = "Field DSM")
        } else {
          stop("DSM folder does not exist.")
        }
      } else {
        stop("DSM folder input is NULL.")
      }
    }, error = function(e) {
      message("No DSM data available. Please upload a file.")
      output$rasterPlotDSM_file <- renderText("DSM visualization - Select analysis method: Ind. Data Flight")
      showNotification(paste("No DSM Plot data available. Please upload a file or select a flight."), type = "error")
      return(NULL)
    })
  })
  })
  
  # Define a reactive value to store the selected p value
  selected_k <- reactiveVal()

  # Render ind plot - DSM
  output$rasterPlotDSM_ind <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$dsm_folder)) {
        if (dir.exists(input$dsm_folder)) {
          colorPal <- terrain.colors(255)
          DSM1 <- stack(input$flight_select)
          k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for k
          selected_k(k) # store the selected k value in the reactive value
          DSM1.c <-  crop(DSM1, st_bbox(indPlots_2()[k,]))
          output$rasterPlotDSM_file_ind <- renderText(paste0("Plot figure: ", indPlots_col()$PlotID_sel[k]))
          levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "Selected plot view")
          } else {
          stop("DSM folder does not exist.")
        }
      } else {
        stop("DSM folder input is NULL.")
      }
    }, error = function(e) {
      message("No DSM Plot data available. Please upload a file.")
      #showNotification(paste("No DSM Plot data available. Please upload a file."), type = "error")
      #output$rasterPlotDSM_file_ind <- renderText(paste0("Plot figure"))
      
      return(NULL)
    })
  })
  })
  
  # Render ind plot2 - DSM
  output$rasterPlotDSM_ind2 <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (input$dsm_single){
      if (!is.null(input$dsm_folder)) {
        if (dir.exists(input$dsm_folder)) {
          colorPal <- terrain.colors(255)
          imgFiles.dtm <-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM
          DSM1 <- stack(input$flight_select)
          k <- selected_k() # access the selected k value from the reactive value
          #k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for k
          DSM1.c <-  crop(DSM1, st_bbox(indPlots_2()[k,]))
          EPH.Extract<-extract(x = DSM1.c, y = indPlots_2()[k,])
          EPH.soil<-lapply(EPH.Extract, quantile, probs = input$qthreshSoil_single, na.rm=TRUE)
          DSM1.c[DSM1.c <= EPH.soil[[1]][[1]]] = NA 
          
          csm_rem <- quantile(DSM1.c,input$qthresh)
          csm_q <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
          
              EPH_rem = switch(input$analysis_type,
                           "mean" = cellStats(DSM1.c,mean), 
                           "median" = cellStats(DSM1.c,median),
                           "quantile" = cellStats(csm_q,mean))
          
          DSM1.c[DSM1.c <= EPH_rem[[1]][[1]]] = NA 
          
          output$rasterPlotDSM_file_ind2 <- renderText(paste0("Plot CSM figure: ", indPlots_col()$PlotID_sel[k],
                                                              " - Assigning selected pixels."))
          levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")
        } else {
          stop("DSM folder does not exist.")
        }
      } else {
        stop("DSM folder input is NULL.")
      }
    } else {
      if (!is.null(input$dsm_folder)) {
        if (dir.exists(input$dsm_folder)) {
          colorPal <- terrain.colors(255)
          imgFiles.dtm <-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM
          DSM0 <- stack(imgFiles.dtm)
          DSM1 <- stack(input$flight_select)
          k <- selected_k() # access the selected k value from the reactive value
          #k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for k
          DSM1.c <-  crop(DSM1, st_bbox(indPlots_2()[k,]))
          DSM0.c <-  crop(DSM0, st_bbox(indPlots_2()[k,]))
          DSM0.r <-resample(DSM0.c, DSM1.c)
          csm <- DSM1.c-DSM0.r 
          csm[csm <=0] = NA
          
          csm_rem <- quantile(csm,input$qthresh)
          csm_q <- reclassify(csm, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
          
          EPH_rem = switch(input$analysis_type,
                            "mean" = cellStats(csm,mean), 
                            "median" = cellStats(csm,median),
                            "quantile" = cellStats(csm_q,mean))
          
          csm[csm <= EPH_rem[[1]][[1]]] = NA 

          output$rasterPlotDSM_file_ind2 <- renderText(paste0("Plot CSM figure: ", indPlots_col()$PlotID_sel[k],
                                                              " - Assigning selected pixels. "))
          levelplot(csm[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")
          
        } else {
          stop("DSM folder does not exist.")
        }
      } else {
        stop("DSM folder input is NULL.")
      }

    }
    }, error = function(e) {
      message("No DSM Plot data available. Please upload a file.")
      #showNotification(paste("No DSM Plot data available. Please upload a file."), type = "error")
      #output$rasterPlotDSM_file_ind2 <- renderText(paste0("Plot figure"))
      
      return(NULL)
    })
  }) 
  })

  
  ### LAZ Point Cloud
  
  # Render ts plot - Point Cloud
  output$PlotLaz_ind <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$laz_folder)) {
        if (dir.exists(input$laz_folder)) {
        
          #imgFiles.laz.dtm <-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) #get the DTM
          #cloud_s <- readLAS(files = imgFiles.laz.dtm, select = "xyz")
          cloud <- readLAS(files = input$flight_select_laz, select = "xyz") 
          k <- selected_k() 
          #k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for p
          p <- indPlots_2()[k,]
          p_sp <- as(p, "Spatial")
          c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                  ytop = p_sp@bbox['y','max'], 
                                  xright = p_sp@bbox['x','max'], 
                                  ybottom = p_sp@bbox['y','min'])
          # Remove outliers in c_veg
          mean_veg <- mean(c_veg@data$Z)
          std_veg <- sd(c_veg@data$Z)
          kout = 3 
          c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
          
          # Set up plot region to arrange p1 and p2 vertically
          par(mfrow=c(2,1))
          
          # Display p1 and p2 in the same plot region
          plot(c_veg@data$Y, c_veg@data$Z, xlab = "Lateral view", ylab = "EPH", main = "Selected Plot - Point cloud distribution") 
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z),
                            "median" = median(c_veg@data$Z),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh)),
                 col = "red")
          plot(c_veg@data$X, c_veg@data$Z, xlab = "EPH", ylab = "Frontal view")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z),
                            "median" = median(c_veg@data$Z),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh)),
                 col = "blue")

          output$PlotLaz_ind_file <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
          
          } else {
          stop("Point cloud folder does not exist.")
        }
      } else {
        stop("Point cloud folder input is NULL.")
      }
    }, error = function(e) {
      message("No Point cloud Plot data available. Please upload a file.")
      output$PlotLaz_ind_file <- renderText(paste0("Point Cloud visualization - Select analysis method: Ind. Data Flight"))
      showNotification(paste("No LAZ vegetaion Plot data available. Please upload a file or select a flight."), type = "error")
      
      return(NULL)
    })
  }) 
  }) 
  
  
  # Render ts average Point Cloud
  output$PlotLaz_ind2 <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$laz_folder)) {
        if (dir.exists(input$laz_folder)) {
          
          #imgFiles.laz.dtm <-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) #get the DTM
          #cloud_s <- readLAS(files = imgFiles.laz.dtm, select = "xyz")
          cloud <- readLAS(files = input$flight_select_laz, select = "xyz") 
          k <- selected_k() 
         # k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for p
          p <- indPlots_2()[k,]
          p_sp <- as(p, "Spatial")
          c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                  ytop = p_sp@bbox['y','max'], 
                                  xright = p_sp@bbox['x','max'], 
                                  ybottom = p_sp@bbox['y','min'])
          # Remove outliers in c_veg
          mean_veg <- mean(c_veg@data$Z)
          std_veg <- sd(c_veg@data$Z)
          kout = 3 
          c_veg <- c_veg[c_veg@data$Z >= mean_veg - kout * std_veg & c_veg@data$Z <= mean_veg + kout * std_veg,]
          
          # Set up plot region to arrange p1 and p2 vertically
          par(mfrow=c(2,1))
          
          # Compute the average value of Z across Y
          avg_Zy <- tapply(c_veg@data$Z, c_veg@data$Y, mean)
          plot(avg_Zy, type="l", xlab="Lateral view", ylab="EPH", main = "Selected Plot - Average point cloud distribution")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z),
                            "median" = median(c_veg@data$Z),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh)),
                 col = "red")
          
          #abline(h = mean(c_veg@data$Z), col = "red")
          
          # Compute the average value of Z across Y
          avg_Zx <- tapply(c_veg@data$Z, c_veg@data$X, mean)
          plot(avg_Zx, type="l", xlab="Frontal view", ylab="EPH")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z),
                            "median" = median(c_veg@data$Z),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh)),
                 col = "blue")
          
          
          output$PlotLaz_ind_file2 <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
          
        } else {
          stop("Point cloud folder does not exist.")
        }
      } else {
        stop("Point cloud folder input is NULL.")
      }
    }, error = function(e) {
      message("No Point cloud plot data available. Please upload a file.")
      #output$PlotLaz_ind_file2 <- renderText(paste0("Point cloud figure"))
      
      return(NULL)
    })
  }) 
  })
    
   
  ### PC soil
  
  # Render ind plot - PC
  output$PlotLaz_ind_s <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
        if (!is.null(input$laz_folder.soil)) {
          if (dir.exists(input$laz_folder.soil)) {
            
            imgFiles.laz.dtm <-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) #get the DTM
            cloud_s <- readLAS(files = imgFiles.laz.dtm, select = "xyz")
            #cloud <- readLAS(files = input$flight_select_laz, select = "xyz") 
            k <- selected_k() 
           # k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for p
            p <- indPlots_2()[k,]
            p_sp <- as(p, "Spatial")
            c_soil <- clip_rectangle(cloud_s, xleft = p_sp@bbox['x','min'], 
                                    ytop = p_sp@bbox['y','max'], 
                                    xright = p_sp@bbox['x','max'], 
                                    ybottom = p_sp@bbox['y','min'])

            # Remove outliers in c_soil
            mean_soil <- mean(c_soil@data$Z)
            std_soil <- sd(c_soil@data$Z)
            c_soil <- c_soil[c_soil@data$Z >= mean_soil - kout * std_soil & c_soil@data$Z <= mean_soil + kout * std_soil,]
            
            
            # Set up plot region to arrange p1 and p2 vertically
            par(mfrow=c(2,1))
            
            # Display p1 and p2 in the same plot region
            plot(c_soil@data$Y, c_soil@data$Z, xlab = "EPH", ylab = "Lateral view", main = "Selected Plot - Point cloud distribution") 
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z),
                              "median" = median(c_soil@data$Z),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil)),
                   col = "red")
            plot(c_soil@data$X, c_soil@data$Z, xlab = "EPH", ylab = "Frontal view")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z),
                              "median" = median(c_soil@data$Z),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil)),
                   col = "blue")
            
            output$PlotLaz_ind_file_s <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
            
          } else {
            stop("Point cloud folder does not exist.")
          }
        } else {
          stop("Point cloud folder input is NULL.")
        }
      }, error = function(e) {
        message("No Point cloud Plot data available. Please upload a file.")
        #output$rasterPlotDSM_file_ind2 <- renderText(paste0("Point cloud figure"))
        showNotification(paste("No LAZ soil plot data available. Please upload a file."), type = "error")
        
        return(NULL)
      })
    }) 
  }) 
  
  
  # Render ind plot2 - PC
  output$PlotLaz_ind2_s <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
        if (!is.null(input$laz_folder)) {
          if (dir.exists(input$laz_folder)) {
            
            imgFiles.laz.dtm <-list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) #get the DTM
            cloud_s <- readLAS(files = imgFiles.laz.dtm, select = "xyz")
            cloud <- readLAS(files = input$flight_select_laz, select = "xyz") 
            #k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for p
            k <- selected_k() 
            p <- indPlots_2()[k,]
            p_sp <- as(p, "Spatial")
            c_soil <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                    ytop = p_sp@bbox['y','max'], 
                                    xright = p_sp@bbox['x','max'], 
                                    ybottom = p_sp@bbox['y','min'])
            

            # Remove outliers in c_soil
            mean_soil <- mean(c_soil@data$Z)
            std_soil <- sd(c_soil@data$Z)
            c_soil <- c_soil[c_soil@data$Z >= mean_soil - kout * std_soil & c_soil@data$Z <= mean_soil + kout * std_soil,]
            
            # Set up plot region to arrange p1 and p2 vertically
            par(mfrow=c(2,1))
            
            # Compute the average value of Z across Y
            avg_Zy <- tapply(c_soil@data$Z, c_soil@data$Y, mean)
            plot(avg_Zy, type="l", xlab="Lateral view", ylab="EPH", main = "Selected Plot - Average point cloud distribution")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z),
                              "median" = median(c_soil@data$Z),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil)),
                   col = "red")
            
            # Compute the average value of Z across Y
            avg_Zx <- tapply(c_soil@data$Z, c_soil@data$X, mean)
            plot(avg_Zx, type="l", xlab="Frontal view", ylab="EPH")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z),
                              "median" = median(c_soil@data$Z),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil)),
                   col = "blue")
            
            
            output$PlotLaz_ind_file2_s <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
            
          } else {
            stop("Point cloud folder does not exist.")
          }
        } else {
          stop("Point cloud folder input is NULL.")
        }
      }, error = function(e) {
        message("No Point cloud Plot data available. Please upload a file.")
        #output$rasterPlotDSM_file_ind2 <- renderText(paste0("Point cloud figure"))
        
        return(NULL)
      })
    }) 
  })
     

  ## Load the shapefile files - Field area DSM
  observeEvent(input$shapefile_button_clip.dsm, {
    shapefile_path_clip <- tryCatch(file.choose(),
                                    error = function(e) NULL)
    if (!is.null(shapefile_path_clip)) {
      if (endsWith(shapefile_path_clip, ".shp")) {
        shapefile_path_clip_global.dsm<<- shapefile_path_clip
        shapefile_name_clip.dsm <- basename(shapefile_path_clip)
        indPlots_clip <- st_read(shapefile_path_clip)
        output$shapefile_name_clip.dsm <- DT::renderDataTable(DT::datatable(indPlots_clip, options = list(pageLength = 5, scrollX = T)))
      } else {
        output$shapefile_name_clip.dsm <- renderText("Invalid file type! Please select a shapefile (.shp).")
      }
    } else {
      output$shapefile_name_clip.dsm <- renderText("Invalid shapefile file path!\n")
    }
  })   
  

## Load the shapefile files - Field area PC
observeEvent(input$shapefile_button_clip.pc, {
  shapefile_path_clip <- tryCatch(file.choose(),
                             error = function(e) NULL)
  if (!is.null(shapefile_path_clip)) {
    if (endsWith(shapefile_path_clip, ".shp")) {
      shapefile_path_clip_global<<- shapefile_path_clip
      shapefile_name_clip.pc <- basename(shapefile_path_clip)
      indPlots_clip <- st_read(shapefile_path_clip)
      output$shapefile_name_clip.pc <- DT::renderDataTable(DT::datatable(indPlots_clip, options = list(pageLength = 5, scrollX = T)))
    } else {
      output$shapefile_name_clip.pc <- renderText("Invalid file type! Please select a shapefile (.shp).")
    }
  } else {
    output$shapefile_name_clip.pc <- renderText("Invalid shapefile file path!\n")
  }
})   
  

## Load the PC files - field area DSM
observeEvent(input$dsm_folder_area, {
  if (!is.null(input$dsm_folder_area)) {
    if (dir.exists(input$dsm_folder_area)) {
      dsm_folder_name_area <- basename(input$dsm_folder_area)
      imgFiles.dsm_area <-list.files(path = input$dsm_folder_area, pattern="*.tif$",full.names = T) #get the laz only 
      imgFiles.dsm_area <- sapply(imgFiles.dsm_area, function(x) gsub(".tif", "", basename(x)))
      cat("Selected DSM vegetation folder:", dsm_folder_name_area, "\n")
      #imgFiles.laz.list.area <- as.list(imgFiles.dsm_area)
      #imgFiles.laz.list.area <<- sapply(imgFiles.laz.list.area, function(x) gsub(".laz", "", basename(x)))
      output$dsm_folder_name_area <- renderText(paste0("Selected DSM field area: ", dsm_folder_name_area, "\n"))
      output$imgFiles.dsm_area <- renderText(paste0("Selected DSM files: ", imgFiles.dsm_area, "\n"))
    } else {
      cat("Invalid DSM vegetation folder path!\n")
      output$dsm_folder_name_area <- renderText("Invalid DSM vegetation folder path!")
      output$imgFiles.dsm_area <- renderText("Invalid DSM vegetation files path!")
    }
  }
})


## Load the PC files - field area PC
observeEvent(input$laz_folder_area.pc, {
  if (!is.null(input$laz_folder_area.pc)) {
    if (dir.exists(input$laz_folder_area.pc)) {
      laz_folder_name_area <- basename(input$laz_folder_area.pc)
      imgFiles.laz_area.pc <-list.files(path = input$laz_folder_area.pc, pattern="*.laz$",full.names = T) #get the laz only 
      imgFiles.laz_area.pc <- sapply(imgFiles.laz_area.pc, function(x) gsub(".laz", "", basename(x)))
      cat("Selected LAZ vegetation folder:", laz_folder_name_area, "\n")
      #imgFiles.laz.list.area <- as.list(imgFiles.laz_area.pc)
      #imgFiles.laz.list.area <<- sapply(imgFiles.laz.list.area, function(x) gsub(".laz", "", basename(x)))
      output$laz_folder_name_area <- renderText(paste0("Selected LAZ field area: ", laz_folder_name_area, "\n"))
      output$imgFiles.laz_area.pc <- renderText(paste0("Selected LAZ files: ", imgFiles.laz_area.pc, "\n"))
    } else {
      cat("Invalid LAZ vegetation folder path!\n")
      output$laz_folder_name_area <- renderText("Invalid LAZ vegetation folder path!")
      output$imgFiles.laz_area.pc <- renderText("Invalid LAZ vegetation files path!")
    }
  }
})


## Shapefile to use in the loop to Clipping Point Cloud
indPlots_2_area <- reactive({
  if (is.null(shapefile_path_clip_global)) {
    return(NULL)
  } else {
    st_read(shapefile_path_clip_global) 
  }
})

## Shapefile to use in the loop to Clipping DSM
indPlots_3_area <- reactive({
  if (is.null(shapefile_path_clip_global.dsm)) {
    return(NULL)
  } else {
    st_read(shapefile_path_clip_global.dsm) 
  }
})


### Clipping DSM
img_laz_area.dsm <- reactive({
  if(is.null(input$dsm_folder_area)) {return("No data found")}
  
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Running...")
    
    clipper.area.dsm(
      dsm_list =  imgFiles.dsm_area(),
      shape = indPlots_3_area(),
      n.core=n.core3()
    )
    
  })
})


### Clipping Point Cloud 
img_laz_area.pc <- reactive({
  if(is.null(input$laz_folder_area.pc)) {return("No data found")}
  
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Running...")
    
    clipper.cloud.area.pc(
      cloud_list =  imgFiles.laz_area.pc(),
      shape = indPlots_2_area(),
      n.core=n.core2()
    )
    
  })
})


## Clip DSM function
observeEvent(input$Clip_DSM, {
  
  if(is.null(input$dsm_folder_area)) {return("No data found")}
  img_laz_area.dsm()
})

## Clip point cloud function
observeEvent(input$Clip_PC, {
  
  if(is.null(input$laz_folder_area.pc)) {return("No data found")}
  img_laz_area.pc()
})



}    
    
    
# Run shiny app ---------------------------------------------------------------------------
shinyApp(ui, server)


