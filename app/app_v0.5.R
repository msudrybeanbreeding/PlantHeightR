# SHINY DASHBOARD - PlantHeight_R =========================================================================

## A tool to estimate the plant height using the digital surface model (DSM) and Point Cloud (PC)

## Tab package =========================================================================

### Packages library =================================================
library("shiny")
library("shinyFiles")
library("shinydashboard")
library("dashboardthemes")
library("shinyjs")
library("rgl") 
#install.packages("glue")

library("data.table") 
library("DT") 
library("readr") 
library("inspectdf") 
library("sp") 
library("terra") 
library("raster") 
library("ggplot2") 
library("sf") 
library("doParallel") 
library("parallel") 
library("foreach") 
library("rlas") 
library("lidR") 
library("rasterVis") 
library("devtools") 
library("utils")
# install.packages("devtools") 
# devtools::install_github("OpenDroneMap/FIELDimageR")

options(shiny.maxRequestSize=100000*1024^2)

packages_to_check <- c(  "data.table", "DT","readr", "inspectdf", "sp", "terra",
                        "raster", "ggplot2", "sf", "doParallel", "parallel", "foreach",
                        "rlas", "lidR", "rasterVis", "devtools", "utils")


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
    useShinyjs(), # enable the use of ShinyJS 
    titlePanel("Cropping image"),
    h4("Reducing ohomosaic or Point cloud data by the field polygon shapefile"),
    mainPanel(
      style = "width: 600px;",
      tabsetPanel(
        id = "tabs",
        type = "tabs",
         tabPanel("DSM",
                  br(),
                  
                  checkboxInput("dsm_folder_check_area", "Upload dataset", value = FALSE),
                  br(),
                  
                  conditionalPanel(
                    condition = "input.dsm_folder_check_area == false",
                    textInput(
                      inputId = "dsm_folder_area",
                      label = "Enter DSM folder path (.tif):"
                    )),
                  conditionalPanel(
                    condition = "input.dsm_folder_check_area == true",
                    fileInput("dsm_upload_area", "Upload DSM File area (.tif)",  accept = c(".tif"), multiple = TRUE)
                  ),
                  
                  textOutput(outputId = "dsm_folder_name_area"),
                  textOutput(outputId = "imgFiles.dsm_area"),
                  
                  conditionalPanel(
                    condition = "input.dsm_folder_check_area == false",
                    actionButton("shapefile_button_clip.dsm", "Select Shapefile field area (.shp)", icon = icon("file"))
                  ),
                  conditionalPanel(
                    condition = "input.dsm_folder_check_area == true",
                    fileInput("shapefile_button_clip.dsm_upload", "Upload Shapefile field area (.zip)", accept = c(".zip"))
                  ),
                  
                  br(),
                  textOutput("shapefile_name_clip.dsm"),
                  h4("Shapefile features"),
                  DT::dataTableOutput('contents_shape2'),
                  br(),
                  br(),

                  selectInput("nameID",
                              "Select the File name:",
                              choices = NULL),
                  textOutput("file_name"),
                  br(),
                  br(),
                  
                  actionButton("n.core_button3", "Check cores"),
                  textOutput(outputId = "n.core3"),
                  uiOutput("core.thresh.ui3"),
                  br(),

                  actionButton("Clip_DSM", "RUN DSM snipping"),
                  h4("Snipping DSM using field shp area and saving to folder"),
                  
                  br(),
                  
                  conditionalPanel(
                    condition = "input.dsm_folder_check_area == true",
                    downloadButton("downloadClippedDSM", "Download Clipped DSM", class = "btn btn-primary", disabled = TRUE)
                    
                  )
                  
                  
         ),
         
         tabPanel("Point Cloud",
                  br(),
                  
                  checkboxInput("laz_folder_check_area", "Upload dataset", value = FALSE),
                  br(),
                  
                  conditionalPanel(
                    condition = "input.laz_folder_check_area == false",
                    textInput(
                      inputId = "laz_folder_area.pc",
                      label = "Enter LAZ folder path (.laz):"
                    )),
                  
                  conditionalPanel(
                    condition = "input.laz_folder_check_area == true",
                    fileInput("laz_folder_area.pc_upload", "Upload LAZ File area (.laz)", accept = c(".laz"), multiple = TRUE)
                  ),
                  
                  textOutput(outputId = "laz_folder_name_area"),
                  textOutput(outputId = "imgFiles.laz_area.pc"),
                  
                  conditionalPanel(
                    condition = "input.laz_folder_check_area == false",
                    actionButton("shapefile_button_clip.pc", "Select Shapefile field area (.shp)", icon = icon("file"))
                  ),
                  conditionalPanel(
                    condition = "input.laz_folder_check_area == true",
                    fileInput("shapefile_button_clip.pc_upload", "Upload Shapefile field area (.zip)", accept = c(".zip"))
                  ),
                  
                  br(),
                  textOutput("shapefile_name_clip.pc"),
                  h4("Shapefile features"),
                  DT::dataTableOutput('contents_shape3'),

                  br(),
                  br(),
                  
                  selectInput("nameID_pc",
                              "Select the File name PC:",
                              choices = NULL),
                  textOutput("file_name_pc"),
                  br(),
                  br(),
                  
                  actionButton("n.core_button2", "Check cores"),
                  textOutput(outputId = "n.core2"),
                  uiOutput("core.thresh.ui2"),
                  br(),
                  
                  actionButton("Clip_PC", "RUN Point Cloud snipping"),
                  h4("Snipping Point Cloud using field shp area and saving to folder"),
                  
                  conditionalPanel(
                    condition = "input.laz_folder_check_area == true",
                    downloadButton("downloadClippedLAZ", "Download Clipped LAZ Files", class = "btn btn-primary", disabled = TRUE)
                  )
         )
      )
      ),
    
    conditionalPanel(
      condition = "input.tabs == 'DSM'",
      
      sidebarPanel(
        style = "width: 850px;",
        titlePanel("Field DSM - Clip area"),
        plotOutput("rasterPlotDSM_area", width = "800px", height = "600px")
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'Point Cloud'",
      sidebarPanel(
        style = "width: 850px;",
        
        titlePanel("Field Point Cloud - Clip area"),
        plotOutput("rasterPlotPC_area", width = "800px", height = "600px")
      )
    )
      
  )
)

### Upload file tab ----------------------------------------------

    upload_tab <- tabItem(
      tabName = "FileUpload",
      fluidPage(
        useShinyjs(), # enable the use of ShinyJS 
        titlePanel("Phenotyping dataset"),
        mainPanel(
          tabsetPanel(
            tabPanel("DSM data",
                     br(),
                     
       checkboxInput("dsm_folder_check", "Upload DSM file", value = FALSE),
       
       conditionalPanel(
         condition = "input.dsm_folder_check == false",
          textInput(
            inputId = "dsm_folder",
            label = "Enter DSM folder path:"
          )),
         conditionalPanel(
           condition = "input.dsm_folder_check == true",
           fileInput("dsm_upload", "Upload DSM File", accept = c(".tif"), multiple = TRUE)
         ),
         
          textOutput(outputId = "dsm_folder_name"),
          textOutput(outputId = "imgFiles.dsm"),
        ),
        tabPanel("DTM data",
                 br(),
                 checkboxInput("dtm_folder_check", "Upload DTM file", value = FALSE),
                 
                 conditionalPanel(
                   condition = "input.dtm_folder_check == false",
                   textInput(
                     inputId = "dtm_folder",
                     label = "Enter DTM folder path:"
                   )),
                 conditionalPanel(
                   condition = "input.dtm_folder_check == true",
                   fileInput("dtm_upload", "Upload DTM File", accept = c(".tif"), multiple = FALSE)
                 ),
                 
                 textOutput(outputId = "dtm_folder_name"),
                 textOutput(outputId = "imgFiles.dtm"),
        ),
        tabPanel("LAZ veg data",
                 br(),
            checkboxInput("laz_folder_check", "Upload LAZ file", value = FALSE),
                 
            conditionalPanel(
              condition = "input.laz_folder_check == false",
              textInput(
                inputId = "laz_folder",
                label = "Enter LAZ folder path:"
              )),
            conditionalPanel(
              condition = "input.laz_folder_check == true",
              fileInput("laz_upload", "Upload LAZ File", accept = c(".laz"), multiple = TRUE)
            ),
          
          textOutput(outputId = "laz_folder_name"),
          textOutput(outputId = "imgFiles.laz")
        ),
        
          tabPanel("LAZ soil data",
                   br(),
                   checkboxInput("laz_folder_soil_check", "Upload LAZ soil file", value = FALSE),
                   conditionalPanel(
                     condition = "input.laz_folder_soil_check == false",
                     textInput(
                       inputId = "laz_folder.soil",
                       label = "Enter LAZ soil folder path:"
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.laz_folder_soil_check == true",
                     fileInput("laz_upload_soil", "Upload LAZ soil File", accept = c(".laz"), multiple = FALSE)
                   ),
          
          textOutput(outputId = "laz_folder_name_soil"),
          textOutput(outputId = "imgFiles.laz.soil")
        ),
        
       tabPanel("Shapefile plots",
                br(),
                checkboxInput("shapefile_upload_check", "Upload Shapefile", value = FALSE),
                conditionalPanel(
                  condition = "input.shapefile_upload_check == false",
                  actionButton("shapefile_button", "Select Shapefile", icon = icon("file"), disabled = TRUE)
                ),
                conditionalPanel(
                  condition = "input.shapefile_upload_check == true",
                  fileInput("shapefile_upload", "Upload Shapefile (.zip)", accept = c(".zip"))
                ),
                textOutput(outputId = "shapefile_name"),
                selectInput("plotID",
                            "Select the Plot ID name:",
                            choices = NULL),
                verbatimTextOutput(outputId = "indPlotsName"),
                
                br(),
                h4("Shapefile features"),
                
                DT::dataTableOutput('contents'),
                
                sidebarPanel(
                  br(),
                  DT::dataTableOutput("indPlots_col_plot")
                )
                
        )
      )


    )
    )
    )


### Plant Height ------------------------------------------------------

EPH_tab <- tabItem(
  tabName = "EPH",
  fluidPage(
    useShinyjs(), # enable the use of ShinyJS 
    titlePanel("Plant Height Estimation"),
    sidebarPanel(
      radioButtons("PH_engine",
                   "Select engine method:",
                   choices = c("CSM (DSM - DTM)", "Point Cloud")),
      checkboxInput("dsm_single", "Analysis based solely on DSM or Vegetation Point Cloud?"),
      
      conditionalPanel(
        condition = "input.dsm_single == true",
        
        numericInput("qthreshSoil_single", "Assign quantile threshold to soil:",
                     min = 0.01, max = 99.9, step = 0.1, value = NULL),
        
        conditionalPanel(
          condition = "output.showWarning",
          div(class = "alert alert-warning", "Value = 0. Please enter a value for Soil Quantile threshold")
        ),
        
        radioButtons("analysis_type_soil",
                     "Select extraction analysis to soil:",
                     choices = c("mean", "median", "quantile"), selected = "median"),
        conditionalPanel(
          condition = "input.analysis_type_soil.includes('quantile')",
          numericInput("qthresh_s", "Soil Quantile threshold:",
                       min = 0.01, max = 99.9, step = 0.1, value = NULL),
        ),
      ),
      
      conditionalPanel(
        condition = "input.PH_engine.includes('Point Cloud') && input.dsm_single == false",
        numericInput("qthreshSoil", "Soil Quantile threshold:",
                     min = 0.01, max = 99.9, step = 0.1, value = NULL),
      ),
      br(),
      
      selectInput("method",
                  "Select analysis method:",
                  choices = c("Time-Series Data", "Ind. Data Flight"), 
                  selected = "Ind. Data Flight"),
      
      # Dynamic UI for Ind. Data Flight
      uiOutput("ind_data_flight_ui"),
      uiOutput("ind_data_flight_ui_laz"),

      radioButtons("analysis_type",
                   "Select analysis type:",
                   choices = c("mean", "median", "quantile"), selected = "median"),
      
      conditionalPanel(
        condition = "input.analysis_type.includes('quantile')",
        numericInput("qthresh", "Vegetation Quantile threshold:",
                    min = 0.01, max = 99.9, step = 0.1, value = NULL),
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
      DT::dataTableOutput('met_ts_img_laz.table'),
      DT::dataTableOutput('met_ind_img_laz.table'),
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
          
          rglwidgetOutput("PlotLaz_ind_3d"),
          textOutput(outputId = "PlotLaz_ind_file_3d"),
          
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
    showNotification("The data does not intersect the shapefile.", type = "warning", duration = NULL)
    message(paste("Error in overlay_data:", e))
  })
}


shp_test_crs<- function(data, shp){
  tryCatch({
    if(is.na(sf::st_crs(data))) {
      sf::st_crs(shp) <- NA
      message("shapefileshave been adjusted to NA CRS.")

    } else if(!is.na(sf::st_crs(shp)$epsg)) {
      epsg_value<- sf::st_crs(shp)$epsg
      raster::projection(data) <- epsg_value
      message("CRS DSM have been adjusted using shpfile CRS. ")

    } else if(!is.na(raster::projection(shp))) {
      projection_value<- raster::projection(shp)
      raster::projection(data) <- projection_value
      message("CRS projection DSM have been adjusted using shpfile CRS projection. ")
   
    } else if(!isTRUE(sf::st_crs(shp) == sf::st_crs(data))) {
      stop("The CRS for DSM must be the same as the CRS for shpfiles")
    }
    
    # Return the DSM and shpfile
    return(list(data = data, shp = shp))
    
  },
  error = function(e) {
    showNotification(paste("Error in shp_test_crs:", e), type = "error")
    showNotification("The CRS for data must be the same as the CRS for shpfiles", type = "error", duration = NULL)
    message(paste("Error in shp_test_crs:", e))
  })
}

shp_test_crs_pc<- function(data, shp){
  tryCatch({
    if(is.na(lidR::st_crs(data))) {
      lidR::st_crs(shp) <- NA
      message("shapefiles have been adjusted to NA CRS.")

    } else if(!is.na(lidR::st_crs(shp)$epsg)) {
      epsg_value<- lidR::st_crs(shp)$epsg
      lidR::projection(data) <- epsg_value
      message("CRS Point Cloud have been adjusted using shpfile CRS. ")
  
    } else if(!is.na(lidR::projection(shp))) {
      projection_value<- lidR::projection(shp)
      lidR::projection(data) <- projection_value
      message("CRS projection Point Cloud have been adjusted using shpfile CRS projection. ")
 
    } else if(!isTRUE(lidR::st_crs(shp) == lidR::st_crs(data))) {
      stop("The CRS for Point Cloud must be the same as the CRS for shpfiles")

    }
    
     # Return the DSM and shpfile
    return(list(data = data, shp = shp))
    
  },
  error = function(e) {
    showNotification(paste("Error in shp_test_crs_pc:", e), type = "error")
    showNotification("The CRS for data must be the same as the CRS for shpfiles", type = "error", duration = NULL)
    message(paste("Error in shp_test_crs_pc:", e))
  })
}

PlantHeigh_est_ind<- function(imgFiles.dtm, dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh) {

  shape <- indPlots
  
  for(c in 1:length(dataset_img_sel)){

    DSM <- stack(dataset_img_sel[c])
    overlay_data(DSM, shape)
  }

  for(c in 1:length(imgFiles.dtm)){ 
    
    DSM0 <- stack(imgFiles.dtm[c])
    overlay_data(DSM0, shape)
  }
  
    DSM0 <- stack(imgFiles.dtm)
    DSM1 <- stack(dataset_img_sel) #ind. DSM selected by the user
    
    if (!isTRUE(sf::st_crs(DSM0) == sf::st_crs(DSM1))) {
      showNotification("The CRS for DSM and DTM must be the same", type = "error")
      message("The CRS for DSM and DTM must be the same")
      return(NULL)
    } else {
      message("The CRS for DSM and DTM are the same")
    }
    
    if (isTRUE(identical(DSM0, DSM1))) {
      showNotification("DSM and DTM are identical files", type = "error")
      message("DSM and DTM are identical files")
      return(NULL)
    } else {
      message("DSM and DTM are NOT identical files")
    }
    
    setProgress(message = "Running EPH analysis", detail = "")
    
    crs.result0 <- shp_test_crs(DSM0, shape)
    DSM0 <- crs.result0$data
    shape <- crs.result0$shp
    
    crs.result <- shp_test_crs(DSM1, shape)
    DSM1 <- crs.result$data
    shape <- crs.result$shp

    # Starting parallel
    cl <- makeCluster(n.core, output = "")
    registerDoParallel(cl)
    getDoParWorkers()
    
    message("Processing DSM: ",paste(dataset_img_sel))

    tryCatch({
    results<- foreach(p = 1:nrow(shape),  #loop through plots numbers
                      .packages = c("raster", "sf"),
                      .combine = rbind) %dopar% {

                        DSM1.c <-  crop(DSM1, st_bbox(shape[p,]))
                        
                        DSM0.c <-  crop(DSM0, st_bbox(shape[p,]))
                        
                       # Canopy Surface Model (CSM):
                        DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                        csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                        #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                        csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                         
                        Q1_DSM <- quantile(csm, probs = 0.25, na.rm=TRUE)
                        Q3_DSM <- quantile(csm, probs = 0.75, na.rm=TRUE)
                        IQR_DSM <- Q3_DSM - Q1_DSM
                        
                        # Define the lower and upper bounds for outliers
                        kout = 1.5
                        lower_bound <- Q1_DSM - kout * IQR_DSM
                        upper_bound <- Q3_DSM + kout * IQR_DSM
                        
                        # Remove outliers in DSM1.c using overlay() function
                        csm <- overlay(csm, fun = function(x) {
                          x[x < lower_bound | x > upper_bound] <- NA
                          return(x)
                        })
                        
                          # Extracting the estimate plant height average (EPH):
                        if (ExtMet == "mean") {
                          EPH <- mean(csm, na.rm=TRUE)

                        } else if (ExtMet == "median"){
                          EPH <- median(csm, na.rm=TRUE)

                        } else if (ExtMet == "quantile"){
                          EPH <- quantile(csm, qthresh)

                        }


                      }
    },
    error = function(e) {
      showNotification(paste("Error in PlantHeigh_est_ind:", e), type = "error")
      message(paste("Error in PlantHeigh_est_ind:", e))
      #stop(paste("Error in PlantHeigh_est_ind:", e))
    }
    )

    parallel::stopCluster(cl) # Stopping the parallel function

    if (ExtMet == "quantile"){
     colnames(results) <- paste0("EPH_", ExtMet, "_", qthresh)
    } else {
      colnames(results) <- paste0("EPH_", ExtMet)
    }

    # Convert results to a data frame
    results_df <- as.data.frame(results)

    # Add row names from Plot_ID as a new column to results data frame
    results_df <- cbind(Plot_ID, results_df)

    results_df$DSM_data<-sapply(imgFiles.dsm.names, function(x) gsub(".tif", "", basename(x)))

    # Assign row names
    row.names(results_df) <- paste0(1:nrow(results_df))

    # show a message when the function has finished running
    showNotification("Finished processing DSM", type = "message", duration = NULL)
    message("Finished processing DSM")
    
    return(results_df)

}


PlantHeigh_est_ts<- function(imgFiles.dtm, dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh) {
      
  tryCatch({
    if (is.null(dataset_img_sel) || is.null(imgFiles.dtm)) {
      stop("No DSM or DTM files provided")
    }
  },
  error = function(e) {
    showNotification(paste("No data set provided", e), type = "error")
    message(paste("No data set provided", e))
  }
  )
  
  shape <- indPlots
  
  for(c in 1:length(dataset_img_sel)){ 
    
    DSM <- stack(dataset_img_sel[c])
    overlay_data(DSM, shape)
  }
  
  for(c in 1:length(imgFiles.dtm)){ 
    
    DSM0 <- stack(imgFiles.dtm[c])
    overlay_data(DSM0, shape)
  }
  
      j=0
      setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
      
      message("Processing DTM: ",paste(imgFiles.dtm))
      
      DSM0 <- stack(imgFiles.dtm)
      

      
      crs.result0 <- shp_test_crs(DSM0, shape)
      DSM0 <- crs.result0$data
      shape <- crs.result0$shp
      
    for(i in 1:length(dataset_img_sel)){ #loop through images
      
      message("Processing DSM: ",paste(dataset_img_sel[i]))
      
      DSM1 <- stack(dataset_img_sel[i])
      
      if (!isTRUE(sf::st_crs(DSM0) == sf::st_crs(DSM1))) {
        showNotification("The CRS for DSM and DTM must be the same", type = "error")
        message("The CRS for DSM and DTM must be the same")
        return(NULL)
      } else {
        message("The CRS for DSM and DTM are the same")
      }
      
      if (isTRUE(identical(DSM0, DSM1))) {
        showNotification("DSM and DTM are identical files", type = "error")
        message("DSM and DTM are identical files")
        return(NULL)
      } else {
        message("DSM and DTM are NOT identical files")
      }
      
      crs.result <- shp_test_crs(DSM1, shape)
      DSM1 <- crs.result$data
      shape <- crs.result$shp
      
      # Starting parallel
      cl <- makeCluster(n.core, output = "")
      registerDoParallel(cl)
      getDoParWorkers()
      
      tryCatch(
      results<- foreach(p = 1:nrow(shape),  #loop through plots numbers
                        .packages = c("raster", "sf"), 
                        .combine = rbind) %dopar% {
                         
                          DSM1.c <-  crop(DSM1, st_bbox(shape[p,]))
                          
                          DSM0.c <-  crop(DSM0, st_bbox(shape[p,]))
                          
                        # Canopy Surface Model (CSM):
                          DSM0.r <-resample(DSM0.c, DSM1.c) # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
                          csm <- DSM1.c-DSM0.r # same results compared to the bottom line
                          #csm <- overlay(DSM1.c, DSM0.r, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
                          csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
                          
                          Q1_DSM <- quantile(csm, probs = 0.25, na.rm=TRUE)
                          Q3_DSM <- quantile(csm, probs = 0.75, na.rm=TRUE)
                          IQR_DSM <- Q3_DSM - Q1_DSM
                          
                          # Define the lower and upper bounds for outliers
                          kout = 1.5
                          lower_bound <- Q1_DSM - kout * IQR_DSM
                          upper_bound <- Q3_DSM + kout * IQR_DSM
                          
                          # Remove outliers in DSM1.c using overlay() function
                          csm <- overlay(csm, fun = function(x) {
                            x[x < lower_bound | x > upper_bound] <- NA
                            return(x)
                          })
                          
                          
                          # Extracting the estimate plant height average (EPH):
                          if (ExtMet == "mean") {
                            EPH <- mean(csm, na.rm=TRUE)
                            
                          } else if (ExtMet == "median"){
                            EPH <- median(csm, na.rm=TRUE)
                            
                          } else if (ExtMet == "quantile"){
                            EPH <- quantile(csm, qthresh)

                          }
                          
                          
                        },
      error = function(e) {
        showNotification(paste("Error in PlantHeigh_est_ts:", e), type = "error")
        message(paste("Error in PlantHeigh_est_ts:", e))
      }
      )
      
      parallel::stopCluster(cl) # Stopping the parallel function
      
      if (ExtMet == "quantile"){
        colnames(results) <- paste0("EPH_", ExtMet, "_", qthresh)
      } else {
        colnames(results) <- paste0("EPH_", ExtMet)
      }
      
      
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
      
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message", duration = NULL)
  message("Finished processing DSM")
  
  return(results_df.1)
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ind <- function(cloud, cloud_s, shape, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names, n.core) {
  
  tryCatch({
    if (is.null(cloud) || is.null(cloud_s)) {
      stop("No DSM or DTM files provided")
    }
  },
  error = function(e) {
    showNotification(paste("No LAZ soil files provided", e), type = "error")
    message(paste("No LAZ soil files provided", e))
  }
  )

  # loading the LAS files
  cloud_soil <- readLAS(files = cloud_s, select = "xyz")
  cloud_veg <- readLAS(files = cloud, select = "xyz")
  
  if (!isTRUE(lidR::st_crs(cloud_soil) == lidR::st_crs(cloud_veg))) {
    showNotification("The CRS for Point Cloud data must be the same", type = "error")
    message("The CRS for Point Cloud must be the same")
    return(NULL)
  } else {
    message("The CRS for Point Cloud data are the same")
  }
  
  if (isTRUE(identical(cloud_soil, cloud_veg))) {
    showNotification("Point Cloud data are identical", type = "error")
    message("Point Cloud data are identical")
    return(NULL)
  } else {
    message("Point Cloud data are NOT identical")
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  
  crs.result0 <- shp_test_crs_pc(cloud_soil, shape)
  cloud_soil <- crs.result0$data
  shape <- crs.result0$shp
  
  crs.result <- shp_test_crs_pc(cloud_veg, shape)
  cloud_veg <- crs.result$data
  shape <- crs.result$shp
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  message("Processing Point Cloud: ",paste(cloud))
  
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
                       
                       # Remove outliers in c_veg using IQR
                       # Interquartile Range (IQR)
                       Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
                       Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
                       IQR_veg <- Q3_veg - Q1_veg
                       kout <- 1.5
                       c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
                       
                       # Remove outliers in c_soil using IQR
                       Q1_soil <- quantile(c_soil@data$Z, 0.25, na.rm=TRUE)
                       Q3_soil <- quantile(c_soil@data$Z, 0.75, na.rm=TRUE)
                       IQR_soil <- Q3_soil - Q1_soil
                       c_soil <- c_soil[c_soil@data$Z >= Q1_soil - kout * IQR_soil & c_soil@data$Z <= Q3_soil + kout * IQR_soil,]
                       
                       # Extracting the estimate plant height average (EPH):
                       if (ExtMet == "mean") {
                         EPH1 <- mean(c_veg@data$Z, na.rm=TRUE)
                         EPH2 <- mean(c_soil@data$Z, na.rm=TRUE)
                         
                       } else if (ExtMet == "median") {
                         EPH1 <- median(c_veg@data$Z, na.rm=TRUE)
                         EPH2 <- median(c_soil@data$Z, na.rm=TRUE)
                         
                        } else if (ExtMet == "quantile"){
                         
                          EPH1 <- quantile(c_veg@data$Z, qthresh, na.rm=TRUE)
                          EPH2 <- quantile(c_soil@data$Z, qthreshSoil)
                        }
                       
                       # Compute difference and round to two decimal places
                       EPH <- round(EPH1 - EPH2, 4)
                       
                     },
  error = function(e) {
    showNotification(paste("Error in clipper.cloud.est.ind:", e), type = "error")
    message(paste("Error in clipper.cloud.est.ind:", e))
  }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  if (ExtMet == "quantile"){
    colnames(results) <- paste0("EPH_soil_", qthreshSoil, "_", ExtMet, "_", qthresh )
  } else {
    colnames(results) <- paste0("EPH_", ExtMet)
  }
  
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$PointCloud_data<-sapply(cloud_names, function(x) gsub(".laz", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
 
  # show a message when the function has finished running
  showNotification("Finished processing LAZ", type = "message", duration = NULL)
  message("Finished processing Point Cloud")
  
  return(results_df)
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ts <- function(cloud_list, cloud_soil, shape, qthresh, qthreshSoil, ExtMet, Plot_ID, cloud_names, n.core) {
  
  tryCatch({
    if (is.null(cloud_list) || is.null(cloud_soil)) {
      stop("No DSM or DTM files provided")
    }
  },
  error = function(e) {
    showNotification(paste("No LAZ soil files provided", e), type = "error")
    message(paste("No LAZ soil files provided", e))
  }
  )
  
  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  # loading the LAS files
  cloud_s <- readLAS(files = cloud_soil, select = "xyz")
  
  crs.result0 <- shp_test_crs_pc(cloud_s, shape)
  cloud_s <- crs.result0$data
  shape <- crs.result0$shp

  for(i in 1:length(cloud_list)){ #loop through images
    
    message("Processing Point Cloud - Vegetation: ",paste(cloud_list[[i]]))
    message("Processing Point Cloud - Soil: ", paste(cloud_soil))
    
    # loading the LAS files
    cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
    
    if (!isTRUE(lidR::st_crs(cloud_s) == lidR::st_crs(cloud))) {
      showNotification("The CRS for Point Cloud data must be the same", type = "error")
      message("The CRS for Point Cloud must be the same")
      return(NULL)
    } else {
      message("The CRS for Point Cloud data are the same")
    }
    
    if (isTRUE(identical(cloud_s, cloud))) {
      showNotification("Point Cloud data are identical", type = "error")
      message("Point Cloud data are identical")
      return(NULL)
    } else {
      message("Point Cloud data are NOT identical")
    }
    
    crs.result <- shp_test_crs_pc(cloud, shape)
    cloud <- crs.result$data
    shape <- crs.result$shp
    
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
                         
                         # Remove outliers in c_veg using IQR
                         Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
                         Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
                         IQR_veg <- Q3_veg - Q1_veg
                         kout <- 1.5
                         c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
                         
                         # Remove outliers in c_soil using IQR
                         Q1_soil <- quantile(c_soil@data$Z, 0.25, na.rm=TRUE)
                         Q3_soil <- quantile(c_soil@data$Z, 0.75, na.rm=TRUE)
                         IQR_soil <- Q3_soil - Q1_soil
                         c_soil <- c_soil[c_soil@data$Z >= Q1_soil - kout * IQR_soil & c_soil@data$Z <= Q3_soil + kout * IQR_soil,]
                         
                         # Extracting the estimate plant height average (EPH):
                         if (ExtMet == "mean") {
                           EPH1 <- mean(c_veg@data$Z, na.rm=TRUE)
                           EPH2 <- mean(c_soil@data$Z, na.rm=TRUE)
                           
                         } else if (ExtMet == "median") {
                           EPH1 <- median(c_veg@data$Z, na.rm=TRUE)
                           EPH2 <- median(c_soil@data$Z, na.rm=TRUE)
                           
                         } else if (ExtMet == "quantile"){
                           
                           EPH1 <- quantile(c_veg@data$Z, qthresh, na.rm=TRUE)
                           EPH2 <- quantile(c_soil@data$Z, qthreshSoil, na.rm=TRUE)

                         }
                         
                         # Compute difference and round to two decimal places
                         EPH <- round(EPH1 - EPH2, 4)
                         
                       },
    error = function(e) {
      showNotification(paste("Error in clipper.cloud.est.ts:", e), type = "error")
      message(paste("Error in clipper.cloud.est.ts:", e))
    }
    )
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    
    if (ExtMet == "quantile"){
      colnames(results) <- paste0("EPH_", ExtMet, "_", qthresh, "_", qthreshSoil)
    } else {
      colnames(results) <- paste0("EPH_", ExtMet)
    }
    
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
  
  # show a message when the function has finished running
  showNotification("Finished processing LAZ", type = "message", duration = NULL)
  message("Finished processing Point Cloud")
  
  return(results_df.1)
  
}


# Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.area.pc<- function(cloud_list, shape, n.core) {
  
  if (length(cloud_list) == 0) {
    showNotification("No Point Cloud files provided", type = "error")
    return()
  }
  
  # filter out files with "_clip" in the name
 # cloud_list <- cloud_list[!grepl("_clip", cloud_list)]
  
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  clipped_laz_paths <-tryCatch(
  foreach(i = 1:length(cloud_list), 
          .packages = c("lidR", "sf"),
          .combine='c') %dopar% {
    
    #message("Processing Point Cloud - Clipping: ",paste(cloud_list[[i]]))
    
    # loading the LAS files
    cloud <- readLAS(files = cloud_list[[i]], select = "xyz")
    
    if(is.na(lidR::st_crs(cloud))) {
      lidR::st_crs(shape) <- NA
      message("Shapefiles have been adjusted to NA CRS.")
      shape
    } else if(!is.na(lidR::st_crs(shape)$epsg)) {
      epsg_value<- lidR::st_crs(shape)$epsg
      lidR::projection(cloud) <- epsg_value
      message("CRS Point Cloud have been adjusted using shapefile CRS. ")
      cloud
    } else if(!is.na(lidR::projection(shape))) {
      projection_value<- lidR::projection(shape)
      lidR::projection(cloud) <- projection_value
      message("CRS projection Point Cloud have been adjusted using shapefile CRS projection. ")
      cloud
    } else if(!isTRUE(lidR::st_crs(shape) == lidR::st_crs(cloud))) {
      stop("The CRS for Point Cloud must be the same as the CRS for shapefiles")
    }
    
    # clip the point cloud by the polygon shapefile
    clipped_las <- clip_roi(cloud, shape)
    
    # save the clipped point cloud to a new LAS file
    cloud_names <- sapply(cloud_list[i], function(x) gsub(".laz", "", x))
    writeLAS(clipped_las, paste0(cloud_names,"_clip.laz"))
    
    # Return the generated DSM file paths
    return(paste0(cloud_names, "_clip.laz"))

          },
  error = function(e) {
    showNotification(paste("Error in clipper.cloud.area.pc:", e), type = "error")
    message(paste("Error in clipper.cloud.area.pc:", e))
    return(character(0))
  }
  )
  
  stopCluster(cl)
  
  # show a message when the function has finished running
  showNotification("Finished processing point clouds", type = "message", duration = NULL)
  message("Finished processing point clouds")
  
  print(paste("Returning clipped_laz_paths:", clipped_laz_paths))
  return(clipped_laz_paths)
}


# Auxiliary function for clipping DSM by shapefile (features)
clipper.area.dsm <- function(dsm_list, shape, n.core) {
  
  if (length(dsm_list) == 0) {
    showNotification("No DSM files provided", type = "error")
    return(character(0))
    
  }
  
  for(c in 1:length(dsm_list)){ 
    
    DSM <- stack(dsm_list[c])
    overlay_data(DSM, shape)
  }
  
  # filter out files with "_clip" in the name
  # dsm_list <- dsm_list[!grepl("_clip", dsm_list)]
  
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers() 
  
  
  clipped_dsm_paths <- tryCatch(
    foreach(i = 1:length(dsm_list), 
            .packages = c("raster", "sf"),
            .combine='c') %dopar% {
              
              # message("Processing DSM - Clipping: ", paste(dsm_list[[i]]))
              
              # loading the DSM files
              DSM <- stack(dsm_list[[i]])
              
              if(is.na(sf::st_crs(DSM))) {
                sf::st_crs(shape) <- NA
                message("Shapefiles have been adjusted to NA CRS.")
                shape
              } else if(!is.na(sf::st_crs(shape)$epsg)) {
                epsg_value<- sf::st_crs(shape)$epsg
                raster::projection(DSM) <- epsg_value
                message("CRS DSM have been adjusted using shapefile CRS. ")
                DSM
              } else if(!is.na(raster::projection(shape))) {
                projection_value<- raster::projection(shape)
                raster::projection(DSM) <- projection_value
                message("CRS projection DSM have been adjusted using shapefile CRS projection. ")
                DSM
              } else if(!isTRUE(sf::st_crs(shape) == sf::st_crs(DSM))) {
                stop("The CRS for DSM must be the same as the CRS for shapefiles")
              }
              
              # clip the point cloud by the polygon shapefile
              DSM.c <-  crop(DSM, st_bbox(shape))
              
              # save the clipped point cloud to a new DSM file
              DSM.c_names <- sapply(dsm_list[i], function(x) gsub(".tif", "", x))
              #writeRaster(DSM.c, paste0(DSM.c_names,"_clip.tif"), format = "GTiff", overwrite=TRUE)

                clipped_dsm_path <- paste0(DSM.c_names, "_clip.tif")
                writeRaster(DSM.c, clipped_dsm_path, format = "GTiff", overwrite=TRUE)

              # Return the generated DSM file paths
              return(paste0(DSM.c_names, "_clip.tif"))
              
              
            },
    error = function(e) {
      showNotification(paste("Error in clipper.area.dsm:", e), type = "error")
      message(paste("Error in clipper.area.dsm:", e))
      return(character(0))
    }
  )
  
  stopCluster(cl)
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message", duration = NULL)
  message("Finished processing DSM")
  
  print(paste("Returning clipped_dsm_paths:", clipped_dsm_paths))
  return(clipped_dsm_paths)
}


PH_est_ind_single<- function(dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, ExtMet_soil, qthresh, qthresh_soil, qthreshsingle) {
  
  if (length(dataset_img_sel) == 0) {
    showNotification("No DSM files provided", type = "error")
    return()
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  
  shape <- indPlots
  
  for(c in 1:length(dataset_img_sel)){ 
    
    DSM <- stack(dataset_img_sel[c])
    overlay_data(DSM, shape)
  }
  
  DSM1 <- stack(dataset_img_sel) #ind. DSM selected by the user
  
  crs.result <- shp_test_crs(DSM1, shape)
  DSM1 <- crs.result$data
  shape <- crs.result$shp
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  message("Processing DSM: ",paste(dataset_img_sel))
  
  tryCatch(
    results<- foreach(p = 1:nrow(shape),  #loop through plots numbers
                      .packages = c("raster", "sf"), 
                      .combine = rbind) %dopar% {
                        
                        DSM1.c <-  crop(DSM1, st_bbox(shape[p,]))
                        DSM2.c <-  crop(DSM1, st_bbox(shape[p,]))
                        
                        Q1_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.25, na.rm=TRUE)
                        Q3_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.75, na.rm=TRUE)
                        IQR_DSM <- Q3_DSM - Q1_DSM

                        # Define the lower and upper bounds for outliers
                        kout = 1.5
                        lower_bound <- Q1_DSM - kout * IQR_DSM
                        upper_bound <- Q3_DSM + kout * IQR_DSM

                        # Remove outliers in DSM1
                        DSM1.c <- DSM1.c[DSM1.c >= lower_bound & DSM1.c <= upper_bound,]
                        DSM2.c <- DSM2.c[DSM2.c >= lower_bound & DSM2.c <= upper_bound,]
                        
                        csm_rem <- quantile(DSM1.c,qthreshsingle, na.rm=TRUE)

                        DSM1.c[DSM1.c <= csm_rem] = NA
                        DSM2.c[DSM2.c > csm_rem] = NA
                        
                         # Extracting the estimate plant height average (EPH):
                        if (ExtMet == "mean") {
                          EPH1 <- mean(DSM1.c, na.rm=TRUE)
                         }
                              
                        if( ExtMet_soil == "mean") {
                          EPH2 <- mean(DSM2.c, na.rm=TRUE)
                         } 
                              
                        if (ExtMet == "median"){
                          EPH1 <- median(DSM1.c, na.rm=TRUE)
                        } 
                              
                        if( ExtMet_soil == "median") {
                          EPH2 <- median(DSM2.c, na.rm=TRUE)
                         } 
                              
                        if (ExtMet == "quantile"){
                          EPH1 <- quantile(DSM1.c,qthresh, na.rm=TRUE)
                        } 
                              
                       if (ExtMet_soil == "quantile"){
                          EPH2 <- quantile(DSM2.c,qthresh_soil, na.rm=TRUE)

                        }
                        
                        EPH <- round(EPH1 - EPH2, 4)
                        
                      },
    error = function(e) {
      showNotification(paste("Error in PH_est_ind_single:", e), type = "error")
      message(paste("Error in PH_est_ind_single:", e))
    }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  # 
  if (ExtMet == "quantile" && ExtMet_soil == "quantile"){
    colnames(results) <- paste0("EPH_", qthreshsingle,"_", ExtMet_soil,  "_", qthresh_soil, "_", ExtMet, "_", qthresh )
  } else if (ExtMet == "quantile") {
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet, "_", qthresh)
  } else if (ExtMet_soil == "quantile"){
    colnames(results) <- paste0("EPH_", qthreshsingle,"_", ExtMet_soil, "_", qthresh_soil,"_", ExtMet )
  } else {
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet )
  }
  
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$DSM_data<-sapply(imgFiles.dsm.names, function(x) gsub(".tif", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message", duration = NULL)
  message("Finished processing DSM")
  
  return(results_df)
  
}


PH_est_ts_single<- function(dataset_img_sel, n.core, indPlots, Plot_ID, imgFiles.dsm.names, ExtMet, qthresh, qthreshsingle, qthresh_soil, ExtMet_soil) {
  
  tryCatch({
    if (is.null(dataset_img_sel)) {
      stop("No DSM or DTM files provided")
    }
  },
  error = function(e) {
    showNotification(paste("No data set provided", e), type = "error")
    message(paste("No data set provided", e))
  }
  )
  
  shape <- indPlots
  
  for(c in 1:length(dataset_img_sel)){ 
    
    DSM <- stack(dataset_img_sel[c])
    overlay_data(DSM, shape)
  }
  
  
  j=0
  setProgress(message = "Running EPH analysis - Looping through the files", detail = "", value = 0)
  
  for(i in 1:length(dataset_img_sel)){ #loop through images
    
    message("Processing Point Cloud: ",paste(dataset_img_sel[i]))

    DSM1 <- stack(dataset_img_sel[i])
    
    crs.result <- shp_test_crs(DSM1, shape)
    DSM1 <- crs.result$data
    shape <- crs.result$shp
    
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
                          
                          Q1_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.25, na.rm=TRUE)
                          Q3_DSM <- cellStats(DSM1.c, 'quantile', probs = 0.75, na.rm=TRUE)
                          IQR_DSM <- Q3_DSM - Q1_DSM
                          
                          # Define the lower and upper bounds for outliers
                          kout = 1.5
                          lower_bound <- Q1_DSM - kout * IQR_DSM
                          upper_bound <- Q3_DSM + kout * IQR_DSM
                          
                          # Remove outliers in DSM1
                          DSM1.c <- DSM1.c[DSM1.c >= lower_bound & DSM1.c <= upper_bound,]
                          DSM2.c <- DSM2.c[DSM2.c >= lower_bound & DSM2.c <= upper_bound,]
                          
                          csm_rem <- quantile(DSM1.c,qthreshsingle, na.rm=TRUE)
                          
                          DSM1.c[DSM1.c <= csm_rem] = NA
                          DSM2.c[DSM2.c > csm_rem] = NA
                          
                          # Extracting the estimate plant height average (EPH):
                          if (ExtMet == "mean") {
                            EPH1 <- mean(DSM1.c, na.rm=TRUE)
                          }
                          
                          if( ExtMet_soil == "mean") {
                            EPH2 <- mean(DSM2.c, na.rm=TRUE)
                          } 
                          
                          if (ExtMet == "median"){
                            EPH1 <- median(DSM1.c, na.rm=TRUE)
                          } 
                          
                          if( ExtMet_soil == "median") {
                            EPH2 <- median(DSM2.c, na.rm=TRUE)
                          } 
                          
                          if (ExtMet == "quantile"){
                            EPH1 <- quantile(DSM1.c,qthresh, na.rm=TRUE)
                          } 
                          
                          if (ExtMet_soil == "quantile"){
                            EPH2 <- quantile(DSM2.c,qthresh_soil, na.rm=TRUE)
                            
                          }
                    
                          EPH <- round(EPH1 - EPH2, 4)
                          
                        },
      error = function(e) {
        showNotification(paste("Error in PH_est_ts_single:", e), type = "error")
        message(paste("Error in PH_est_ts_single:", e))
      }
    ) 
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    if (ExtMet == "quantile" && ExtMet_soil == "quantile"){
      colnames(results) <- paste0("EPH_", qthreshsingle,"_", ExtMet_soil,  "_", qthresh_soil, "_", ExtMet, "_", qthresh )
    } else if (ExtMet == "quantile") {
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet, "_", qthresh)
    } else if (ExtMet_soil == "quantile"){
      colnames(results) <- paste0("EPH_", qthreshsingle,"_", ExtMet_soil, "_", qthresh_soil,"_", ExtMet )
    } else {
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet )
    }
    
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
  
  # Assign row names
  row.names(results_df.1) <- paste0(1:nrow(results_df.1))
 
  # show a message when the function has finished running
  showNotification("Finished processing DSM", type = "message",  duration = NULL)
  message("Finished processing DSM")
  
  return(results_df.1)
  
}


## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud.est.ind.single <- function(cloud, shape, qthresh, qthresh_soil, qthreshsingle, ExtMet, ExtMet_soil, Plot_ID, cloud_names, n.core) {
  
  if (length(cloud) == 0) {
    showNotification("No LAZ files provided", type = "error")
    return()
  }
  
  setProgress(message = "Running EPH analysis", detail = "")
  # loading the LAS files
  cloud_veg <- readLAS(files = cloud, select = "xyz")
  
  crs.result <- shp_test_crs_pc(cloud_veg, shape)
  cloud_veg <- crs.result$data
  shape <- crs.result$shp
  
  
  # Starting parallel
  cl <- makeCluster(n.core, output = "")
  registerDoParallel(cl)
  getDoParWorkers()
  
  message("Processing DSM: ",paste(cloud))
  
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
                         # Remove outliers in c_veg using IQR
                         Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
                         Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
                         IQR_veg <- Q3_veg - Q1_veg
                         kout <- 1.5
                         c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
                         
                         EPH.Extract.veg<- c_veg@data$Z
                         EPH.Extract.soil<- c_veg@data$Z
                         
                         EPH_soil <-  quantile(EPH.Extract.soil, probs = qthreshsingle, na.rm=TRUE)
                         
                         EPH.Extract.veg[EPH.Extract.veg <= EPH_soil[[1]][[1]]] = NA 
                         EPH.Extract.soil[EPH.Extract.soil > EPH_soil[[1]][[1]]] = NA
                         
                         # Extracting the estimate plant height average (EPH):
                         if (ExtMet == "mean") {
                           EPH1 <- mean(EPH.Extract.veg, na.rm=TRUE)
                         }
                         if (ExtMet_soil == "mean") {
                             EPH2 <- mean(EPH.Extract.soil, na.rm=TRUE)
                         }
                         if (ExtMet == "median") {
                           EPH1 <- median(EPH.Extract.veg, na.rm=TRUE)
                         }
                         if (ExtMet_soil == "median") {
                           EPH2 <- median(EPH.Extract.soil, na.rm=TRUE)
                         }
                         if (ExtMet == "quantile"){
                           
                           EPH1 <-  quantile(EPH.Extract.veg, probs = qthresh, na.rm=TRUE)
                         }
                         if (ExtMet_soil == "quantile"){
                           EPH2 <- quantile(EPH.Extract.soil, probs = qthresh_soil, na.rm=TRUE)
                           
                         }
                         # Compute difference and round to two decimal places
                         EPH <- round(EPH1 - EPH2, 4)
                       },
    error = function(e) {
      showNotification(paste("Error in clipper.cloud.est.ind.single:", e), type = "error")
      message(paste("Error in clipper.cloud.est.ind.single:", e))
    }
  )
  
  parallel::stopCluster(cl) # Stopping the parallel function
  
  if (ExtMet == "quantile" && ExtMet_soil == "quantile"){
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil,"_", qthresh_soil, "_", ExtMet,  "_", qthresh )
  } else if (ExtMet == "quantile") {
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet, "_", qthresh )
  } else if (ExtMet_soil == "quantile"){
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", qthresh_soil, "_", ExtMet )
  } else {
    colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil,  "_", ExtMet)
  }
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Add row names from Plot_ID as a new column to results data frame
  results_df <- cbind(Plot_ID, results_df)
  
  results_df$PointCloud_data<-sapply(cloud_names, function(x) gsub(".laz", "", basename(x)))
  
  # Assign row names
  row.names(results_df) <- paste0(1:nrow(results_df))
  
  # show a message when the function has finished running
  showNotification("Finished processing LAZ", type = "message", duration = NULL)
  message("Finished processing point cloud")
  
  return(results_df)
  
}

## Time series data - PC single flight
clipper.cloud.est.ts.single <- function(cloud_list, shape, qthresh, qthreshsingle, ExtMet, Plot_ID, cloud_names, n.core, ExtMet_soil, qthresh_soil) {
  
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
    
    crs.result <- shp_test_crs_pc(cloud, shape)
    cloud <- crs.result$data
    shape <- crs.result$shp
    
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
                           # Remove outliers in c_veg using IQR
                           Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
                           Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
                           IQR_veg <- Q3_veg - Q1_veg
                           kout <- 1.5
                           c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
                           
                           EPH.Extract.veg<- c_veg@data$Z
                           EPH.Extract.soil<- c_veg@data$Z
                           
                           EPH_soil <-  quantile(EPH.Extract.soil, probs = qthreshsingle, na.rm=TRUE)
                           
                           EPH.Extract.veg[EPH.Extract.veg <= EPH_soil[[1]][[1]]] = NA 
                           EPH.Extract.soil[EPH.Extract.soil > EPH_soil[[1]][[1]]] = NA
                           
                           # Extracting the estimate plant height average (EPH):
                           if (ExtMet == "mean") {
                             EPH1 <- mean(EPH.Extract.veg, na.rm=TRUE)
                           } 
                           if (ExtMet_soil == "mean") {
                             EPH2 <- mean(EPH.Extract.soil, na.rm=TRUE)
                           } 
                           if (ExtMet == "median") {
                             EPH1 <- median(EPH.Extract.veg, na.rm=TRUE)
                           } 
                           if (ExtMet_soil == "median") {
                             EPH2 <- median(EPH.Extract.soil, na.rm=TRUE)
                           } 
                           if (ExtMet == "quantile"){
                             
                             EPH1 <-  quantile(EPH.Extract.veg, probs = qthresh, na.rm=TRUE)

                           } 
                           if (ExtMet_soil == "quantile"){
                             EPH2 <- quantile(EPH.Extract.soil, probs = qthresh_soil, na.rm=TRUE)
                             
                           }
                           # Compute difference and round to two decimal places
                           EPH <- round(EPH1 - EPH2, 4)
                             
                           
                         },
      error = function(e) {
        showNotification(paste("Error in clipper.cloud.est.ts.single:", e), type = "error")
        message(paste("Error in clipper.cloud.est.ts.single:", e))
      }
    )
    
    parallel::stopCluster(cl) # Stopping the parallel function
    
    
    if (ExtMet == "quantile" && ExtMet_soil == "quantile"){
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil,"_", qthresh_soil, "_", ExtMet,  "_", qthresh )
    } else if (ExtMet == "quantile") {
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", ExtMet, "_", qthresh )
    } else if (ExtMet_soil == "quantile"){
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil, "_", qthresh_soil, "_", ExtMet )
    } else {
      colnames(results) <- paste0("EPH_", qthreshsingle, "_", ExtMet_soil,  "_", ExtMet)
    }
    
    
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
  
  # show a message when the function has finished running
  showNotification("Finished processing LAz", type = "message", duration = NULL)
  message("Finished processing point cloud")
  
  return(results_df.1)
  
}



## Server =========================================================================

server <- function(input, output, session) {
  
  shinyjs::useShinyjs()
  
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
  
  observeEvent(input$check_button, {
    result <- check_packages()
    
    if (result == "All packages are already installed.") {
      shinyjs::enable("shapefile_button") # Enable the shapefile_button
      showNotification("All packages are installed.", type = "message", duration = 3)
    } else {
      showNotification(result, type = "error", duration = NULL)
    }
  }, ignoreInit = TRUE)
  
  
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
  
  
  dsm_files_path <- reactiveVal(NULL)
  
  observeEvent(list(input$dsm_folder, input$dsm_upload), {
    if (is.null(input$dsm_folder_check)) {
      return(NULL)
    }
    
    if (input$dsm_folder_check) {
      # Handle DSM file upload
      if (!is.null(input$dsm_upload)) {
        dsm_files <- input$dsm_upload
        dsm_files_path(lapply(seq_along(dsm_files$datapath), function(i) {
          file.copy(dsm_files$datapath[i], paste0(tempdir(), "/", dsm_files$name[i]))
          return(paste0(tempdir(), "/", dsm_files$name[i]))
        }))
        print(dsm_files_path)
        dsm_files_names <- sapply(input$dsm_upload$name, function(x) gsub(".tif", "", x))
        cat("Selected DSM files:", dsm_files_names, "\n")
        output$dsm_folder_name <- renderText("Uploaded DSM files:")
        output$imgFiles.dsm <- renderText(paste0("Selected DSM files: ", dsm_files_names))
        #print(dsm_files_names)
        #print(dsm_files)
      } else {
        cat("No DSM files uploaded!\n")
        output$dsm_folder_name <- renderText("No DSM files uploaded!")
        output$imgFiles.dsm <- renderText("No DSM files uploaded!")
      }
      
    } else {
      # Handle DSM folder selection
      if (!is.null(input$dsm_folder) && input$dsm_folder != "") {
        if (dir.exists(input$dsm_folder)) {
          dsm_folder_name <- basename(input$dsm_folder)
          imgFiles.dsm <- list.files(path = input$dsm_folder, pattern="*.tif$", full.names = T) #get the DSM 
          imgFiles.dsm <- sapply(imgFiles.dsm, function(x) gsub(".tif", "", basename(x)))
          cat("Selected DSM folder:", dsm_folder_name, "\n")
          imgFiles.dsm.list <- as.list(imgFiles.dsm)
          imgFiles.dsm.list <<- sapply(imgFiles.dsm.list, function(x) gsub(".tif", "", basename(x)))
          output$dsm_folder_name <- renderText(paste0("Selected DSM folder: ", dsm_folder_name))
          output$imgFiles.dsm <- renderText(paste0("Selected DSM files: ", imgFiles.dsm))
          #print(imgFiles.dsm)
          #print(imgFiles.dsm.list)
        } else {
          cat("Invalid DSM folder path!\n")
          output$dsm_folder_name <- renderText("Invalid DSM folder path!")
          output$imgFiles.dsm <- renderText("Invalid DSM files path!")
        }
      } else {
        cat("No DSM folder selected!\n")
        output$dsm_folder_name <- renderText("No DSM folder selected!")
        output$imgFiles.dsm <- renderText("No DSM files selected!")
      }
    }
  }, ignoreInit = TRUE )
  
  
  
  ## Load the PC files - vegetation
laz_files_path <- reactiveVal(NULL)

observeEvent(list(input$laz_folder, input$laz_upload), {
  if (is.null(input$laz_folder_check)) {
    return(NULL)
  }
  
  if (input$laz_folder_check) {
    # Handle LAZ file upload
    if (!is.null(input$laz_upload)) {
      laz_files <- input$laz_upload
      laz_files_path(lapply(seq_along(laz_files$datapath), function(i) {
        file.copy(laz_files$datapath[i], paste0(tempdir(), "/", laz_files$name[i]))
        return(paste0(tempdir(), "/", laz_files$name[i]))
      }))
      print(laz_files_path)
      laz_files_names <- sapply(input$laz_upload$name, function(x) gsub(".laz", "", x))
      cat("Selected LAZ files:", laz_files_names, "\n")
      output$laz_folder_name <- renderText("Uploaded LAZ files:")
      output$imgFiles.laz <- renderText(paste0("Selected LAZ files: ", laz_files_names))
      #print(laz_files_names)
    } else {
      cat("No LAZ files uploaded!\n")
      output$laz_folder_name <- renderText("No LAZ files uploaded!")
      output$imgFiles.laz <- renderText("No LAZ files uploaded!")
    }
  } else {
    # Handle LAZ folder selection
    if (!is.null(input$laz_folder) && input$laz_folder != "") {
      if (dir.exists(input$laz_folder)) {
        laz_folder_name <- basename(input$laz_folder)
        imgFiles.laz <- list.files(path = input$laz_folder, pattern="*.laz$", full.names = T) #get the LAZ 
        imgFiles.laz <- sapply(imgFiles.laz, function(x) gsub(".laz", "", basename(x)))
        cat("Selected LAZ folder:", laz_folder_name, "\n")
        imgFiles.laz.list <- as.list(imgFiles.laz)
        imgFiles.laz.list <<- sapply(imgFiles.laz.list, function(x) gsub(".laz", "", basename(x)))
        output$laz_folder_name <- renderText(paste0("Selected LAZ folder: ", laz_folder_name))
        output$imgFiles.laz <- renderText(paste0("Selected LAZ files: ", imgFiles.laz))
        #print(imgFiles.laz)
      } else {
        cat("Invalid LAZ folder path!\n")
        output$laz_folder_name <- renderText("Invalid LAZ folder path!")
        output$imgFiles.laz <- renderText("Invalid LAZ files path!")
      }
    } else {
      cat("No LAZ folder selected!\n")
      output$laz_folder_name <- renderText("No LAZ folder selected!")
      output$imgFiles.laz <- renderText("No LAZ files selected!")
    }
  }
}, ignoreInit = TRUE)

  
  
  ## Load the shapefile files - Field area DSM
  dtm_path <- reactiveVal(NULL)
  
  ## Load the DTM files
  observeEvent(list(input$dtm_folder, input$dtm_upload), {
    if (is.null(input$dtm_folder_check)) {
      return(NULL)
    }
    
    if (input$dtm_folder_check) {
      # Handle DTM file upload
      if (!is.null(input$dtm_upload)) {
        dtm_files <- input$dtm_upload$datapath
        dtm_files_names <- sapply(input$dtm_upload$name, function(x) gsub(".tif", "", x))
        cat("Selected DTM files:", dtm_files_names, "\n")
        output$dtm_folder_name <- renderText("Uploaded DTM files:")
        output$imgFiles.dtm <- renderText(paste0("Selected DTM files: ", dtm_files_names))
        print(dtm_files_names)
        
        # Set dtm_path
        if (length(dtm_files) == 1) {
          dtm_path(dtm_files)
        } else {
          showNotification("Only one DTM file can be uploaded. Please upload only one file.", type = "error", duration = 10)
          message("Only one DTM file can be uploaded. Please upload only one file.")
        }
      } else {
        cat("No DTM files uploaded!\n")
        output$dtm_folder_name <- renderText("No DTM files uploaded!")
        output$imgFiles.dtm <- renderText("No DTM files uploaded!")
      }
    } else {
      # Handle DTM folder selection
      if (!is.null(input$dtm_folder) && input$dtm_folder != "") {
        if (dir.exists(input$dtm_folder)) {
          dtm_folder_name <- basename(input$dtm_folder)
          imgFiles.dtm <- list.files(path = input$dtm_folder, pattern="*.tif$", full.names = T) #get the DTM
          
          if (length(imgFiles.dtm) != 1) {
            showNotification("Only one DTM file can be uploaded. Please check the folder.", type = "error", duration = 10)
            message("Only one DTM file can be uploaded. Please check the folder.")
          } else {
            imgFiles.dtm_all <- list.files(path = input$dtm_folder, pattern="*.tif$", full.names = TRUE) #get the DTM
            dtm_path(imgFiles.dtm_all)
            imgFiles.dtm <- sapply(imgFiles.dtm, function(x) gsub(".tif", "", basename(x)))
            cat("Selected DTM folder:", dtm_folder_name, "\n")
            output$dtm_folder_name <- renderText(paste0("Selected DTM folder: ", dtm_folder_name))
            output$imgFiles.dtm <- renderText(paste0("Selected DTM file: ", imgFiles.dtm))
          }
          
        } else {
          cat("Invalid DTM folder path!\n")
          output$dtm_folder_name <- renderText("Invalid DTM folder path!")
          output$imgFiles.dtm <- renderText("Invalid DTM file path!")
        }
      }
    }
  }, ignoreInit = TRUE)
  
 
  ## Load the shapefile files - Field area DSM
  laz_soil_path <- reactiveVal(NULL)
  
  ## Load the laz files - Point Cloud
  observeEvent(list(input$laz_folder.soil, input$laz_upload_soil), {
    if (is.null(input$laz_folder_soil_check)) {
      return(NULL)
    }
    
    if (input$laz_folder_soil_check) {
      # Handle LAZ soil file upload
      if (!is.null(input$laz_upload_soil)) {
        laz_soil_files <- input$laz_upload_soil$datapath
        laz_soil_files_names <- sapply(input$laz_upload_soil$name, function(x) gsub(".laz", "", x))
        cat("Selected LAZ soil files:", laz_soil_files_names, "\n")
        output$laz_folder_name_soil <- renderText("Uploaded LAZ soil files:")
        output$imgFiles.laz.soil <- renderText(paste0("Selected LAZ soil files: ", laz_soil_files_names))
        print(laz_soil_files_names)
        
        # Set laz_soil_path
        if (length(laz_soil_files) == 1) {
          laz_soil_path(laz_soil_files)
        } else {
          showNotification("Only one LAZ soil file can be uploaded. Please upload only one file.", type = "error", duration = 10)
          message("Only one LAZ soil file can be uploaded. Please upload only one file.")
        }
      } else {
        cat("No LAZ soil files uploaded!\n")
        output$laz_folder_name_soil <- renderText("No LAZ soil files uploaded!")
        output$imgFiles.laz.soil <- renderText("No LAZ soil files uploaded!")
      }
    } else {
      # Handle LAZ soil folder selection
      if (!is.null(input$laz_folder.soil) && input$laz_folder.soil != "") {
        if (dir.exists(input$laz_folder.soil)) {
          laz_folder_name_soil <- basename(input$laz_folder.soil)
          imgFiles.laz.soil <- list.files(path = input$laz_folder.soil, pattern="*.laz$", full.names = T)
          
          if (length(imgFiles.laz.soil) != 1) {
            showNotification("Only one LAZ soil file can be uploaded. Please check the folder.", type = "error", duration = 10)
            message("Only one LAZ soil file can be uploaded. Please check the folder.")
          } else {
            imgFiles.laz.soil_all <- list.files(path = input$laz_folder.soil, pattern="*.laz$", full.names = T)
            laz_soil_path(imgFiles.laz.soil_all)
            imgFiles.laz.soil <- sapply(imgFiles.laz.soil, function(x) gsub(".laz", "", basename(x)))
            cat("Selected LAZ soil folder:", laz_folder_name_soil, "\n")
            output$laz_folder_name_soil <- renderText(paste0("Selected LAZ soil folder: ", laz_folder_name_soil))
            output$imgFiles.laz.soil <- renderText(paste0("Selected LAZ soil files: ", imgFiles.laz.soil))
          }
          
        } else {
          cat("Invalid LAZ soil folder path!\n")
          output$laz_folder_name_soil <- renderText("Invalid LAZ soil folder path!")
          output$imgFiles.laz.soil <- renderText("Invalid LAZ soil files path!")
        }
      }
    }
  }, ignoreInit = TRUE)
  

   
  clippedDSMPath <- reactiveVal(NULL)
    # Create a temporary directory
    temp_dir <- tempfile()
    print(temp_dir)
    dir.create(temp_dir)
    # Update clippedDSMPath reactive value with the new temporary directory
    clippedDSMPath(temp_dir)

  
  # Use isolate to access the value of input$dsm_folder_check_area outside of reactive context
  observe({
      print("Using existing files")
      DSMPath_val<-isolate(DSMPath((input$dsm_folder_area)))
      if (!is.null(DSMPath_val)) {
        print("Using existing files")
        print(paste("Path:", DSMPath()))
      } else {
        print("Invalid DSM path!")
      }
    
  })
  
  

  clippedLazPath <- reactiveVal(NULL)
  # Create a temporary directory
  temp_dir_laz <- tempfile()
  dir.create(temp_dir_laz)
  
  # Update clippedLazPath reactive value with the new temporary directory
  clippedLazPath(temp_dir_laz)
  
  
  ## Load the shapefile files
  shapefile_path_global <- reactiveVal(NULL)
  
  
  observeEvent(list(input$shapefile_button, input$shapefile_upload), {
    shapefile_path <- NULL
    shapefile_name <- NULL
    
    if (is.null(input$shapefile_upload_check)) {
      return(NULL)
    }
    
    if (input$shapefile_upload_check) {
      # Handle shapefile file upload
      if (!is.null(input$shapefile_upload)) {
        #temp_dir <- tempdir()
        unzip(input$shapefile_upload$datapath, exdir = temp_dir)
        shp_files <- list.files(temp_dir, pattern = "*.shp$", full.names = TRUE)
        
        if (length(shp_files) == 1) {
          shapefile_path <- shp_files[[1]]
          shapefile_name <- input$shapefile_upload$name
          cat("Selected shapefile:", shapefile_name, "\n")
          shapefile_path_global(shapefile_path)  
          print(paste("Uploaded shapefile path:", shapefile_path)) 
        } else {
          shapefile_path <- NULL
          shapefile_name <- "Invalid shapefile! Please upload a .zip file containing only one shapefile and its associated files."
        }
      } else {
        shapefile_path <- NULL
        shapefile_name <- "Invalid shapefile file path!"
      }
    } else {
      # Handle shapefile file selection
      shapefile_path <- tryCatch(file.choose(),
                                 error = function(e) NULL)
      if (!is.null(shapefile_path)) {
        if (endsWith(shapefile_path, ".shp")) {
          shapefile_name <- basename(shapefile_path)
        } else {
          shapefile_path <- NULL
          shapefile_name <- "Invalid file type! Please select a shapefile (.shp)."
        }
      } else {
        shapefile_name <- "Invalid shapefile file path!\n"
      }
    }
    
    if (!is.null(shapefile_path)) {
      shapefile_path_global(shapefile_path)
      print(paste("Shapefile path:", shapefile_path))
      output$shapefile_name <- renderText(paste0("Selected shapefile: ", shapefile_name))
      indPlots <- st_read(shapefile_path_global())
      output$contents <- DT::renderDataTable(DT::datatable(indPlots, options = list(pageLength = 5, scrollX = T)))
      # Update choices of plotID select input
      updateSelectInput(session, "plotID",
                        label = "Select the Plot ID name:",
                        choices = colnames(indPlots),
                        selected = colnames(indPlots)[1])
      shinyjs::enable("go_button")
      shinyjs::hide("upload-shapefile-message")
    } else {
      output$shapefile_name <- renderText(shapefile_name)
      shinyjs::disable("go_button")
      shinyjs::show("upload-shapefile-message")
    }
  }, ignoreInit = TRUE)
  
  
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
  
  
  
  ## Printing the data frame using the plotID selected
  indPlots_df <- reactive({
    if (is.null(shapefile_path_global)) {
      return(NULL)
    } else {
      st_read(shapefile_path_global()) %>% as.data.frame()
    }
  })
  
  ## Shapefile to use in the loop
  indPlots_2 <- reactive({
    if (is.null(shapefile_path_global)) {
      return(NULL)
    } else {
      st_read(shapefile_path_global()) 
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
  uploaded_dsm_folder <- reactiveVal(NULL) 
  
  imgFiles.dsm <- reactive({
    if (is.null(input$dsm_folder_check)) {
      return(NULL)
    }
    
    if (input$dsm_folder_check) {
      # Handle DSM file upload
      if (!is.null(input$dsm_upload)) {
        dsm_files_path <- unique(dirname(unlist(dsm_files_path())))
        uploaded_dsm_folder(dsm_files_path)
        files <- list.files(dsm_files_path, pattern = "*.tif$", full.names = TRUE)
        #print(files)
        if (length(files) == 0) {
          print("No DSM files found in the uploaded file!")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("No DSM files uploaded!")
        return(NULL)
      }
      
    } else {  
      # Handle DSM file selection
      if (!is.null(input$dsm_folder)) {
        if (dir.exists(input$dsm_folder)) {
          files <- list.files(path = input$dsm_folder, pattern="*.tif$", full.names = T) #get the DSM 
          #print(files)
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
    }
  })
  
  uploaded_dtm_folder <- reactiveVal(NULL)
  
  observeEvent(input$dtm_upload, {
    req(input$dtm_upload)
    dtm_files <- input$dtm_upload$datapath
    dtm_folder <- dirname(dtm_files[1])
    uploaded_dtm_folder(dtm_folder)
  })
  
  
  output$ind_data_flight_ui <- renderUI({
    if (input$method == "Ind. Data Flight" && input$PH_engine == "CSM (DSM - DTM)" && !is.null(imgFiles.dsm())) {
      if (length(imgFiles.dsm()) == 0) {
        print("No flights available!")
        return(NULL)
      }
      radioButtons("flight_select", "Select flight:",
                   choices = basename(imgFiles.dsm()),
                   selected = basename(imgFiles.dsm())[1]
      )
      
    } 

  })
  

  selected_dsm <- reactive({
    if (!is.null(input$flight_select)) {
      if (input$dsm_folder_check) {
        # Handle DSM file upload
        dsm_files <- imgFiles.dsm()
        print(dsm_files)
        selected_file <- file.path(normalizePath(uploaded_dsm_folder()), input$flight_select)
        if (file.exists(selected_file)) {
          return(selected_file)
        } else {
          return(NULL)
        }
      } else {
        # Handle DSM file selection
        selected_file <- file.path(normalizePath(input$dsm_folder), input$flight_select)
        if (file.exists(selected_file)) {
          return(selected_file)
        } else {
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  
  
  
  ## Selecting individual images to run - PC
  uploaded_laz_folder <- reactiveVal(NULL)
  
  imgFiles.laz <- reactive({
    if (is.null(input$laz_folder_check)) {
      return(NULL)
    }
    
    if (input$laz_folder_check) {
      # Handle LAZ file upload
      if (!is.null(input$laz_upload)) {
        laz_files_path <- unique(dirname(unlist(laz_files_path())))
        uploaded_laz_folder(laz_files_path)
        files <- list.files(laz_files_path, pattern="*.laz$", full.names = TRUE)
        if (length(files) == 0) {
          print("No laz files found in the uploaded file!")
          return(NULL)
        } else {
          cat("Files found in imgFiles.laz():", files, "\n")
          return(files)
        }
      } else {
        print("No laz files uploaded!")
        return(NULL)
      }
    } else {
      # Handle LAZ file selection
      if (!is.null(input$laz_folder)) {
        if (dir.exists(input$laz_folder)) {
          files <- list.files(path = input$laz_folder, pattern="*.laz$", full.names = T) #get the PC only 
          if (length(files) == 0) {
            print("No laz files found in the selected folder!")
            return(NULL)
          } else {
            cat("Files found in imgFiles.laz():", files, "\n")
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
    }
  })
  
  output$ind_data_flight_ui_laz <- renderUI({
    if (input$method == "Ind. Data Flight" && input$PH_engine == "Point Cloud" &&  !is.null(imgFiles.laz())) {
      if (length(imgFiles.laz()) == 0) {
        print("No flights available!")
        return(NULL)
      }
      radioButtons("flight_select_laz", "Select flight:",
                   choices = basename(imgFiles.laz()),
                   selected = basename(imgFiles.laz()[1]))
    }
  })
  
  selected_laz <- reactive({
    cat("input$flight_select_laz:", input$flight_select_laz, "\n")
    if (!is.null(input$flight_select_laz)) {
      cat("input$laz_folder_check:", input$laz_folder_check, "\n")
      if (input$laz_folder_check) {
        # Handle LAZ file upload
        laz_files <- imgFiles.laz()
        cat("laz_files:", laz_files, "\n")
        selected_file <- file.path(normalizePath(uploaded_laz_folder()), input$flight_select_laz)
        if (file.exists(selected_file)) {
          return(selected_file)
        } else {
          return(NULL)
        }
      } else {
        # Handle LAZ file selection
        selected_file <- file.path(normalizePath(input$laz_folder), input$flight_select_laz)
        if (file.exists(selected_file)) {
          return(selected_file)
        } else {
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  # Create a reactiveVal to store the DSM path
  DSMPath <- reactiveVal(NULL)
  
  ## Field area DSM
  imgFiles.dsm_areas <- reactive({
    if (!is.null(input$dsm_folder_area) && !input$dsm_folder_check_area) {
      # Handle selected files
      if (dir.exists(input$dsm_folder_area)) {
        files <- list.files(path = input$dsm_folder_area, pattern="*.tif$", full.names = T) #get the PC only 
        message("Print file names")
        print(files)

        if (length(files) == 0) {
          print("No tif files found in the selected folder!")
          return(NULL)
        } else {
          dir_name <- dirname(files[1])
          DSMPath(dir_name)
          print(paste("Directory name:", dir_name))
          return(files)
        }
      } else {
        print("Invalid tif files folder path!")
        return(NULL)
      }
    } else if (!is.null(input$dsm_upload_area) && input$dsm_folder_check_area) {
      # Handle uploaded files
      uploaded_files <- input$dsm_upload_area$datapath
      uploaded_file_names <- input$dsm_upload_area$name # Get the original file names
      message("Print file names")
      print(uploaded_files)
      print(clippedDSMPath())
      
      # Save the uploaded TIF files in temp_dir with their original file names
      temp_files <- file.path(temp_dir, uploaded_file_names)
      file.copy(uploaded_files, temp_files, overwrite = TRUE)
      
      if (length(temp_files) == 0) {
        print("No tif files uploaded!")
        return(NULL)
      } else {
        return(temp_files)
      }
    } else {
      print("No tif files selected or uploaded!")
      return(NULL)
    }
  })
  
    

  dsm_files <- reactive({
    if (!is.null(input$dsm_folder_area) && !input$dsm_folder_check_area) {
      # Handle selected files
      if (dir.exists(input$dsm_folder_area)) {
        files <- list.files(path = input$dsm_folder_area, pattern="*.tif$", full.names = T) 
       # files <- files[!grepl("_clip", files)]
        if (length(files) == 0) {
          print("No tif files found in the selected folder!")
          return(NULL)
        } else {
          return(basename(files))
        }
      } else {
        print("Invalid tif files folder path!")
        return(NULL)
      }
    } else if (!is.null(input$dsm_upload_area) && input$dsm_folder_check_area) {
      # Handle uploaded files
      files <- input$dsm_upload_area$name
      if (length(files) == 0) {
        print("No tif files uploaded!")
        return(NULL)
      } else {
        return(files)
      }
    } else {
      print("No tif files selected or uploaded!")
      return(NULL)
    }
  }) 
    
    
  selected_dsm_file <- reactive({
    if (!is.null(input$dsm_folder_area) && !input$dsm_folder_check_area) {
      # Handle selected files
      file.path(input$dsm_folder_area, input$nameID)
    } else if (!is.null(input$dsm_upload_area) && input$dsm_folder_check_area) {
      # Handle uploaded files
      input$dsm_upload_area$datapath[input$dsm_upload_area$name == input$nameID]
    }
  })
  
  
  observe({
    if (!is.null(dsm_files())) {
      updateSelectInput(session, "nameID",
                        label = "Select the File name:",
                        choices = dsm_files(),
                        selected = dsm_files()[1])
    }
  })
  
  
  output$file_name <- renderText({
    paste0("Selected File: ", input$nameID)
  })
 
  ## Field area PC
  imgFiles.laz_areas.pc <- reactive({
    if (!is.null(input$laz_folder_area.pc) && !input$laz_folder_check_area) {
      # Handle selected files
      if (dir.exists(input$laz_folder_area.pc)) {
        files <- list.files(path = input$laz_folder_area.pc, pattern="*.laz$", full.names = T) #get the PC only 
        message("Print file names")
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
    } else if (!is.null(input$laz_folder_area.pc_upload) && input$laz_folder_check_area) {
      # Handle uploaded files
      uploaded_files <- input$laz_folder_area.pc_upload$datapath
      uploaded_file_names <- input$laz_folder_area.pc_upload$name # Get the original file names
      message("Print file names")
      print(uploaded_files)
      
      # Save the uploaded LAZ files in temp_dir_laz with their original file names
      temp_files <- file.path(temp_dir_laz, uploaded_file_names)
      file.copy(uploaded_files, temp_files, overwrite = TRUE)
      
      if (length(temp_files) == 0) {
        print("No laz files uploaded!")
        return(NULL)
      } else {
        return(temp_files)
      }
    } else {
      print("No laz files selected or uploaded!")
      return(NULL)
    }
  })
  
  
  pc_files <- reactive({
    if (!is.null(input$laz_folder_area.pc) && !input$laz_folder_check_area) {
      # Handle selected files
      if (dir.exists(input$laz_folder_area.pc)) {
        files <- list.files(path = input$laz_folder_area.pc, pattern="*.laz$", full.names = T) 
        # files <- files[!grepl("_clip", files)]
        if (length(files) == 0) {
          print("No laz files found in the selected folder!")
          return(NULL)
        } else {
          return(basename(files))
        }
      } else {
        print("Invalid laz files folder path!")
        return(NULL)
      }
    } else if (!is.null(input$laz_folder_area.pc_upload) && input$laz_folder_check_area) {
      # Handle uploaded files
      files <- input$laz_folder_area.pc_upload$name
      if (length(files) == 0) {
        print("No laz files uploaded!")
        return(NULL)
      } else {
        return(files)
      }
    } else {
      print("No laz files selected or uploaded!")
      return(NULL)
    }
  })
  
  selected_pc_file <- reactive({
    if (!is.null(input$laz_folder_area.pc) && !input$laz_folder_check_area) {
      # Handle selected files
      file.path(input$laz_folder_area.pc, input$nameID_pc)
    } else if (!is.null(input$laz_folder_area.pc_upload) && input$laz_folder_check_area) {
      # Handle uploaded files
      input$laz_folder_area.pc_upload$datapath[input$laz_folder_area.pc_upload$name == input$nameID_pc]
    }
  })
  
  observe({
    if (!is.null(pc_files())) {
      updateSelectInput(session, "nameID_pc",
                        label = "Select the File name PC:",
                        choices = pc_files(),
                        selected = pc_files()[1])
    }
  })
  
  # add this reactive value to store the selected DSM file name
  
  output$file_name_pc <- renderText({
    paste0("Selected File: ", input$nameID_pc)
  })
  

 ###############################################################################
############################################################################### 
  
 ####### Plant Height Estimation (EPH)  ####### 

  ##### DSM PHE using Time Series Data method #####
  met_time.series <- reactive({
    tryCatch({
      if (is.null(imgFiles.dsm()) || is.null(dtm_path())) {
        stop("No DSM or DTM files provided")
      }
    
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
   PlantHeigh_est_ts(imgFiles.dtm = dtm_path(),
                     dataset_img_sel = imgFiles.dsm(),
                     n.core=n.core(),
                     indPlots=indPlots_2(),
                     Plot_ID = indPlots_col(),
                     imgFiles.dsm.names = imgFiles.dsm.list,
                     ExtMet =input$analysis_type,
                     qthresh = input$qthresh)
   
    })
    },
   error = function(e) {
     showNotification("No DSM or DTM files provided", type = "error", duration = 10)
     message(paste("No data set provided", e))
   }
    )
    
  })
  
  #### DSM PHE using individual flight data method ####
    met_ind_img <- reactive({
      tryCatch({
        if (is.null(selected_dsm()) || is.null(dtm_path())) {
          stop("No DSM or DTM files provided")
        }
        withProgress(message = "Running individual flight Data method", {
          setProgress(message = "Running...")
          
          PlantHeigh_est_ind(
            imgFiles.dtm = dtm_path(),
            dataset_img_sel = selected_dsm(),
            n.core = n.core(),
            indPlots = indPlots_2(),
            Plot_ID = indPlots_col(),
            imgFiles.dsm.names = input$flight_select,
            ExtMet = input$analysis_type,
            qthresh = input$qthresh
          )
        })
      },
      error = function(e) {
        showNotification("No DSM or DTM files provided", type = "error", duration = 10)
        message(paste("No data set provided", e))
      })
    })
    
    
  
  #### DSM PHE using individual flight data method ####
  met_ind_img_single <- reactive({
    tryCatch({
      if (is.null(selected_dsm())) {
        stop("No DSM files provided")
      }
    
    withProgress(message = "Running indvidual flight Data method", {
      setProgress(message = "Running...")
      
      PH_est_ind_single(
                          dataset_img_sel = selected_dsm(),
                          n.core=n.core(),
                          indPlots=indPlots_2(),
                          Plot_ID = indPlots_col(),
                          imgFiles.dsm.names = input$flight_select,
                          ExtMet =input$analysis_type,
                          qthresh = input$qthresh,
                          qthreshsingle = input$qthreshSoil_single,
                          ExtMet_soil = input$analysis_type_soil,
                          qthresh_soil = input$qthresh_s)
    })
    },
    error = function(e) {
      showNotification("No DSM or DTM files provided", type = "error", duration = 10)
      message(paste("No data set provided", e))
    }
    )
    
  })
 

      #### DSM PHE using time series flight data method ####
  met_ts_img_single <- reactive({
    tryCatch({
      if (is.null(imgFiles.dsm())) {
        stop("No DSM files provided")
      }
    
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
                        qthreshsingle = input$qthreshSoil_single,
                        qthresh_soil = input$qthresh_s,
                        ExtMet_soil = input$analysis_type_soil
                        )
      
      
    })
    },
    error = function(e) {
      showNotification("No DSM or DTM files provided", type = "error", duration = 10)
      message(paste("No data set provided", e))
    }
    )
  })
  
    

  #### PC PHE using Time Series Data method - Point Cloud ####
  met_time.series_laz <- reactive({
    tryCatch({
      if (is.null(imgFiles.laz()) || is.null(laz_soil_path())) {
        stop("No LAZ vegetation or soil files provided")
      }
      
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ts(
                      cloud_list =  imgFiles.laz(),
                      cloud_soil = laz_soil_path(),
                      shape = indPlots_2(),
                      Plot_ID = indPlots_col(),
                      qthresh = input$qthresh,
                      qthreshSoil = input$qthreshSoil,
                      ExtMet =input$analysis_type,
                      cloud_names = imgFiles.laz.list,
                      n.core=n.core()
      )

    })
    },
    error = function(e) {
      showNotification("No LAZ vegetation or soil files provided", type = "error", duration = 10)
      message(paste("No data set provided", e))
    }
    )
    
  })
  

  #### PC PHE using individual flight data method ####
  met_ind_img_laz <- reactive({
    tryCatch({
      if (is.null(selected_laz()) || is.null(laz_soil_path())) {
        stop("No LAZ vegetation or soil files provided")
      }
      
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ind(
                            cloud =  selected_laz(),
                            cloud_s = laz_soil_path(),
                            shape = indPlots_2(),
                            Plot_ID = indPlots_col(),
                            qthresh = input$qthresh,
                            qthreshSoil = input$qthreshSoil,
                            ExtMet =input$analysis_type,
                            cloud_names = input$flight_select_laz,
                            n.core=n.core()
      )
      
    })
    },
    error = function(e) {
      showNotification("No LAZ vegetation or soil files provided", type = "error", duration = 10)
      message(paste("No data set provided", e))
    }
    )
  })
  


  ## Individual - PC single
  met_ind_img_laz_single <- reactive({
    tryCatch({
      if (is.null(selected_laz())) {
        stop("No LAZ vegetation or soil files provided")
      }

    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Running...")
      
      clipper.cloud.est.ind.single(
        cloud =  selected_laz(),
        shape = indPlots_2(),
        Plot_ID = indPlots_col(),
        qthresh = input$qthresh,
        qthresh_soil = input$qthresh_s,
        ExtMet =input$analysis_type,
        ExtMet_soil = input$analysis_type_soil,
        cloud_names = input$flight_select_laz,
        n.core=n.core(),
        qthreshsingle = input$qthreshSoil_single
      )
    })
  },
  error = function(e) {
    showNotification("No LAZ vegetation files provided", type = "error", duration = 10)
    message(paste("No data set provided", e))
  }
  )
  })
  
  
  ## Time series - PC single
  met_ts_img_laz_single <- reactive({
    tryCatch({
      if (is.null(imgFiles.laz())) {
        stop("No LAZ vegetation or soil files provided")
      }

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
        qthreshsingle = input$qthreshSoil_single,
        qthresh_soil = input$qthresh_s,
        ExtMet_soil = input$analysis_type_soil
      )
  
    })
    },
   error = function(e) {
     showNotification("No LAZ vegetation files provided", type = "error", duration = 10)
     message(paste("No data set provided", e))
   }
    )
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
                                           input$PH_engine,
                                           ".csv") },
            content = function(file) { write.csv(met_ts_img_single(), file, row.names = FALSE) }
          )
          
        } else {
    
        output$met_time.series.table <- DT::renderDataTable({
          DT::datatable(met_time.series()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ts_flights", "_",
                                         input$PH_engine,
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
            filename = function() { paste0(input$flight_select, "_",
                                           input$PH_engine,
                                           ".csv") },
            content = function(file) { write.csv(met_ind_img_single(), file, row.names = FALSE) }
          )
          
        } else {
        
        output$met_ind_img.table <- DT::renderDataTable({
          DT::datatable(met_ind_img()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0(input$flight_select, "_",
                                         input$PH_engine,
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
                                           input$PH_engine,
                                           ".csv") },
            
            content = function(file) { write.csv(met_ts_img_laz_single(), file, row.names = FALSE) }
          )
          
        } else {
        
        output$met_ts_img_laz.table <- DT::renderDataTable({
          DT::datatable(met_time.series_laz()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0("ts_flights", "_",
                                         input$PH_engine,
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
          filename = function() { paste0(input$flight_select_laz, "_",
                                         input$PH_engine,
                                         ".csv") },
          
          
          content = function(file) { write.csv(met_ind_img_laz_single(), file, row.names = FALSE) }
        )
        
      } else {
        
        output$met_ind_img_laz.table <- DT::renderDataTable({
          DT::datatable(met_ind_img_laz()) 
        })
        
        output$downloadData <- downloadHandler(
          filename = function() { paste0(input$flight_select_laz, "_",
                                         input$PH_engine,
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
        dtm_path_plot <- if (input$dtm_folder_check) {
          normalizePath(uploaded_dtm_folder()) # Use the uploaded DTM folder path
        } else {
          normalizePath(input$dtm_folder) # Use the selected DTM folder path
        }
        
        if (!is.null(dtm_path_plot)) {
          if (dir.exists(dtm_path_plot)) {
            imgFiles.dtm <- list.files(path = dtm_path_plot, pattern = "*.tif$", full.names = T) #get the DTM
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
  
  
  # Define a reactive value to store the selected p value
  selected_k <- reactiveVal()

  ## DSM
  output$rasterPlotDSM <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
        dsm_path <- if (input$dsm_folder_check) {
          normalizePath(uploaded_dsm_folder()) # Use the uploaded DSM folder path
        } else {
          normalizePath(input$dsm_folder) # Use the selected DSM folder path
        }
        
        if (!is.null(dsm_path)) {
          if (dir.exists(dsm_path)) {
            colorPal <- terrain.colors(255)
            DSM1 <- stack(selected_dsm())
            
            # Adjust margins using par()
            par(mar = c(1.5, 1, 2, 0.5))
            plot(DSM1[[1]], col = colorPal, main = "Field DSM", axes=FALSE)
            
            # Check if there's a selected k value
            if (!is.null(selected_k())) {
              shape <- indPlots_2()
              bbox_sf <- st_bbox(shape[selected_k(),])
              
              # Convert bbox_sf to sf and then to Spatial
              bbox_sf_polygon <- st_as_sfc(bbox_sf)
              bbox_sp <- as(bbox_sf_polygon, "Spatial")
              # Convert bbox_sp to SpatialLines and plot the boundaries
              bbox_lines <- as(bbox_sp, "SpatialLines")
              plot(bbox_lines, add = TRUE, col = "red", lwd = 2)
            }
          } else {
            stop("DSM folder does not exist.")
          }
        } else {
          stop("DSM folder input is NULL.")
        }
      }, error = function(e) {
        message(paste("No DSM data available. Please upload a file.", e))
        output$rasterPlotDSM_file <- renderText("DSM visualization - Select analysis method: Ind. Data Flight")
        showNotification(paste("No DSM Plot data available. Please upload a file or select a flight."), type = "error")
        return(NULL)
      })
    })
  })
  

  # Render ind plot - DSM
  output$rasterPlotDSM_ind <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
            colorPal <- terrain.colors(255)
            DSM1 <- stack(selected_dsm())
            
            shape <- indPlots_2()
            
            crs.result <- shp_test_crs(DSM1, shape)
            DSM1 <- crs.result$data
            shape <- crs.result$shp
            
            k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for k
            selected_k(k) # store the selected k value in the reactive value
            DSM1.c <-  crop(DSM1, st_bbox(shape[k,]))

            levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "Selected plot view")
            
      }, error = function(e) {
        message("No DSM data available. Please upload a file to crop plots")
        showNotification(paste("No DSM data available. Please upload a file to crop plots"), type = "error")
        return(NULL)
      })

    })

  })
  
  output$rasterPlotDSM_file_ind <- renderText(paste0("Plot figure: ", indPlots_col()$PlotID_sel[selected_k()]))
  
  
  # Render ind plot2 - DSM
  output$rasterPlotDSM_ind2 <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
        if (input$dsm_single){
              colorPal <- terrain.colors(255)
              # imgFiles.dtm <-list.files(path = input$dtm_folder, pattern="*.tif$",full.names = T) #get the DTM
              DSM1 <- stack(selected_dsm())
              
              # print(selected_dsm())
              k <- selected_k() # access the selected k value from the reactive value
              DSM1.c <-  crop(DSM1, st_bbox(indPlots_2()[k,]))
              
              Q1_DSM <- quantile(DSM1.c, probs = 0.25, na.rm=TRUE)
              Q3_DSM <- quantile(DSM1.c, probs = 0.75, na.rm=TRUE)
              IQR_DSM <- Q3_DSM - Q1_DSM
              
              # Define the lower and upper bounds for outliers
              kout = 1.5
              lower_bound <- Q1_DSM - kout * IQR_DSM
              upper_bound <- Q3_DSM + kout * IQR_DSM
              
              # Remove outliers in DSM1.c using overlay() function
              DSM1.c <- overlay(DSM1.c, fun = function(x) {
                x[x < lower_bound | x > upper_bound] <- NA
                return(x)
              })
            
              EPH.Extract<-extract(x = DSM1.c, y = indPlots_2()[k,])
              EPH.soil<-lapply(EPH.Extract, quantile, probs = input$qthreshSoil_single, na.rm=TRUE)
              DSM1.c[DSM1.c <= EPH.soil[[1]][[1]]] = NA 
              
              csm_rem <- quantile(DSM1.c,input$qthresh, na.rm=TRUE)
              csm_q <- reclassify(DSM1.c, cbind(-Inf, csm_rem, NA)) #Remove values lower than the quantile threshold. set them to NA.
              
              EPH_rem = switch(input$analysis_type,
                               "mean" = cellStats(DSM1.c,mean, na.rm=TRUE), 
                               "median" = cellStats(DSM1.c,median, na.rm=TRUE),
                               "quantile" = cellStats(csm_q,median, na.rm=TRUE))
              
              DSM1.c[DSM1.c <= EPH_rem[[1]][[1]]] = NA 
             
              levelplot(DSM1.c[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")

        } else {
          dtm_path_plot <- if (input$dtm_folder_check) {
            normalizePath(uploaded_dtm_folder()) # Use the uploaded DTM folder path
          } else {
            normalizePath(input$dtm_folder) # Use the selected DTM folder path
          }
          
          if (!is.null(dtm_path_plot)) {
            if (dir.exists(dtm_path_plot)) {
              colorPal <- terrain.colors(255)
              
              imgFiles.dtm <-list.files(path = dtm_path_plot, pattern="*.tif$",full.names = T) #get the DTM
              
              DSM0 <- stack(imgFiles.dtm)
              DSM1 <- stack(selected_dsm())
              
              if (!isTRUE(lidR::st_crs(DSM0) == lidR::st_crs(DSM1))) {
                showNotification("The CRS for DSM and DTM data must be the same", type = "error")
                message("The CRS for DSM and DTM data must be the same")
                return(NULL)
              } else {
                message("The CRS for DSM and DTM data are the same")
              }
              
              if (isTRUE(identical(DSM0, DSM1))) {
                showNotification("DSM and DTM are identical files", type = "error")
                message("DSM and DTM are identical files")
                return(NULL)
              } else {
                message("DSM and DTM are NOT identical files")
              }
              
              shape <- indPlots_2()
              
              crs.result0 <- shp_test_crs(DSM0, shape)
              DSM0 <- crs.result0$data
              shape <- crs.result0$shp
              
              crs.result <- shp_test_crs(DSM1, shape)
              DSM1 <- crs.result$data
              shape <- crs.result$shp
              
              k <- selected_k() # access the selected k value from the reactive value
              DSM1.c <-  crop(DSM1, st_bbox(shape[k,]))
              DSM0.c <-  crop(DSM0, st_bbox(shape[k,]))
              
              DSM0.r <-resample(DSM0.c, DSM1.c)
              csm <- DSM1.c-DSM0.r 
              csm[csm <=0] = NA
              
              Q1_DSM <- quantile(csm, probs = 0.25, na.rm=TRUE)
              Q3_DSM <- quantile(csm, probs = 0.75, na.rm=TRUE)
              IQR_DSM <- Q3_DSM - Q1_DSM
              
              # Define the lower and upper bounds for outliers
              kout = 1.5
              lower_bound <- Q1_DSM - kout * IQR_DSM
              upper_bound <- Q3_DSM + kout * IQR_DSM
              
              # Remove outliers in DSM1.c using overlay() function
              csm <- overlay(csm, fun = function(x) {
                x[x < lower_bound | x > upper_bound] <- NA
                return(x)
              })
              
             EPH_rem = switch(input$analysis_type,
                               "mean" = cellStats(csm,mean, na.rm=TRUE), 
                               "median" = cellStats(csm,median, na.rm=TRUE),
                               "quantile" = quantile(csm,input$qthresh,na.rm=TRUE))
              
              csm[csm <= EPH_rem[[1]][[1]]] = NA 
              
              levelplot(csm[[1]], col.regions = colorPal, margin = FALSE, main = "CSM - Selected plot view")
            
              } else {
              stop("DSM folder does not exist.")
            }
          } else {
            stop("DSM folder input is NULL.")
          }
        }
        
      }, error = function(e) {
        message("No DSM Plot data available. Please upload a file to perform the CSM.")
        return(NULL)
      
      })

    }) 

  })
  
  output$rasterPlotDSM_file_ind2 <- renderText(paste0("Plot CSM figure: ", indPlots_col()$PlotID_sel[selected_k()],
                                                      " - Assigning selected pixels."))
  
  ### LAZ Point Cloud
  
  # Define a reactive value to store the selected p value
  selected_k2 <- reactiveVal()
  
  imgFiles.laz_veg_pc <- reactive({
    if (input$laz_folder_check) {
      # Handle LAZ file upload
      if (!is.null(input$laz_upload)) {
        files <- readLAS(files = selected_laz(), select = "xyz")
        
        if (length(files) == 0) {
          print("No laz files found in the uploaded file!")
          showNotification("No laz files found in the uploaded file!", type = "error")
          return(NULL)
        } else {
          return(files)
        }
      } else {
        print("No laz files uploaded!")
        return(NULL)
      }
    } else {
      # Handle LAZ file selection
      if (!is.null(input$laz_folder)) {
        if (dir.exists(input$laz_folder)) {
          files <- readLAS(files = selected_laz(), select = "xyz")
          
          if (length(files) == 0) {
            print("No laz files found in the selected folder!")
            showNotification("No laz files found in the selected folder!", type = "error")
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
    }
  })
  
  
  # Render vegetation plot - Point Cloud
  output$PlotLaz_ind <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
          cloud <- imgFiles.laz_veg_pc()
          shape <- indPlots_2()
          
          crs.result <- shp_test_crs_pc(cloud, shape)
          cloud <- crs.result$data
          shape <- crs.result$shp
          
          k <- sample(1:nrow(indPlots_df()), 1) # randomly select a value for p
          selected_k2(k) # store the selected k value in the reactive value
          p <- shape[k,]
          p_sp <- as(p, "Spatial")
          c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                  ytop = p_sp@bbox['y','max'], 
                                  xright = p_sp@bbox['x','max'], 
                                  ybottom = p_sp@bbox['y','min'])
          
          # Remove outliers in c_veg using IQR
          Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
          Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
          IQR_veg <- Q3_veg - Q1_veg
          kout <- 1.5
          c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
          
           # Set up plot region to arrange p1 and p2 vertically
          par(mfrow=c(2,1))
          
          # Display p1 and p2 in the same plot region
          plot(c_veg@data$Y, c_veg@data$Z, xlab = "Frontal view", ylab = "EPH", main = "Selected Plot - Point cloud distribution") 
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z, na.rm=TRUE),
                            "median" = median(c_veg@data$Z, na.rm=TRUE),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh, na.rm=TRUE)),
                 col = "red")
          plot(c_veg@data$X, c_veg@data$Z, xlab = "Lateral view", ylab = "EPH")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z, na.rm=TRUE),
                            "median" = median(c_veg@data$Z, na.rm=TRUE),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh, na.rm=TRUE)),
                 col = "blue")

          output$PlotLaz_ind_file <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
          

    }, error = function(e) {
      message("No Point cloud Plot data available. Please upload a file.")
      output$PlotLaz_ind_file <- renderText(paste0("Point Cloud visualization - Select analysis method: Ind. Data Flight"))
      showNotification(paste("No LAZ vegetaion Plot data available. Please upload a file or select a flight."), type = "error")
      
      return(NULL)
    })
  }) 
  }) 
  
  
  # Render vegetation average Point Cloud
  output$PlotLaz_ind2 <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
    tryCatch({
          cloud <- imgFiles.laz_veg_pc()
          k <- selected_k2() 
          p <- indPlots_2()[k,]
          p_sp <- as(p, "Spatial")
          c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'], 
                                  ytop = p_sp@bbox['y','max'], 
                                  xright = p_sp@bbox['x','max'], 
                                  ybottom = p_sp@bbox['y','min'])
          # Remove outliers in c_veg using IQR
          Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
          Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
          IQR_veg <- Q3_veg - Q1_veg
          kout <- 1.5
          c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]
          
          # Set up plot region to arrange p1 and p2 vertically
          par(mfrow=c(2,1))
          
          # Compute the average value of Z across Y
          avg_Zy <- tapply(c_veg@data$Z, c_veg@data$Y, mean, na.rm=TRUE)
          plot(avg_Zy, type="l", xlab="Frontal view", ylab="EPH", main = "Selected Plot - Average point cloud distribution")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z, na.rm=TRUE),
                            "median" = median(c_veg@data$Z, na.rm=TRUE),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh, na.rm=TRUE)),
                 col = "red")
          
          # Compute the average value of Z across Y
          avg_Zx <- tapply(c_veg@data$Z, c_veg@data$X, mean, na.rm=TRUE)
          plot(avg_Zx, type="l", xlab="Lateral view", ylab="EPH")
          abline(h = switch(input$analysis_type,
                            "mean" = mean(c_veg@data$Z, na.rm=TRUE),
                            "median" = median(c_veg@data$Z, na.rm=TRUE),
                            "quantile" = quantile(c_veg@data$Z, probs = input$qthresh, na.rm=TRUE)),
                 col = "blue")
          
          
          output$PlotLaz_ind_file2 <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
          
    }, error = function(e) {
      message("No Point cloud plot data available. Please upload a file.")
      #output$PlotLaz_ind_file2 <- renderText(paste0("Point cloud figure"))
      
      return(NULL)
    })
  }) 
  })
    
   
  ### PC soil
  
  imgFiles.laz_soil_pc <- reactive({
    if (input$laz_folder_check) {
      # Handle LAZ file upload
      if (!is.null(input$laz_upload_soil)) {
        
        uploaded_files <- unique(dirname(unlist(input$laz_upload_soil$datapath)))
        files <- list.files(uploaded_files, pattern = "*.laz$", full.names = TRUE)
        
        if (length(files) > 1) {
          stop("Only one Soil Point Cloud should be uploaded")
          showNotification("Only one Soil Point Cloud should be uploaded", type = "error")
          return(NULL)
        } else {
          cloud_s <- readLAS(files = files, select = "xyz")
        }
        
        if (length(files) == 0) {
          print("No LAZ files uploaded!")
          return(NULL)
        } else {
          return(cloud_s)
        }
      } else {
        print("No LAZ files uploaded!")
        return(NULL)
      }
    } else {
      # Handle LAZ file selection
      if (!is.null(input$laz_folder.soil)) {
        if (dir.exists(input$laz_folder.soil)) {
          files <- list.files(path = input$laz_folder.soil, pattern="*.laz$",full.names = T) #get the DTM
          
          if (length(files) > 1) {
            stop("Only one Soil Point Cloud should be uploaded")
            showNotification("Only one Soil Point Cloud should be uploaded", type = "error")
            return(NULL)
          } else {
            cloud_s <- readLAS(files = files, select = "xyz")
          }
          
          if (length(files) == 0) {
            print("No laz files found in the selected folder!")
            return(NULL)
          } else {
            return(cloud_s)
          }
        } else {
          print("Invalid laz folder path!")
          return(NULL)
        }
      } else {
        print("No laz folder selected!")
        return(NULL)
      }
    }
  })
  
  
  # Render ind plot - PC
  output$PlotLaz_ind_s <- renderPlot({
    withProgress(message = "Running Time Series Data method", {
      setProgress(message = "Initializing view plot figures")
      tryCatch({
            
            cloud_s <- imgFiles.laz_soil_pc()
            
            k <- selected_k2() 
            p <- indPlots_2()[k,]
            p_sp <- as(p, "Spatial")
            c_soil <- clip_rectangle(cloud_s, xleft = p_sp@bbox['x','min'], 
                                    ytop = p_sp@bbox['y','max'], 
                                    xright = p_sp@bbox['x','max'], 
                                    ybottom = p_sp@bbox['y','min'])

            # Remove outliers in c_veg using IQR
            Q1_soil <- quantile(c_soil@data$Z, 0.25, na.rm=TRUE)
            Q3_soil <- quantile(c_soil@data$Z, 0.75, na.rm=TRUE)
            IQR_soil <- Q3_soil - Q1_soil
            kout <- 1.5
            c_soil <- c_soil[c_soil@data$Z >= Q1_soil - kout * IQR_soil & c_soil@data$Z <= Q3_soil + kout * IQR_soil,]
            
            # Set up plot region to arrange p1 and p2 vertically
            par(mfrow=c(2,1))
            
            # Display p1 and p2 in the same plot region
            plot(c_soil@data$Y, c_soil@data$Z, xlab = "Frontal view", ylab = "EPH", main = "Selected Plot - Point cloud distribution") 
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z, na.rm=TRUE),
                              "median" = median(c_soil@data$Z, na.rm=TRUE),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil, na.rm=TRUE)),
                   col = "red")
            plot(c_soil@data$X, c_soil@data$Z, xlab = "Lateral view", ylab = "EPH")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z, na.rm=TRUE),
                              "median" = median(c_soil@data$Z, na.rm=TRUE),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil, na.rm=TRUE)),
                   col = "blue")
            
            output$PlotLaz_ind_file_s <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
            
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
            
            cloud_s <- imgFiles.laz_soil_pc()

            k <- selected_k2() 
            p <- indPlots_2()[k,]
            p_sp <- as(p, "Spatial")
            c_soil <- clip_rectangle(cloud_s, xleft = p_sp@bbox['x','min'], 
                                    ytop = p_sp@bbox['y','max'], 
                                    xright = p_sp@bbox['x','max'], 
                                    ybottom = p_sp@bbox['y','min'])
            

            # Remove outliers in c_veg using IQR
            Q1_soil <- quantile(c_soil@data$Z, 0.25, na.rm=TRUE)
            Q3_soil <- quantile(c_soil@data$Z, 0.75, na.rm=TRUE)
            IQR_soil <- Q3_soil - Q1_soil
            kout <- 1.5
            c_soil <- c_soil[c_soil@data$Z >= Q1_soil - kout * IQR_soil & c_soil@data$Z <= Q3_soil + kout * IQR_soil,]
            
            # Set up plot region to arrange p1 and p2 vertically
            par(mfrow=c(2,1))
            
            # Compute the average value of Z across Y
            avg_Zy <- tapply(c_soil@data$Z, c_soil@data$Y, mean, na.rm=TRUE)
            plot(avg_Zy, type="l", xlab="Frontal view", ylab="EPH", main = "Selected Plot - Average point cloud distribution")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z, na.rm=TRUE),
                              "median" = median(c_soil@data$Z, na.rm=TRUE),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil, na.rm=TRUE)),
                   col = "red")
            
            # Compute the average value of Z across Y
            avg_Zx <- tapply(c_soil@data$Z, c_soil@data$X, mean, na.rm=TRUE)
            plot(avg_Zx, type="l", xlab="Lateral view", ylab="EPH")
            abline(h = switch(input$analysis_type,
                              "mean" = mean(c_soil@data$Z, na.rm=TRUE),
                              "median" = median(c_soil@data$Z, na.rm=TRUE),
                              "quantile" = quantile(c_soil@data$Z, probs = input$qthreshSoil, na.rm=TRUE)),
                   col = "blue")
            
            
            output$PlotLaz_ind_file2_s <- renderText(paste0("Point cloud figure ID: ", indPlots_col()$PlotID_sel[k]))
            
      }, error = function(e) {
        message("No Point cloud Plot data available. Please upload a file.")
        #output$rasterPlotDSM_file_ind2 <- renderText(paste0("Point cloud figure"))
        
        return(NULL)
      })
    }) 
  })
     

  ## Load the shapefile files - Field area DSM
  selected_shapefile_path <- reactiveVal(NULL)
  
  observeEvent(c(input$shapefile_button_clip.dsm, input$shapefile_button_clip.dsm_upload), {
    if (input$dsm_folder_check_area) {
      # Handle uploaded shapefiles
      zip_path <- input$shapefile_button_clip.dsm_upload$datapath[1]
      print(zip_path)
      if (!is.null(zip_path) && endsWith(zip_path, ".zip")) {
        message("test here")
        unzip(zip_path, exdir = temp_dir)
        shapefile_path_clip <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
      } else {
        shapefile_path_clip <- NULL
      }
    } else {
      # Handle selected shapefiles
      shapefile_path_clip <- tryCatch(file.choose(), error = function(e) NULL)
    }
    
    if (!is.null(shapefile_path_clip) && endsWith(shapefile_path_clip, ".shp")) {
      shapefile_path_clip_global.dsm <<- shapefile_path_clip
      shapefile_name_clip.dsm <- basename(shapefile_path_clip)
      indPlots_clip <- st_read(shapefile_path_clip)
      output$contents_shape2 <- DT::renderDataTable(DT::datatable(indPlots_clip, options = list(pageLength = 5, scrollX = T)))
      selected_shapefile_path(shapefile_path_clip)
    } else {
      output$shapefile_name_clip.dsm <- renderText("Invalid file type! Please select a shapefile (.shp).")
    }
  }, ignoreInit = TRUE)
  
  
  ## Load the shapefile files - Field area PC
  selected_shapefile_path_pc <- reactiveVal(NULL)
  
  observeEvent(c(input$shapefile_button_clip.pc, input$shapefile_button_clip.pc_upload), {
    if (input$laz_folder_check_area) {
      # Handle uploaded shapefiles
      zip_path <- input$shapefile_button_clip.pc_upload$datapath[1]
      if (!is.null(zip_path) && endsWith(zip_path, ".zip")) {
        unzip(zip_path, exdir = temp_dir)
        shapefile_path_clip <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
      } else {
        shapefile_path_clip <- NULL
      }
    } else {
      # Handle selected shapefiles
      shapefile_path_clip <- tryCatch(file.choose(), error = function(e) NULL)
    }
    
    if (!is.null(shapefile_path_clip) && endsWith(shapefile_path_clip, ".shp")) {
      shapefile_path_clip_global<<- shapefile_path_clip
      shapefile_name_clip.pc <- basename(shapefile_path_clip)
      indPlots_clip <- st_read(shapefile_path_clip)
      output$contents_shape3 <- DT::renderDataTable(DT::datatable(indPlots_clip, options = list(pageLength = 5, scrollX = T)))
      selected_shapefile_path_pc(shapefile_path_clip)
    } else {
      output$shapefile_name_clip.pc <- renderText("Invalid file type! Please select a shapefile (.shp).")
    }
  }, ignoreInit = TRUE)
  
  
  imgFiles.dsm_area_names <- reactiveVal(NULL)
## Load the DSM files - field area DSM
observeEvent(list(input$dsm_folder_area, input$dsm_upload_area), {
  if (!is.null(input$dsm_folder_area) && !input$dsm_folder_check_area) {
    # Handle selected files
    if (dir.exists(input$dsm_folder_area)) {
      dsm_folder_name_area <- basename(input$dsm_folder_area)
      imgFiles.dsm_area <- list.files(path = input$dsm_folder_area, pattern="*.tif$", full.names = T)
      imgFiles.dsm_area <- sapply(imgFiles.dsm_area, function(x) gsub(".tif", "", basename(x)))
      cat("Selected DSM vegetation folder:", dsm_folder_name_area, "\n")
      output$dsm_folder_name_area <- renderText(paste0("Selected DSM folder name: ", dsm_folder_name_area, "\n"))
      output$imgFiles.dsm_area <- renderText(paste0("Selected DSM files: ", imgFiles.dsm_area, "\n"))
    } else {
      cat("Invalid DSM vegetation folder path!\n")
      output$dsm_folder_name_area <- renderText("Invalid DSM vegetation folder path!")
      output$imgFiles.dsm_area <- renderText("Invalid DSM vegetation files path!")
    }
  } else if (!is.null(input$dsm_upload_area) && input$dsm_folder_check_area) {
    # Handle uploaded files
    dsm_folder_name_area <- "Uploaded Files"
    imgFiles.dsm_area <- input$dsm_upload_area$name
    imgFiles.dsm_area <- sapply(imgFiles.dsm_area, function(x) gsub(".tif", "", x))
    imgFiles.dsm_area_names(imgFiles.dsm_area)
    cat("Selected DSM vegetation folder:", dsm_folder_name_area, "\n")
    output$dsm_folder_name_area <- renderText(paste0("Selected DSM folder name: ", dsm_folder_name_area, "\n"))
    output$imgFiles.dsm_area <- renderText(paste0("Selected DSM files: ", imgFiles.dsm_area, "\n"))
  }
}, ignoreInit = TRUE)



## Load the PC files - field area PC
observeEvent(list(input$laz_folder_area.pc, input$laz_folder_area.pc_upload), {
  if (!is.null(input$laz_folder_area.pc) && !input$laz_folder_check_area) {
    # Handle selected files
    if (dir.exists(input$laz_folder_area.pc)) {
      laz_folder_name_area <- basename(input$laz_folder_area.pc)
      imgFiles.laz_area.pc <- list.files(path = input$laz_folder_area.pc, pattern="*.laz$", full.names = T) #get the laz only 
      imgFiles.laz_area.pc <- sapply(imgFiles.laz_area.pc, function(x) gsub(".laz", "", basename(x)))
      cat("Selected LAZ vegetation folder:", laz_folder_name_area, "\n")
      output$laz_folder_name_area <- renderText(paste0("Selected LAZ field area: ", laz_folder_name_area, "\n"))
      output$imgFiles.laz_area.pc <- renderText(paste0("Selected LAZ files: ", imgFiles.laz_area.pc, "\n"))
    } else {
      cat("Invalid LAZ vegetation folder path!\n")
      output$laz_folder_name_area <- renderText("Invalid LAZ vegetation folder path!")
      output$imgFiles.laz_area.pc <- renderText("Invalid LAZ vegetation files path!")
    }
  } else if (!is.null(input$laz_folder_area.pc_upload) && input$laz_folder_check_area) {
    # Handle uploaded files
    laz_folder_name_area <- "Uploaded Files"
    imgFiles.laz_area.pc <- input$laz_folder_area.pc_upload$name
    imgFiles.laz_area.pc <- sapply(imgFiles.laz_area.pc, function(x) gsub(".laz", "", x))
    cat("Selected LAZ vegetation folder:", laz_folder_name_area, "\n")
    output$laz_folder_name_area <- renderText(paste0("Selected LAZ field area: ", laz_folder_name_area, "\n"))
    output$imgFiles.laz_area.pc <- renderText(paste0("Selected LAZ files: ", imgFiles.laz_area.pc, "\n"))
  }
}, ignoreInit = TRUE)



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

clipped_dsm_paths_rv <- reactiveValues(clipped_dsm_paths = NULL)

img_area.dsm <- reactive({
  if(is.null(input$dsm_folder_area)) {return("No data found")}
  
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Running...")
    
    clipped_dsm_paths <- clipper.area.dsm(
      dsm_list = imgFiles.dsm_areas(),
      shape = indPlots_3_area(),
      n.core = n.core3()
    )
    
    print(paste("clipped_dsm_paths after clipper.area.dsm call:", clipped_dsm_paths))
    
    is_dsm_folder_check_area_selected <- isolate(input$dsm_folder_check_area)
    if (is_dsm_folder_check_area_selected) {
      
      clipped_dsm_paths_rv$clipped_dsm_paths <- clipped_dsm_paths
      
      cat("clipped_dsm_paths:", clippedDSMPath(), "\n")
      print(paste("clipped_dsm_paths_rv$clipped_dsm_paths after assignment:", clipped_dsm_paths_rv$clipped_dsm_paths))
   
       } else {
      DSMPath_val<-isolate(DSMPath((input$dsm_folder_area)))
      if (DSMPath_val) {
        clipped_dsm_paths_rv$clipped_dsm_paths <- clipped_dsm_paths
        
        cat("clipped_dsm_paths:", DSMPath(), "\n")
        print(paste("clipped_dsm_paths_rv$clipped_dsm_paths:", clipped_dsm_paths_rv$clipped_dsm_paths))
        
      } else {
        print("Invalid DSM path!")
      }
       }
    
  })
})


output$downloadClippedDSM <- downloadHandler(
  filename = function() {
    paste0("clipped_dsm_", Sys.Date(), ".zip")
  },
  content = function(file) {
    # Get all the clipped DSM paths
    clipped_dsm_paths <- clipped_dsm_paths_rv$clipped_dsm_paths
    
    # Check if the paths are not NULL
    if (!is.null(clipped_dsm_paths) && length(clipped_dsm_paths) > 0 && all(file.exists(clipped_dsm_paths))) {
      # Create a temporary folder to store the TIFF files
      temp_folder <- tempfile("clipped_dsm")
      dir.create(temp_folder)
      
      # Copy the TIFF files to the temporary folder
      file.copy(clipped_dsm_paths, temp_folder)
      
      # Create the ZIP file containing all the TIFF files
      zip(file, files = list.files(temp_folder, full.names = TRUE))
      
      # Remove the temporary folder
      unlink(temp_folder, recursive = TRUE)
    } else {
      stop("No clipped DSM files available for download.")
    }
  }
)


observe({
  # Check if the folder path is available
  folder_path_available <- !is.null(clipped_dsm_paths_rv$clipped_dsm_paths)
  
  # Enable/disable the button based on the folder path's availability
  if (folder_path_available) {
    shinyjs::enable("downloadClippedDSM")
  } else {
    shinyjs::disable("downloadClippedDSM")
  }
})


## Clip point cloud function
observeEvent(input$Clip_DSM, {
  
  if(is.null(input$dsm_folder_area)) {return("No data found")}
  img_area.dsm()
})

### Clipping Point Cloud 
# Add a reactive value to store the clipped_laz_paths
clipped_laz_paths_rv <- reactiveValues(clipped_laz_paths = NULL)

img_laz_area.pc <- reactive({
  if(is.null(input$laz_folder_area.pc)) {return("No data found")}
  
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Running...")
    
    clipped_laz_paths <- clipper.cloud.area.pc(
      cloud_list =  imgFiles.laz_areas.pc(),
      shape = indPlots_2_area(),
      n.core=n.core2()
    )
    
    print(paste("clipped_laz_paths after clipper.cloud.area.pc call:", clipped_laz_paths))
    
    is_laz_folder_check_area_selected <- isolate(input$laz_folder_check_area)
    if (is_laz_folder_check_area_selected) {
      
      clipped_laz_paths_rv$clipped_laz_paths <- clipped_laz_paths
      
      cat("clipped_laz_paths:", clippedLazPath(), "\n")
      print(paste("clipped_laz_paths_rv$clipped_laz_paths after assignment:", clipped_laz_paths_rv$clipped_laz_paths))
      
    } else {
      LazPath_val <- isolate(LazPath((input$laz_folder_area.pc)))
      if (LazPath_val) {
        clipped_laz_paths_rv$clipped_laz_paths <- clipped_laz_paths
        
        cat("clipped_laz_paths:", LazPath(), "\n")
        print(paste("clipped_laz_paths_rv$clipped_laz_paths:", clipped_laz_paths_rv$clipped_laz_paths))
        
      } else {
        print("Invalid LAZ path!")
      }
    }
  })
})

output$downloadClippedLAZ <- downloadHandler(
  filename = function() {
    paste0("clipped_laz_", Sys.Date(), ".zip")
  },
  content = function(file) {
    # Get all the clipped LAZ paths
    clipped_laz_paths <- clipped_laz_paths_rv$clipped_laz_paths
    
    # Check if the paths are not NULL
    if (!is.null(clipped_laz_paths) && length(clipped_laz_paths) > 0 && all(file.exists(clipped_laz_paths))) {
      # Create a temporary folder to store the LAZ files
      temp_folder <- tempfile("clipped_laz")
      dir.create(temp_folder)
      
      # Copy the LAZ files to the temporary folder
      file.copy(clipped_laz_paths, temp_folder)
      
      # Create the ZIP file containing all the LAZ files
      zip(file, files = list.files(temp_folder, full.names = TRUE))
      
      # Remove the temporary folder
      unlink(temp_folder, recursive = TRUE)
    } else {
      stop("No clipped LAZ files available for download.")
    }
  }
)


observe({
  # Check if the folder path is available
  folder_path_available <- !is.null(clipped_laz_paths_rv$clipped_laz_paths)
  
  # Enable/disable the button based on the folder path's availability
  if (folder_path_available) {
    shinyjs::enable("downloadClippedLAZ")
  } else {
    shinyjs::disable("downloadClippedLAZ")
  }
})


## Clip point cloud function
observeEvent(input$Clip_PC, {
  
  if(is.null(input$laz_folder_area.pc)) {return("No data found")}
  img_laz_area.pc()
})


# Render 3D plot - Point Cloud
output$PlotLaz_ind_3d <- renderRglwidget({
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Initializing view plot figures")
    tryCatch({

          cloud <- imgFiles.laz_veg_pc()
          
          k <- selected_k2()
          p <- indPlots_2()[k,]
          p_sp <- as(p, "Spatial")
          c_veg <- clip_rectangle(cloud, xleft = p_sp@bbox['x','min'],
                                  ytop = p_sp@bbox['y','max'],
                                  xright = p_sp@bbox['x','max'],
                                  ybottom = p_sp@bbox['y','min'])
          # Remove outliers in c_veg using IQR
          Q1_veg <- quantile(c_veg@data$Z, 0.25, na.rm=TRUE)
          Q3_veg <- quantile(c_veg@data$Z, 0.75, na.rm=TRUE)
          IQR_veg <- Q3_veg - Q1_veg
          kout <- 1.5
          c_veg <- c_veg[c_veg@data$Z >= Q1_veg - kout * IQR_veg & c_veg@data$Z <= Q3_veg + kout * IQR_veg,]

          # plot the cropped point cloud using the plot() function from lidR
          suppressWarnings(plot(c_veg))

          # add axis labels and adjust the plot view
          title3d(xlab = "X", ylab = "Y", zlab = "Z")
          bg3d("white")
          par3d(windowRect = c(0, 0, 800, 800))
          options(rgl.useNULL = TRUE)

          # convert the plot to an rgl widget for display in the Shiny app
          rglwidget()


          #output$PlotLaz_ind_file_3d <- renderText(paste0("Point cloud figure 3D ID: ", indPlots_col()$PlotID_sel[k]))

    }, error = function(e) {
      message("3D - No Point cloud Plot data available. Please upload a file.")
       return(NULL)
    })
  })
})


## Plot area DSM

output$rasterPlotDSM_area <- renderPlot({
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$dsm_folder_area) && !input$dsm_folder_check_area) {
        # Handle selected files
        if (dir.exists(input$dsm_folder_area)) {
          DSM_filename <- selected_dsm_file()
        } else {
          stop("DSM folder does not exist.")
        }
      } else if (!is.null(input$dsm_upload_area) && input$dsm_folder_check_area) {
        # Handle uploaded files
        DSM_filename <- selected_dsm_file()
      } else {
        stop("DSM folder input is NULL.")
      }
      
      message("names files selected:")
      print(DSM_filename)
      # Read the selected DSM file
      DSM1 <- raster::stack(DSM_filename)
      
      if (!is.null(selected_shapefile_path())) {
        shape <- sf::st_read(selected_shapefile_path())
        overlay_data(DSM1, shape)
        crs.result <- shp_test_crs(DSM1, shape)
        DSM1 <- crs.result$data
        shape <- crs.result$shp
        plot(DSM1, xlab="", ylab="")
        plot(shape, add = TRUE, col = "red")
      } else {
        plot(DSM1, xlab="", ylab="")
      }
      
    }, error = function(e) {
      message("No DSM data available. Please upload a file.")
      showNotification(paste("No DSM Plot data available. Please upload a file."), type = "error")
    })
  })
})



## Plot area PC

output$rasterPlotPC_area <- renderPlot({
  withProgress(message = "Running Time Series Data method", {
    setProgress(message = "Initializing view plot figures")
    tryCatch({
      if (!is.null(input$laz_folder_area.pc) && !input$laz_folder_check_area) {
        # Handle selected files
        if (dir.exists(input$laz_folder_area.pc)) {
          PC_filename <- selected_pc_file()
        } else {
          stop("Point cloud folder does not exist.")
        }
      } else if (!is.null(input$laz_folder_area.pc_upload) && input$laz_folder_check_area) {
        # Handle uploaded files
        PC_filename <- selected_pc_file()
      } else {
        stop("Point cloud folder input is NULL.")
      }
      
      message("names files selected figure:")
      print(PC_filename)
      
      cloud_s <- readLAS(files = PC_filename, select = "xyz")
      # Select points randomly to reach an homogeneous density of 1
      cloud_s <- decimate_points(cloud_s, homogenize(1,5))
      
      # Remove outliers in c_veg using IQR
      Q1_soil <- quantile(cloud_s@data$Z, 0.25, na.rm=TRUE)
      Q3_soil <- quantile(cloud_s@data$Z, 0.75, na.rm=TRUE)
      IQR_soil <- Q3_soil - Q1_soil
      kout <- 1.5
      cloud_s <- cloud_s[cloud_s@data$Z >= Q1_soil - kout * IQR_soil & cloud_s@data$Z <= Q3_soil + kout * IQR_soil,]
      
      if (!is.null(selected_shapefile_path_pc())) {
        shape <- sf::st_read(selected_shapefile_path_pc())
        crs.result <- shp_test_crs_pc(cloud_s, shape)
        cloud_s <- crs.result$data
        shape <- crs.result$shp
        plot(cloud_s@data$X, cloud_s@data$Y, xlab="", ylab="", main = "Nadir view - Point cloud distribution")
        plot(shape, add = TRUE, col = "red")
        
      } else {
        plot(cloud_s@data$X, cloud_s@data$Y, xlab="", ylab="", main = "Nadir view - Point cloud distribution")
      }
      
    }, error = function(e) {
      message("No Point cloud Plot data available. Please upload a file.")
      showNotification(paste("No LAZ soil plot data available. Please upload a file."), type = "error")
      
      return(NULL)
    })
  }) 
})



output$showWarning <- reactive({
  is.na(input$qthreshSoil_single)
})

outputOptions(output, "showWarning", suspendWhenHidden = FALSE)


}    
    
    
# Run shiny app ---------------------------------------------------------------------------
shinyApp(ui, server)


