#Client Results App V.2
#2025-08-29
#Updated from Client Results App V.1 (2025-02-19)
#Michaela Stoyel
#R V 4.4.2



#R Shiny app to provide an interactive user interface to clients for viewing results.
#App plots the results of the eDNA Krabappel client report on a map with colour coded site and sample results.
#Client may select file to view, toggle species and site/sample results, toggle site ID/n on and off, and
#export a screen shot of the map.

#V2 added:
#"select a file" button (import csv in the app, not before running it)
#"Show samples" check box plots a pie chart of sample results at each site. 


#Load Packages
library(dplyr)
library(tidyr)#for pivot_wider() in sample results
library(stringr)#to search partial strings
library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)#for sample result pie charts
library(shinyscreenshot)#to download map.


#Import client report

#HAVE A REMINDER POP UP FOR THESE
#Must manually confirm that all sample within the same site have identical site names (unique from all other sites)
#Make species names unique if multiple assays were run for the same species. 
#Remove T4 columns if tested in simplex, and "T4 (IPC), " from each species column if tested in duplex. 

#Import will automatically skip first row (app export information). 

#unfit samples are automatically removed from site call calculations and sample n per site (label) 


#__________________________________________________________________________________________________
#### Define User Interface####
#__________________________________________________________________________________________________
ui <- page_sidebar(
  title = h1(textOutput("case")),  #Case as page title, Use H1 header
  theme = bs_theme(preset = "flatly"), #set theme
  # Define sidebar inputs
  sidebar = sidebar(
    # Controls for map
    strong("Map Controls"), #bold side panel text
    
    #Select a file button to import client report csv
    fileInput("file", "Select a file",
              multiple = FALSE,
              accept = ".csv"),
    
    #create drop down selection for species (conditional upon file being selected)
    conditionalPanel(
      condition = "output.fileUploaded",
      selectizeInput(inputId = "spp",
                     label = "Select a species",
                     choices = NULL,
                     selected = NULL)
    ),
    
    #create a "show site ID" checkbox
    checkboxInput(inputId = "show_labels",
                  label = "Show Site ID",
                  value = FALSE),
    
    #create a "show sample results" checkbox (pie charts instead of site results)
    checkboxInput(inputId = "show_samples",
                  label = "Show Sample Results",
                  value = FALSE),
    
    # Add button to download map
    actionButton(inputId = "save_map",
                    label = "Download map")
    
  ),
  # Main panel content
  
  #Title: *selected spp* site detections
  htmlOutput("mapTitle"),
  
  # Add reactive text warning if blank contaminations were observed for that spp
  htmlOutput("warning"),
  
  #plot map
  leafletOutput("map"),
  
)

#To Add
#Toggle year (not yet, just load different datasets)

#__________________________________________________________________________________________________
#### Define Server ####
#__________________________________________________________________________________________________
server <- function(input, output, session) {
  
  #Import the selected client report csv
  raw_data <- reactive({
    req(input$file)
    raw_data <-read.csv(input$file$datapath, comment.char="#", stringsAsFactors=TRUE, skip=1)
    return(raw_data)
  })
    
  filtered_data <- reactive({
    req(raw_data())
    #re-save client report without blanks
    filtered_data <- raw_data() %>% filter(!str_detect(Sample.Type, "lank"))
    #average GPS coordinates by site. 
    mean_coord <- filtered_data %>% group_by(Site) %>% summarize(mean_lat = mean(Lat), mean_lon = mean(Long))
    #add mean_lat and mean_lon cols to filtered client_report. Fill mean value in all rows of that site
    filtered_data <- right_join(mean_coord, filtered_data, by = "Site")
    
    #FOR SIMPLEX, REMOVE T4 COLUMN
    #FOR DUPLEX, REMOVE "(IPC) T4" FROM SPECIES COLUMN IF PRESENT
    
    return(filtered_data)
  })
  
  
  #Update species selection choices when file is uploaded. 
  observe({
    req(filtered_data())
    spp_list <- unique(select(filtered_data(), contains("Species"))%>% sapply(levels)) #get unique entries in cols with "species"
    spp_list <- spp_list[spp_list != ""] #remove empty cells from species list
    updateSelectInput(session, "spp",
                      choices = spp_list,
                      selected = spp_list[1])
  })
  
  #Output check that file is uploaded (for conditional species selection panel)
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output,"fileUploaded", suspendWhenHidden =FALSE)
    
    
  #Add file name to page title
  output$case <- renderText({
    if(is.null(input$file)){
      return("Select a file")
    }
    paste(input$file$name)
  })

  
  #create reactive object table summarizing site calls (table with selected species data)
  #filter data to select site, mean_lat and long, selected assay results
  site_data_filt <- reactive({
    req(filtered_data())

    #grab name of column that contains selected species
    species <- filtered_data() %>% select(contains("Species"))
    selected_spp_col <-  NA
    selected_res_col <- NA

    for(i in 1:length(species)){
      if(any(species[,i] == input$spp)){#find species in any row
        selected_spp_col <- colnames(species[,i]) #find name of column containing selected species
        selected_res_col <- which(colnames(filtered_data())== selected_spp_col)+1 #get index of corresponding results column
        }
    }
    site_data_filt <- filtered_data() |>
      select(Site, mean_lat, mean_lon, as.numeric(paste(selected_res_col))) |>
      rename(Site = 1, mean_lat = 2, mean_lon = 3, sample_call = 4) |>
      #add col with numeric code for sample result (Detected = 100, Not Detected = 0, Inconclusive = 1, other = NA)
      mutate(sample_call_code = ifelse(sample_call == "Detected", 100, ifelse(sample_call == "Not Detected", 0, ifelse(sample_call == "Inconclusive", 1, NA))))|>
      #removes unfit before calculating site call and n
      na.omit()|>
      #summarize by site, calculate site calls
      group_by(Site) |> summarize(latitude = mean(mean_lat), longitude = mean(mean_lon), site_call_code = sum(sample_call_code), n = n()) |>
      mutate(site_call = ifelse(site_call_code == 0, "Not Detected", ifelse(site_call_code ==1, "Inconclusive", ifelse(site_call_code > 1, "Detected", NA))))
    #|>
    #|na.omit()#remove rows containing any NA (omit Unfits)
    return(site_data_filt)
    })

  
  
  #create a reactive object with sample calls at each site (table with selected species data)

  sample_data_filt <- reactive({
    req(filtered_data())
    #grab name of column that contains selected species
    species <- filtered_data() %>% select(contains("Species"))
    selected_spp_col <-  NA
    selected_res_col <- NA
    
    for(i in 1:length(species)){
      if(any(species[,i] == input$spp)){#find species in any row
        selected_spp_col <- colnames(species[,i]) #find name of column containing selected species
        selected_res_col <- which(colnames(filtered_data())== selected_spp_col)+1 #get index of corresponding results column
      }
    }
    sample_data_filt <- filtered_data() |>
      select(Site, mean_lat, mean_lon,  as.numeric(paste(selected_res_col))) |>
      rename(Site = 1, mean_lat = 2, mean_lon = 3, sample_call = 4) |>
      #count occurrences of each call
      group_by(Site, mean_lat, mean_lon, sample_call)|> summarize (N=n())|>
      #remove rows without sample call (not tested for that spp)
      filter(sample_call != "")|>
      #flip results from row to columns
      pivot_wider(names_from = sample_call, values_from = N)
    #add columns for all results calls, if not already present
    if(!("Detected" %in% colnames(sample_data_filt))){sample_data_filt <- mutate(sample_data_filt, "Detected" = NA)}
    if(!("Inconclusive" %in% colnames(sample_data_filt))){sample_data_filt <- mutate(sample_data_filt, "Inconclusive" = NA)}
    if(!("Not Detected" %in% colnames(sample_data_filt))){sample_data_filt <- mutate(sample_data_filt, "Not Detected" = NA)}
    #replace NA with 0
    sample_data_filt[is.na(sample_data_filt)] <- 0
    
    return(sample_data_filt)
  })

  
  #create map title output (spp selected + site or sample data)
  output$mapTitle <- renderText({
    if(input$show_samples){paste(h3(strong(input$spp), " sample detections"))}
    else{paste(h3(strong(input$spp), " site detections"))}
    })

  #create contamination warning
  output$warning <-renderText({
    req(raw_data())
    #Remove Blanks. save a pop-up warning if blanks were contaminated.
    blanks <- raw_data() %>% filter(str_detect(Sample.Type, "lank")) #pull sample types containing lank (blank or Blank)
    contamination <- data.frame(contam_spp = NA, contam_site = NA)
    #loop through species (results columns for each assay)
    #if the column is not all "Not Detected" then save the contamination for that species (and site if applicable)
    for(n in 1:(ncol(select(blanks, contains("sample.result")))-1)){ #for each results column (assay) in blanks. Drop last (T4) col
      sample_result <- select(blanks, contains("sample.result")) %>% select(all_of(n)) #select the n-th sample results col.
      for(m in 1:nrow(sample_result)){#loop through rows of column
        if(sample_result[m,] != "Not Detected"){
          contamination <- contamination %>% add_row(contam_spp = blanks[m,(which(colnames(blanks)== colnames(sample_result))-1)],
                                                     contam_site = blanks$Site[m])
          #add a row to contamination, fill contam_spp with the value in blanks row m (row of loop)
          #and col 1 before the name of the column we're looking at (saved in sample_result)
          #fill contam_site with the value in blanks row m, column "Site"
        }
      }
    }

    contam_blanks <- contamination %>% filter(contam_spp==input$spp) %>% select(contam_site)
    if(nrow(contam_blanks)>0){
      text <-paste(tags$span(style="color:red", "Warning, contamination for ", input$spp," was observed in the follwing blanks: ", paste0(contam_blanks[,1:length(contam_blanks)], collapse = ", ")))
    }
    else{text <- ""}
    paste(text)
  })
  


  #create leaflet basemap
  output$map <-renderLeaflet({
    req(filtered_data())

    #pull coordinates for mapping (after removing blanks so no NA coordinates)
    coord <- c(min(filtered_data()$Lat), max(filtered_data()$Lat), min(filtered_data()$Long), max(filtered_data()$Long))

    map<-leaflet() |>
      setView(lng = mean(coord[3:4]), #define center of map (mean of min/max lat and lon)
              lat = mean(coord[1:2]),
              zoom = 6) |> # set by trial and error to fill window. approx atl province size.
      addProviderTiles(provider = providers$OpenStreetMap, group = "OSM") |>
      addScaleBar(position = "bottomright")
  })

  outputOptions(output, "map", suspendWhenHidden = FALSE) #need for displaying points for map on load
  #if map is not on first page, need this line to make the points load initially, not just when spp is changed.

  #create reactive value for map proxy, labels visivle and samples visible
  values <- reactiveValues(
    map_proxy = NULL,
    labels_visible = NULL,
    samples_visible = NULL
  )
  
  #create map proxy after map is rendered (to avoid reloaded map with each selection)
  observe({
    values$map_proxy <-leafletProxy(mapId = "map")
  })
  
  #add site data markers with observe event
  observeEvent(!is.null(input$spp),{
    req(site_data_filt(), values$map_proxy)
    
    call_col <- colorFactor(palette = c("palegreen2","khaki2", "ivory3"), levels = c("Detected", "Inconclusive", "Not Detected"))
    
    #add site call markers
    values$map_proxy |> 
      clearMarkers() |>
      clearMinicharts()|>
      clearControls() |>
      addCircleMarkers(data = site_data_filt(),
                       radius = 10,
                       color = "black",#border colour, could also set to ~call_col(site_call),
                       fillColor = ~call_col(site_call), #Fill colour, based on call
                       opacity = 1, #border opacity
                       weight = 1, #border weight
                       stroke = TRUE, #add border
                       fillOpacity = 1, #fill opacity
                       label = ~paste0(Site, " (n=",n,")"), #displays site name when hovering
      ) |>
      #Add legend (for site Calls)
      addLegend(pal = call_col,
                values = site_data_filt()$site_call,
                position = "topright")
    
    #add labels if checkbox is already selected
    if(input$show_labels){
      values$map_proxy |>
        addLabelOnlyMarkers(data = site_data_filt(),
                            label = ~paste0(Site, " (n=",n,")"),
                            layerId = paste0("label_",seq_len(nrow(site_data_filt()))),
                            labelOptions = labelOptions(noHide= T, 
                                                        #textOnly = T,
                                                        direction="right",
                                                        offset=c(10,0))#to not cover marker
                            )
      values$labels_visible <- TRUE
    }else{values$labels_visible <- FALSE}
    
    #add sample pie charts if already selected
    if(input$show_samples){
      values$map_proxy |>
        addMinicharts(lng=sample_data_filt()$mean_lon,
                      lat = sample_data_filt()$mean_lat,
                      chartdata = sample_data_filt()[,c("Detected", "Inconclusive", "Not Detected")],#Currently Unfit (or any other) are not plotted
                      type= "pie",
                      width = 20, height = 20,
                      colorPalette = call_col(c("Detected", "Inconclusive", "Not Detected")),
                      legend=FALSE
                      ) 
      values$samples_visible <- TRUE
    }else{values$samples_visible <- FALSE}
    
  })
  
  #toggle labels with checkbox
  observeEvent(input$show_labels,{
    req(site_data_filt(), values$map_proxy)
    
    #add labels if checkbox is selected and labels are not already present
    if(input$show_labels && !values$labels_visible){
      values$map_proxy |>
        addLabelOnlyMarkers(data = site_data_filt(),
                            label = ~paste0(Site, " (n=",n,")"),
                            layerId = paste0("label_",seq_len(nrow(site_data_filt()))),
                            labelOptions = labelOptions(noHide= TRUE, 
                                                        #textOnly = T,
                                                        direction="right",
                                                        offset=c(10,0))#to not cover marker
                            )
      values$labels_visible <- TRUE
    } 
    #remove labels if present and box is deselected
    else if (!input$show_labels && values$labels_visible){
      label_ids <- paste0("label_",seq_len(nrow(site_data_filt())))
      values$map_proxy|>
        removeMarker(layerId = label_ids)
      values$labels_visible <- FALSE
    }
    
  }, ignoreInit = T)#prevents the observeEvent from running when the app first loads
  
  #toggle sample pie charts with check box
  observeEvent(input$show_samples,{
    req(sample_data_filt(), values$map_proxy)
    
    call_col <- colorFactor(palette = c("palegreen2","khaki2", "ivory3"), levels = c("Detected", "Inconclusive", "Not Detected"))
    
    #add samples if checkbox is selected and not already present
    if(input$show_samples && !values$samples_visible){
      values$map_proxy|>
        addMinicharts(lng=sample_data_filt()$mean_lon,
                      lat = sample_data_filt()$mean_lat,
                      chartdata = sample_data_filt()[,c("Detected", "Inconclusive", "Not Detected")],#Currently Unfit (or any other) are not plotted
                      type= "pie",
                      width = 20, height = 20,
                      colorPalette = call_col(c("Detected", "Inconclusive", "Not Detected")),
                      legend=FALSE
                      )
      values$samples_visible <- TRUE
    }
    #remove sample pie charts if present and box is deselected
    else if (!input$show_samples && values$samples_visible){
      #sample_ids <- paste0("samples_", seq_len(nrow(site_data_filt())))
      values$map_proxy|>
        clearMinicharts()
      values$samples_visible <- FALSE
    }
  }, ignoreInit = T)#prevents the observeEvent from running when the app first loads
  

  
  ### Output to download ###
  # Code to export figure upon clicking download button
  observeEvent(input$save_map, {
    screenshot(selector = "#map") #select only map to download, or use id="map"
  })

}

#### Run app ####
shinyApp(ui, server)

