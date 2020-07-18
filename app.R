
#### Sampo Vesanen's Master's thesis statistical tests and visualisation #######


# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 18.7.2020
#
# This is an interactive tool for analysing the results of my research survey.


# 1 Initialise -----------------------------------------------------------------

# Libraries
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(shiny)
library(shinythemes)
library(ggplot2)
library(Rmisc)
library(tidyr)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(shinyjs)
library(ggiraph)
library(widgetframe)
library(rgeos)
library(classInt)
library(mapproj)
library(shinyWidgets)
library(grid)
library(ggsn)


# Python prepared data directories
datapath <- "pythonshinyrecords.csv"
postal_path <- "pythonshinypostal.csv"

# Spatial data paths
suuraluepath <- "PKS_suuralue.kml"
munsclippedpath <- "hcr_muns_clipped.shp"
munspath <- "hcr_muns_sea.shp"

# CSS data
csspath <- "thesis_stats_vis_style.css"
jspath <- "thesis_stats_vis_script.js"

# Source functions and postal code variables
source("app_funcs.R")



#### 2 Preparation ------------------------------------------------------------- 

# These variables are used to subset dataframe thesisdata inside ShinyApp
continuous <- c("parktime", "walktime") 
ordinal <- c("likert", "parkspot", "timeofday", "artificial", "ykr_zone", 
             "subdiv")
supportcols <- c("id", "timestamp", "ip")
int_cols <- c("n", "Min", "Max", "NA")


# Read in csv data. Define column types. Name factor levels. Determine order of 
# factor levels for plotting. Lastly, remove column "X"
thesisdata <- read.csv(file = datapath,
                       header = TRUE, 
                       sep = ",",
                       colClasses = c(timestamp = "POSIXct", zipcode = "character", 
                                      ip = "character", timeofday = "factor", 
                                      parkspot = "factor", likert = "factor", 
                                      artificial = "factor", ykr_zone = "factor", 
                                      subdiv = "factor"),
                       encoding = "UTF-8", 
                       stringsAsFactors = TRUE) %>%
  
  dplyr::mutate(parkspot = dplyr::recode(
                  parkspot,
                  `1` = "On the side of street",
                  `2` = "Parking lot",
                  `3` = "Parking garage",
                  `4` = "Private or reserved",
                  `5` = "Other"),
                
                likert = dplyr::recode(
                  likert, 
                  `1` = "Extremely familiar",
                  `2` = "Moderately familiar",
                  `3` = "Somewhat familiar",
                  `4` = "Slightly familiar",
                  `5` = "Not at all familiar"),
                
                timeofday = dplyr::recode(
                  timeofday, 
                  `1` = "Weekday, rush hour",
                  `2` = "Weekday, other than rush hour",
                  `3` = "Weekend",
                  `4` = "Can't specify, no usual time"),
                
                artificial = forcats::fct_relevel(
                  artificial,
                  c("Fully built",
                    "Predominantly built", 
                    "Moderately built",
                    "Some built", 
                    "Scarcely built")),
                
                # SYKE does not provide official translations for 
                # "Yhdyskuntarakenteen vyohykkeet".
                ykr_zone = forcats::fct_relevel(
                  ykr_zone, 
                  c("keskustan jalankulkuvyohyke", 
                    "keskustan reunavyohyke", 
                    "alakeskuksen jalankulkuvyohyke", 
                    "intensiivinen joukkoliikennevyohyke", 
                    "joukkoliikennevyohyke",
                    "autovyohyke", "novalue"))) %>%
  dplyr::select(-X)



#### 3 Prepare layers ----------------------------------------------------------

#### 3.1 Subdivisions ----------------------------------------------------------

# Prepare a context map for to visualise currently active areas in analysis
# ShinyApp.
app_crs <- sp::CRS("+init=epsg:3067")

suuralue <- rgdal::readOGR(suuraluepath, 
                           use_iconv = TRUE, 
                           encoding = "UTF-8", 
                           stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)


# Preserve suuralue attribute data by merging fortified data with the original 
# dataframe
suuralue_f <- 
  merge(ggplot2::fortify(suuralue), 
        as.data.frame(suuralue), 
        by.x = "id", 
        by.y = 0)

# Align area names with thesisdata$subdiv
levels(suuralue_f$Name) <- c("Vantaa Aviapolis", "Helsinki Southern",
                             "Vantaa Hakunila", "Helsinki Eastern",
                             "Helsinki Southeastern", "Kauniainen",
                             "Helsinki Central", "Vantaa Kivisto",
                             "Helsinki Northeastern", "Vantaa Koivukyla",
                             "Vantaa Korso", "Helsinki Western",
                             "Vantaa Myyrmaki", "Helsinki Ostersundom",
                             "Helsinki Northern", "Espoo Pohjois-Espoo",
                             "Espoo Suur-Espoonlahti", "Espoo Suur-Kauklahti",
                             "Espoo Suur-Leppavaara", "Espoo Suur-Matinkyla",
                             "Espoo Suur-Tapiola", "Vantaa Tikkurila",
                             "Espoo Vanha-Espoo")

thesisdata$subdiv <- factor(thesisdata$subdiv, levels = sort(levels(thesisdata$subdiv)))
suuralue_f$Name <- factor(suuralue_f$Name, levels = sort(levels(suuralue_f$Name)))

# Reorder dataframe by subdivision
suuralue_f <- suuralue_f[order(suuralue_f$Name), ]



#### 3.2 Municipality borders --------------------------------------------------

# Get municipality borders. Fortify SP DataFrame for ggplot. Remove unnecessary
# columns to save memory.
# Shapefile data is Regional population density 2012, Statistics Finland.
# http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226.
muns_clipped <- 
  rgdal::readOGR(munsclippedpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# Fortify SP DataFrame for ggplot. Remove unnecessary columns to save memory
muns_clipped_f <- merge(ggplot2::fortify(muns_clipped), 
                        as.data.frame(muns_clipped), 
                        by.x = "id", 
                        by.y = 0) %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))


#### 3.3 Annotation ------------------------------------------------------------

# Annotate municipalities and subdivisions in ggplot2
muns_cntr <- GetCentroids(muns_clipped_f, "nimi", "nimi")
subdiv_cntr <- GetCentroids(suuralue_f, "Name", "Name")

# Manually set better location for the annotation of Helsinki. Utilise
# custom infix operators.
muns_cntr["Helsinki", "lat"] %+=% 2000
muns_cntr$label <- c("Espoo", "Helsinki", "Kauniainen", "Vantaa")

# name labels here so that all the reordering doesn't mix up stuff
subdiv_cntr$label <- unique(suuralue_f$Name)

# name labels here so that all the reordering doesn't mix up stuff. Remove
# munnames from subdiv annotations
subdiv_cntr$label <- gsub(".* ", "", unique(suuralue_f$Name)) 
rownames(subdiv_cntr) <- gsub(".* ", "", unique(suuralue_f$Name))

# Manually move labels for Espoonlahti and Southeastern as we are going to use
# some y axis limits when drawing the map. For Espoonlahti, take lat of Helsinki
# Southern. For long, take Pohjois-Espoo's. For Southeastern, take long of Korso.
# Remove subdiv label for Kauniainen.
subdiv_cntr["Suur-Espoonlahti", "lat"] <- subdiv_cntr["Southern", "lat"]
subdiv_cntr["Suur-Espoonlahti", "long"] <- subdiv_cntr["Pohjois-Espoo", "long"]
subdiv_cntr["Southeastern", "lat"] <- subdiv_cntr["Southern", "lat"] + 1500
subdiv_cntr["Southeastern", "long"] <- subdiv_cntr["Korso", "long"]
subdiv_cntr["Southern", "lat"] %+=% 3000
subdiv_cntr["Ostersundom", "lat"] %-=% 500
subdiv_cntr["Ostersundom", "long"] %-=% 400
subdiv_cntr["Suur-Matinkyla", "lat"] %-=% 500



### 4 Interactive map for ShinyApp --------------------------------------------- 
# Created with the help of:
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html

zips <- unique(thesisdata$zipcode)

# Get postal code area data calculated in Python. It contains some interesting
# variables for visualisation. Select essential columns and multiply column 
# "artificial" with 100 for easier to view plotting
postal <- 
  read.csv(file = postal_path,
           header = TRUE,
           sep = ",",
           colClasses = c(zipcode = "factor", kunta = "factor"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(c(2, 3, 6, 108:121)) %>%
  dplyr::mutate(artificial = artificial * 100)

# Create column which reports the largest ykr zone in each postal code area
largest_ykr <- colnames(postal[, 5:11])[apply(postal[, 5:11], 1, which.max)]
largest_ykr <- gsub("ykr_", "", largest_ykr)
largest_ykr_no <- as.numeric(apply(postal[, 5:11], 1, max)) * 100
postal <- cbind(postal, largest_ykr = paste(largest_ykr, largest_ykr_no))

# "postal" geometries are in well-known text format. Some processing is needed to
# utilise these polygons in R. readWKT() is of rgeos
geometries <- lapply(postal[, "geometry"], "readWKT", p4s = app_crs) 
sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(postal[, 1]))
row.names(postal) <- postal[, 1]

data <- SpatialPolygonsDataFrame(
  SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)), 
                  proj4string = app_crs), data = postal)

data_f <- merge(ggplot2::fortify(data), as.data.frame(data), by.x = "id", 
                by.y = 0)

# Get municipality borders from shapefile.
muns <- 
  rgdal::readOGR(munspath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# Fortify muns for ggplot. Remove unnecessary columns to save memory
muns_f <- 
  merge(fortify(muns), as.data.frame(muns), by.x = "id", by.y = 0) %>%
  dplyr::select(-c(namn, vaestontih, km2, vakiluku))



#### 5 Analysis ShinyApp -------------------------------------------------------

# This ShinyApp is a versatile tool to study the thesis survey data. One can
# choose parameters with lots of freedom and exclude options as seen fit.

server <- function(input, output, session){
  
  #### 5.1 Listener functions --------------------------------------------------
  
  # 5.1.1 Listen to clear subdivs button. Resetting uses library shinyjs -------
  observeEvent(input$resetSubdivs, {
    reset("subdivGroup")
  })
  
  observeEvent(input$resetParkWalk, {
    reset("parktime_max")
    reset("walktime_max")
  })
  
  
  #### 5.1.2 Reactive data -----------------------------------------------------
  
  # currentdata(), currentpostal() and current_data_f() are reactive objects
  # made to keep track of changes the Shiny application. Most of the app uses
  # only currentdata(), but the interactive map utilises all of them. 
  
  # Detect user setting for maximum parktime and walktime.
  # currentdata() is the currently active rows of the original thesisdata 
  # DataFrame. Use currentdata() in the rest of the application to not interfere
  # with the original dataset.
  currentdata <- reactive(
    
    # Use ! negation to remove checkGroup and subdivGroup inputs from 
    # thesisdata.
    dplyr::filter(thesisdata,
                  !(!!rlang::sym(input$expl)) %in% input$checkGroup,
                  !subdiv %in% input$subdivGroup,
                  parktime <= input$parktime_max,
                  walktime <= input$walktime_max)
  )
  
  
  # currentpostal() recalculates, when needed, new answer counts, means, and 
  # medians for the values currently active in currentdata(). This is needed to 
  # make the interactive map tooltip values responsive to changes. In this phase,
  # we calculate the required data, the actual mapping part is in current_data_f()
  currentpostal <- reactive({
    
    currentdata <- currentdata()
    
    # tidyr::complete() helps find missing zipcodes and give them n=0
    result <- postal %>%
      dplyr::mutate(answer_count = currentdata %>% 
                      dplyr::group_by(zipcode) %>% 
                      dplyr::tally() %>% 
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(n),
                    
                    parktime_mean = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(parktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    parktime_median = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(median(parktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    walktime_mean = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(walktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    walktime_median = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(median(walktime)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull(),
                    
                    # this enables turning off subdivs and the result being
                    # visible on interactive map
                    artificial = currentdata %>%
                      dplyr::group_by(zipcode) %>%
                      dplyr::summarise(mean(artificial_vals)) %>%
                      tidyr::complete(zipcode = zips, fill = list(n = NA)) %>%
                      dplyr::pull()) %>%
      
      dplyr::mutate(artificial = artificial * 100)
    
    result$parktime_mean <- sapply(result[, "parktime_mean"], round, 2)
    result$walktime_mean <- sapply(result[, "walktime_mean"], round, 2)
    result
  })
  
  
  # current_data_f() produces the currently needed interactive map out of the
  # data contained in currentpostal(). This is a copy of the code above, seen
  # in "Interactive map for ShinyApp". Please See code comments in the original.
  current_data_f <- reactive({
    
    currentpostal <- currentpostal()
    
    geometries <- lapply(currentpostal[, "geometry"], "readWKT", p4s = app_crs)
    sp_tmp_ID <- mapply(sp::spChFIDs, geometries, as.character(currentpostal[, 1]))
    row.names(currentpostal) <- currentpostal[, 1]
    
    data <- sp::SpatialPolygonsDataFrame(
      sp::SpatialPolygons(unlist(lapply(sp_tmp_ID, function(x) x@polygons)),
                          proj4string = app_crs), data = currentpostal)
    
    data_f <- merge(ggplot2::fortify(data), 
                    as.data.frame(data), 
                    by.x = "id", 
                    by.y = 0)
    data_f
  })
  
  
  observe({
    # 5.1.3 Detect changes in selectInput to modify available check boxes ------
    x <- input$expl
    
    updateCheckboxGroupInput(
      session, 
      "checkGroup", 
      label = NULL, 
      choiceNames = levels(thesisdata[, x]),
      choiceValues = levels(thesisdata[, x]),)
    
    
    # 5.1.4 Determine availability of barplot ----------------------------------
    
    # aka availability of "Distribution of ordinal variables"
    available <- c("likert", "parkspot", "timeofday", "artificial", "ykr_zone", 
                   "subdiv")
    updateSelectInput(
      session, 
      "barplot",
      label = NULL,
      choices = available[!available == x])
    
    
    # 5.1.5 Do not allow selection of all checkboxes in Jenks ------------------
    if(length(input$kunta) == 3) {
      threevalues <<- input$kunta
    }
    if(length(input$kunta) > 3) {
      
      updateCheckboxGroupInput(
        session, 
        "kunta", 
        selected = threevalues)
    }
    
    
    # 5.1.6 A clumsy implementation to listen for too large jenks breaks -------
    inputpostal <- postal[!postal$kunta %in% c(input$kunta), ]
    
    if(input$karttacol == "jenks_artificial") {
      datacol <- "artificial"
    } else if (input$karttacol == "jenks_walk_median") {
      datacol <- "walktime_median"
    } else if (input$karttacol == "jenks_park_median") {
      datacol <- "parktime_median"
    } else if (input$karttacol == "jenks_answer_count") {
      datacol <- "answer_count"
    } else if (input$karttacol == "jenks_walk_mean") {
      datacol <- "walktime_mean"
    } else if (input$karttacol == "jenks_park_mean") {
      datacol <- "parktime_mean"
    }
    
    # Only test classIntervals() and change SliderInput value if statement holds.
    # Suppress warnings in the test.
    if(nrow(inputpostal) > 1) {
      classes_test <- suppressWarnings(
        classInt::classIntervals(inputpostal[, datacol], n = input$jenks_n, style = "jenks"))
      
      # object returned from classIntervals() has an attribute nobs which I
      # use to detect cases where too large input$jenks_n is inputted to
      # CreateJenksColumn() function
      if(attributes(classes_test)$nobs < input$jenks_n) {
        updateSliderInput(session,
                          "jenks_n",
                          value = attributes(classes_test)$nobs)
      }
    }
  })
  
  
  
  #### 5.2 Descriptive statistics ----------------------------------------------
  output$descri <- renderTable({
    
    # Vital variables
    resp_col <- input$resp
    expl_col <- input$expl
    thisFormula <- as.formula(paste(resp_col, '~', expl_col))
    
    # In each output, define reactive variable as "inpudata". According to
    # this Stack Overflow answer, it prevents errors down the line
    # https://stackoverflow.com/a/53989498/9455395
    
    # Reminder: the reactive currentdata() is called to keep track of maximum
    # allowed parktime and walktime values, and see changes in input$checkGroup 
    # and input$subdivGroup
    inputdata <- currentdata()
    
    # Take subdiv checkbox group into account
    inputdata <- inputdata[, !names(inputdata) %in% supportcols]
    response <- inputdata[[resp_col]]
    
    # Basic descriptive statistics
    desc <- onewaytests::describe(thisFormula, inputdata)
    
    ### Mean and standard error
    # Clumsily calculate mean, so that we can preserve column names in the next
    # phase. Adapt code from: https://stackoverflow.com/a/41029914/9455395
    stder <- aggregate(
      thisFormula, 
      data = inputdata,
      FUN = function(x) c(mean = mean(x), "Std.Error" = plotrix::std.error(x)))
    
    # Remove column "mean" 
    stder <- subset(stder[[2]], select = -mean)
    desc <- cbind(desc, stder)
    
    # Confidence intervals for mean
    confs <- aggregate(
      thisFormula, 
      data = inputdata, 
      FUN = function(x) c("CI for mean, lower bound" = Rmisc::CI(x)[[3]], 
                          "CI for mean, upper bound" = Rmisc::CI(x)[[1]]))
    confs <- confs[[2]]
    desc <- cbind(desc, confs)
    
    # Reorder to SPSS descriptive statistics order
    desc <- desc[c("n", "Median", "Mean", "Std.Dev", "Std.Error", 
                   "CI for mean, lower bound", "CI for mean, upper bound", "Min", 
                   "Max", "25th", "75th", "Skewness", "Kurtosis", "NA")]
    
    # Add the total row. We will add total values for all columns in this manner,
    # using "response" which is all currently active parktime or walktime values.
    vect <- c()
    vect[1] <- length(response)
    vect[2] <- median(response)
    vect[3] <- mean(response)
    vect[4] <- sd(response)
    vect[5] <- plotrix::std.error(response)
    vect[6] <- Rmisc::CI(response)[[3]]
    vect[7] <- Rmisc::CI(response)[[1]]
    vect[8] <- min(response)
    vect[9] <- max(response)
    vect[10] <- quantile(response)[2]
    vect[11] <- quantile(response)[4]
    vect[12] <- moments::skewness(response)
    vect[13] <- moments::kurtosis(response)
    vect[14] <- sum(is.na(response))
    
    # Add "vect" to "desc" as a new row, then name the new row. Finally, set
    # specific columns as integer to prevent useless decimal places in 
    # tableOutput
    desc <- rbind(desc, vect)
    row.names(desc)[nrow(desc)] <- "Total" #name the last row
    desc[int_cols] <- sapply(desc[int_cols], as.integer)
    desc_out <<- desc # Result to global environment to enable download
    
    # Render descriptive statistics table
    desc
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE,
  digits = 2)
  
  
  #### 5.2.1 Download descriptive statistics ----
  output$dl_descri <- downloadHandler(
    filename = paste("descriptives_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".csv",
                     sep = ""),
    
    content = function(file) {
      write.csv(desc_out, file)
    }
  )
  
  
  #### 5.3 Histogram for parktime or walktime ----------------------------------
  output$hist <- renderggiraph({
    
    resp_col <- input$resp
    expl_col <- input$expl
    binwidth <- input$bin
    
    inputdata <- currentdata()
    resp_vect <- inputdata[[resp_col]] # for vertical line labels
    
    p <- ggplot(inputdata, aes(x = !!sym(resp_col))) + 
      geom_histogram(color = "black", fill = "grey", binwidth = binwidth) +
      xlab(paste(resp_col, "(min)")) +
      
      # Vertical lines for mean and median, respectively. Also display exact
      # values with annotate_custom() and textGrobs.
      geom_vline(aes(xintercept = mean(!!sym(resp_col)),
                     color = "mean"),
                 linetype = "longdash", 
                 size = 1) +
      annotation_custom(
        grob = grid::textGrob(label = round(mean(resp_vect), 2), 
                              hjust = -0.3,
                              vjust = 1.25, 
                              gp = grid::gpar(cex = 1.2, col = "red")),
        ymin = 0,
        ymax = 0,
        xmin = round(mean(resp_vect), 2),
        xmax = round(mean(resp_vect), 2)) +
      
      geom_vline(aes(xintercept = median(!!sym(resp_col)),
                     color = "median"),
                 linetype = "longdash", 
                 size = 1) +
      annotation_custom(
        grob = grid::textGrob(label = round(median(resp_vect), 2), 
                              hjust = 2,
                              vjust = 1.25, 
                              gp = grid::gpar(cex = 1.2, col = "blue")),
        ymin = 0,
        ymax = 0,
        xmin = round(median(resp_vect), 2),
        xmax = round(median(resp_vect), 2)) +
      
      # This is kernel density estimate, a smoothed version of the histogram.
      # Usually geom_density() sets the scale for y axis, but here we will
      # continue using count/frequency. This requires some work on our behalf.
      # Idea from here: https://stackoverflow.com/a/27612438/9455395
      geom_density(aes(y = ..density.. * (nrow(inputdata) * binwidth),
                       color = "kernel density\nestimate"),
                   show.legend = FALSE,
                   adjust = binwidth) +
      
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 15),
            legend.spacing.y = unit(0.3, "cm"),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 15),
            text = element_text(size = 13)) +
      
      # Build legend, override colors to get visible color for density. Also,
      # override linetype to get a solid line for density.
      scale_color_manual(name = paste("Legend for\n", resp_col, sep = ""), 
                         values = c(median = "blue", 
                                    mean = "red", 
                                    "kernel density\nestimate" = alpha("black", 0.4))) +
      guides(color = guide_legend(
        override.aes = list(color = c("darkgrey", "red", "blue"),
                            linetype = c("solid", "longdash", "longdash")))) +
      
      # Conditional histogram bar labeling. No label for zero
      stat_bin(binwidth = binwidth, 
               geom = "text", 
               aes(label = ifelse(..count.. > 0, ..count.., "")), 
               vjust = -0.65)
    
    
    # Prepare the downloadable histogram. "hist_out" is brought global environment 
    # for download. Use larger fonts.
    hist_out <- LabelBuilder(p, input$expl, input$checkGroup, input$subdivGroup)
    hist_out <<- 
      hist_out + 
      theme(legend.title = element_text(size = 23),
            legend.text = element_text(size = 22),
            axis.text = element_text(size = 19),
            axis.title = element_text(size = 22))
    
    # Render histogram
    ggiraph(code = print(p),
            width_svg = 16.7,
            height_svg = 6)
  })
  
  
  #### 5.3.1 Download histogram ----
  output$dl_hist <- downloadHandler(
    filename = paste("hist_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     "binw", input$bin, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = hist_out, file, height = 10, width = 26, dpi = 150)
    }
  )
  
  
  #### 5.4 Barplot -------------------------------------------------------------
  output$barplot_ord <- renderggiraph({
    
    # See distribution of ordinal variables through a grouped bar plot
    expl_col <- input$expl
    barplotval <- input$barplot
    yax <- paste("count of", barplotval)
    
    # Listen to user choices
    inputdata <- currentdata()
    
    # Plot maximum y tick value. Use dplyr to group the desired max amount.
    # In dplyr, use !!as.symbol(var) to signify that we are using variables
    # as column names
    maximum <- inputdata %>% 
      dplyr::group_by(!!as.symbol(expl_col), !!as.symbol(barplotval)) %>% 
      dplyr::summarise(amount = length(!!as.symbol(barplotval))) %>% 
      dplyr::top_n(n = 1) %>%
      dplyr::pull(amount) %>%
      max()
    
    if (maximum <= 200) {
      tick_interval <- 50
    } else {
      tick_interval <- 200
    }
    
    tooltip_content <- paste0("<div id='app-tooltip'>",
                              "<div>n=%s</div></div>")
    
    plo <- 
      ggplot(inputdata, aes(x = get(expl_col), 
                            y = factor(get(barplotval)), 
                            fill = get(barplotval))) +
      
      # Setting width and position_dodge adds space between bars. Add a simple
      # tooltip.
      geom_bar_interactive(aes(y = stat(count),
                               tooltip = sprintf(tooltip_content,
                                                 ..count..)),
                           width = 0.8,
                           position = position_dodge(width = 0.9)) +
      
      scale_y_continuous(breaks = seq(0, maximum, by = tick_interval),
                         expand = expansion(mult = c(0, .1))) +
      xlab(expl_col) +
      ylab(yax) +
      labs(fill = barplotval) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 15),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 15))
    
    # Use RColorBrewer color scale. Paired has 12 set colors, interpolate if
    # there are more values to map than that.
    legendnames <- unique(inputdata[[barplotval]])
    plo <- InterpolateGgplotColors(plo, legendnames, 12, "Paired")
    
    
    # Prepare the downloadable barplot. "barplot_out" is brought global 
    # environment for download. Use larger fonts.
    
    # Labeling inactive checkGroup values is not meaningful in the barplot
    barplot_out <- LabelBuilder(plo, "", c(), input$subdivGroup)
    barplot_out <<- 
      barplot_out + 
      theme(legend.title = element_text(size = 22),
            legend.text = element_text(size = 21),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 22))
    
    # Render barplot
    ggiraph(code = print(plo),
            width_svg = 16.7,
            height_svg = 6)
  })
  
  
  #### 5.4.1 Download barplot ----
  output$dl_barplot <- downloadHandler(
    filename = paste("barplot_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$expl, "-", input$barplot, "_",
                     format(Sys.time(), "%d-%m-%Y"),
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = barplot_out, file, height = 10, width = 26, dpi = 150)
    }
  )
  
  
  #### 5.5 Boxplot -------------------------------------------------------------
  output$boxplot <- renderggiraph({
    
    expl_col <- input$expl
    resp_col <- input$resp
    thisFormula <- as.formula(paste(resp_col, '~', expl_col))
    
    # Listen to user choices
    inputdata <- currentdata()
    legendnames <- levels(unique(inputdata[[expl_col]]))
    inputdata <- CalcBoxplotTooltip(inputdata, resp_col, expl_col)
    
    tooltip_content <- paste0("<div style='font-size: 12px;'>",
                              "<div>Max = %s</div>",
                              "<hr style='margin-top:2px; margin-bottom:2px;'>",
                              "<b>Interquartile<br/>Range (IQR)</b><br/>",
                              "<div style='padding-top: 3px;'>Q3 = %s<br/>",
                              "Med = %s<br/>",
                              "Q1 = %s</div>",
                              "<hr style='margin-top:2px; margin-bottom:2px;'>",
                              "<div style='padding-top: 3px;'>Min = %s</div>",
                              "</div")
    
    # ggplot2 plotting. Rotate labels if enough classes. Use scale_fill_hue()
    # to distinguish boxplot colors from barplot colors.
    p <- ggplot(inputdata, aes_string(x = expl_col, 
                                      y = resp_col, 
                                      fill = expl_col)) + 
      geom_boxplot_interactive(aes(tooltip = sprintf(tooltip_content,
                                                     tooltip_max,
                                                     tooltip_q3, 
                                                     tooltip_mdn,
                                                     tooltip_q1,
                                                     tooltip_min))) + 
      ylab(paste(resp_col, "(min)")) +
      theme(axis.text = element_text(size = 13),
            axis.title = element_text(size = 15),
            legend.position = "none")
    
    # Use RColorBrewer color scale. Set3 has 12 set colors, interpolate if
    # there are more values to map than that.
    p <- InterpolateGgplotColors(p, legendnames, 12, "Set3")
    
    # Diagonal labels if more values to map than five
    if(length(legendnames) > 5) {
      p <- p + theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1))
    }
    
    
    # Prepare the downloadable boxplot. "boxplot_out" is brought global environment 
    # for download. Use larger fonts.
    
    # Labeling inactive checkGroup values is not meaningful in the boxplot
    boxplot_out <- LabelBuilder(p, "", c(), input$subdivGroup)
    boxplot_out <<- 
      boxplot_out + 
      theme(axis.text = element_text(size = 21),
            axis.title = element_text(size = 23),
            axis.text.x = element_text(size = 21))
    
    # Render boxplot
    ggiraph(code = print(p),
            width_svg = 16.7,
            height_svg = 7)
  })
  
  
  #### 5.5.1 Download boxplot ----
  output$dl_boxplot <- downloadHandler(
    filename = paste("boxplot_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     format(Sys.time(), "%d-%m-%Y"),
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = boxplot_out, file, height = 12, width = 26, dpi = 150)
    }
  )
  
  
  #### 5.6 Levene test ---------------------------------------------------------
  output$levene <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    inputdata <- currentdata()
    
    levene <- car::leveneTest(thisFormula, inputdata, center = mean)
    
    res <- SigTableToShiny(levene, TRUE)
    
    # Present "Df" as integer to prevent decimal places in app
    res["Df"] <- sapply(res["Df"], as.integer)
    levene_out <<- res # Result to global environment to enable download
    
    # Render Levene test
    res
  }, 
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  
  #### 5.6.1 Download levene test table ----
  output$dl_levene <- downloadHandler(
    filename = paste("levene_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".csv",
                     sep = ""),
    
    content = function(file) {
      write.csv(levene_out, file)
    }
  )
  
  
  #### 5.7 One-way ANOVA -------------------------------------------------------
  output$anova <- renderTable({
    
    expl_col <- input$expl
    thisFormula <- as.formula(paste(input$resp, "~", expl_col))
    inputdata <- currentdata()
    
    #### One-way ANOVA
    res.aov <- aov(thisFormula, data = inputdata)
    anovasummary <- summary(res.aov)
    
    # Use this function to communicate table correctly to Shiny
    anovasummary <- SigTableToShiny(anovasummary, FALSE)
    
    anovasummary["Df"] <- sapply(anovasummary["Df"], as.integer)
    anova_out <<- anovasummary # Result to global environment to enable download
    
    # Render ANOVA
    anovasummary
  },
  digits = 6,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = TRUE)
  
  
  #### 5.7.1 Download One-way ANOVA table ----
  output$dl_anova <- downloadHandler(
    filename = paste("oneway-anova_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     format(Sys.time(), "%d-%m-%Y"),
                     ".csv",
                     sep = ""),
    
    content = function(file) {
      write.csv(anova_out, file)
    }
  )
  
  
  ### 5.8 Brown-Forsythe test --------------------------------------------------
  output$brownf <- renderPrint({
    
    resp_col <- input$resp
    expl_col <- input$expl
    thisFormula <- as.formula(paste(resp_col, "~", expl_col))
    
    inputdata <- currentdata()
    inputdata <- inputdata[, !names(inputdata) %in% supportcols]
    
    # bf.test() works so that the information we want is only printed to
    # console. Capture that output and place it in a variable
    captured <- capture.output(onewaytests::bf.test(thisFormula, data = inputdata), 
                               file = NULL, 
                               append = TRUE)
    brown_out <<- captured # Result to global environment to enable download
    
    # Render Brown-Forsythe
    cat(captured, sep = "\n")
  })
  
  
  #### 5.8.1 Download Brown-Forsythe table ----
  output$dl_brown <- downloadHandler(
    filename = paste("brownf_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     input$resp, "-", input$expl, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".txt",
                     sep = ""),
    
    content = function(file) {
      cat(brown_out, file = file, sep = "\n")
    }
  )
  
  
  ### 5.9 Interactive map -----------------------------------------------------
  output$interactive <- renderGirafe({
    
    # Use reactive data_f and postal
    # Only select municipalities selected by user. Do the same for "postal",
    # we fetch Jenks breaks from there.
    inputdata <- current_data_f()
    inputdata <- inputdata[!inputdata$kunta %in% c(input$kunta), ]
    
    inputpostal <- currentpostal()
    inputpostal <- inputpostal[!inputpostal$kunta %in% c(input$kunta), ]
    
    # Set interactive map extent by what's active on inputdata
    minlat <- plyr::round_any(min(inputdata$lat), 100, f = floor)
    maxlat <- plyr::round_any(max(inputdata$lat), 1000, f = ceiling)
    minlon <- plyr::round_any(min(inputdata$lon), 100, f = floor)
    maxlon <- plyr::round_any(max(inputdata$lon), 1000, f = ceiling)
    
    # Set properties for interactive map for each input$karttacol value
    if(input$karttacol == "jenks_artificial") {
      datacol <- "artificial"
      brewerpal <- "YlGn"
      legendname <- "Built surface (%)"
      
    } else if (input$karttacol == "jenks_park_mean") {
      datacol <- "parktime_mean"
      brewerpal <- "BuPu"
      legendname <- "Parking time,\nmean (min)"
      
    } else if (input$karttacol == "jenks_walk_mean") {
      datacol <- "walktime_mean"
      brewerpal <- "Oranges"
      legendname <- "Parking time,\nmean (min)"
      
    } else if (input$karttacol == "jenks_park_median") {
      datacol <- "parktime_median"
      brewerpal <- "BuGn"
      legendname <- "Parking time,\nmedian (min)"
      
    } else if (input$karttacol == "jenks_walk_median") {
      datacol <- "walktime_median"
      brewerpal <- "OrRd"
      legendname <- "Walking time,\nmedian (min)"
      
    } else if (input$karttacol == "jenks_answer_count") {
      datacol <- "answer_count"
      brewerpal <- "Reds"
      legendname <- "Answer count"
    }
    
    # Direction of the color gradient, needed for artificial surfaces
    if(input$karttacol == "jenks_artificial") {
      brewer_direction <- 1
    } else {
      brewer_direction <- -1
    }
    
    # Create Jenks breaks columns here so that user gets the control of Jenks
    # breaks classes. classInt::classIntervals() together with cut() creates
    # non-overlapping intervals, which are denoted as follows, for example:
    # Levels: (183,271] (102,183] (56,102] (24,56] [1,24]
    # In this notation ( means "is not included" and [ or ] means "is included"
    inputdata <- CreateJenksColumn(inputdata, inputpostal, datacol, input$karttacol, 
                                   input$jenks_n)
    
    # Get centroids for labelling polygons
    current_centr <- GetCentroids(inputdata, "zipcode", datacol)
    
    # Finetune locations for certain labels. GetCentroids saves the second
    # parameter as rownames. We can use that to reliably find correct rows
    # to finetune.
    current_centr["00250", 1] %+=% 500 # Taka-Toolo
    current_centr["00980", 1] %-=% 850 # Etela-Vuosaari
    current_centr["01640", 2] %-=% 300 # Hamevaara 
    current_centr["01730", 2] %-=% 500 # Vantaanpuisto
    current_centr["02380", 1] %+=% 2400 # Suvisaaristo
    current_centr["02820", 1] %+=% 1000 # Nupuri-Nuuksio
    
    # Use a local version of subdiv centroids to conditionally delete label
    # for Kauniainen
    current_subdiv <- data.frame(subdiv_cntr)
    
    # Format map labels. Remove [, ], (, and ). Also add list dash
    labels <- gsub("(])|(\\()|(\\[)", "", levels(inputdata[, input$karttacol]))
    labels <- gsub(",", " \U2012 ", labels)
    
    tooltip_content <- paste0(
      "<div id='app-tooltip'>",
      "<div>%s, %s<br/>",
      "Answer count: <b>%s</b></div>",
      "<hr id='tooltip-hr'>",
      "<div id='tooltip-div'>Parktime, mean: %s</br>",
      "Parktime, median: %s</div>",
      "<div id='tooltip-div'>Walktime, mean: %s</br>",
      "Walktime, median: %s</div>",
      "<div id='tooltip-div'>Built surface (%%): %s</div>",
      "<div style='padding-top: 3px; line-height: 1.2;'>Largest YKR<br/>zone (%%): %s</div>",
      "</div>")
    
    g <- ggplot(inputdata) +
      geom_polygon_interactive(
        color = "black",
        size = 0.2,
        
        # aes_string() is to facilitate interactive map tooltip creation
        aes_string("long", "lat",
                   group = "group", 
                   fill = input$karttacol,
                   tooltip = substitute(sprintf(tooltip_content,
                      id, nimi, answer_count, parktime_mean, parktime_median, 
                      walktime_mean, walktime_median, artificial, largest_ykr)))) +
      
      # Jenks classes colouring and labels
      scale_fill_brewer(palette = brewerpal,
                        direction = brewer_direction,
                        name = legendname,
                        labels = labels,
                        na.value = "#ebebeb") +
    
    # Scale bar and north arrow
    ggsn::scalebar(inputdata, 
                   dist_unit = "km",
                   dist = 2,
                   st.dist = 0.01,
                   st.size = 4, 
                   height = 0.01, 
                   transform = FALSE) +
      ggsn::north(inputdata, 
                  location = "topright", 
                  scale = 0.04, 
                  symbol = 10)
    
    # Plot municipality boundaries on the interactive map
    if(input$show_muns == TRUE) {

      g <- g + geom_polygon(data = muns_f,
                            aes(long, lat, group = group),
                            linetype = "solid",
                            color = alpha("black", 0.9), 
                            fill = "NA",
                            size = 1.0)
    }
    
    # Subdivision boundaries
    if(input$show_subdivs == TRUE) {

      g <- g + geom_polygon(data = suuralue_f,
                            aes(long, lat, group = group),
                            linetype = "solid",
                            color = alpha("black", 0.6), 
                            fill = "NA",
                            size = 0.8)
    }
    
    # Show current Jenks breaks value in zipcode area polygons 
    if(input$show_postal_labels == TRUE) {

      g <- g + with(current_centr,
                    annotate(geom = "text",
                             x = long, 
                             y = lat, 
                             label = label, 
                             size = 4))
    }
    
    # Show municipality labels
    if(input$show_muns_labels == TRUE) {
      
      # Disable Kauniainen label on subdiv when muns labels visible
      if(input$show_subdivs_labels == TRUE) {
        current_subdiv["Kauniainen", "label"] <- NA
      }
      
      # use geom name "label" and parameters label.size and fill to decide
      # label background box
      g <- g + with(muns_cntr,
                    annotate(geom = "label", 
                             x = long, 
                             y = lat, 
                             label = label, 
                             label.size = NA,
                             fill = alpha("white", 0.5),
                             size = 5,
                             fontface = 2))
    }
    
    # Show subdivision labels
    if(input$show_subdivs_labels == TRUE) {
      
      # Disable Kauniainen label on subdiv when muns labels visible
      if(input$show_muns_labels == TRUE) {
        current_subdiv["Kauniainen", "label"] <- NA
      }
      g <- g + with(current_subdiv,
                    annotate(geom = "label",
                             x = long,
                             y = lat,
                             label = label,
                             label.size = NA,
                             fill = alpha("white", 0.5),
                             size = 4))
    }
    
    g <- g + coord_fixed(xlim = c(minlon, maxlon),
                         ylim = c(minlat, maxlat)) +
      
      # Class intervals disclaimer
      labs(caption = paste("The Jenks breaks classes are non-overlapping. The",
                           "lowest value of each range, with the exception of",
                           "the most bottom one, is not included in that class.")) +
      
      # Legend and caption settings
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            plot.caption = element_text(size = 13, hjust = 0.5, face = "italic"))
    
    
    # Prepare the downloadable interactive map. "interactive_out" is brought 
    # to global environment for download. Use larger fonts.
    interactive_out <- LabelBuilder(g, input$expl, input$checkGroup, 
                                    input$subdivGroup)
    interactive_out <<- 
      interactive_out + 
      theme(legend.title = element_text(size = 17),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
    
    # Render interactive map
    girafe(ggobj = g,
           width = 16.7,
           height = 14.7,
           options = list(opts_zoom(min = 1, max = 3),
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  
  #### 5.9.1 Download interactive map ----
  output$dl_interactive <- downloadHandler(
    filename = paste("interactive-map_",
                     "pmax", input$parktime_max, "-wmax", input$walktime_max, "_",
                     format(Sys.time(), "%d-%m-%Y"), 
                     ".png",
                     sep = ""),
    
    content = function(file) {
      ggsave(plot = interactive_out, file, height = 16, width = 18, dpi = 150)
    }
  )
}



### 6 ShinyApp UI elements ----------------------------------------------------- 
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("slate"),
  
  
  ### 6.1 ShinyApp header ------------------------------------------------------ 
  
  # Edit various features of the ShinyApp: 
  # - sidebarPanel (form.well) width. sidebarPanel width setting is important 
  #   because the long explanations would break it otherwise. 
  # - manually set sidebarPanel z-index to make the element always appear on top
  # - .checkbox input achieves strikeout on selected checkboxes
  # - pointer-events: none; makes zipcode labels invisible to the cursor
  # - noselect makes selecting ggiraph elements not possible
  # - :last-child pseudo-selector makes last row of descriptive statistics bold
  # - add JavaScript function which detects the the display css property of 
  #   given div. Use this to hide or display elements in the app and change icon 
  #   accordingly
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"),
            htmltools::includeCSS(csspath)),
  htmltools::includeScript(path = jspath),
  
  
  
  ### 6.2 Sidebar layout -------------------------------------------------------
  titlePanel(NULL, windowTitle = "Sampo Vesanen MSc thesis research survey results"),
  sidebarLayout(
    sidebarPanel(id = "sidebar",
                 
      ### 6.2.1 Table of contents ----
      # &nbsp; is a non-breaking space
      HTML("<div id='contents'>",
           "<p id='linkheading_t'>Analysis</p>",
           "<a href='#descrilink'>1&nbsp;Descriptive statistics</a> &mdash;",
           "<a href='#histlink'>2&nbsp;Histogram</a> &mdash;",
           "<a href='#barplotlink'>3&nbsp;Barplot</a> &mdash;",
           "<a href='#boxplotlink'>4&nbsp;Boxplot</a> &mdash;",
           "<a href='#levenelink'>5&nbsp;Levene</a> &mdash;",
           "<a href='#anovalink'>6&nbsp;One-way ANOVA</a> &mdash;",
           "<a href='#brownlink'>7&nbsp;Brown-Forsythe</a><br>",
           "<p id='linkheading_b'>Visualisation</p>",
           "<a href='#intmaplink'>8&nbsp;Interactive map</a>",
           "</div>"),
     
     
      ### 6.2.2 Maximum allowed values ----
      # Set allowed maximum for parktime and walktime. Default is 60 for both.
      HTML("<div id='stats-settings-link'>",
           "<label>Set maximum allowed values",
           "<p id='smalltext'>(Affects sections 1&mdash;8)</p></label>",
           "<div id='contents'>"),
      sliderInput(
        "parktime_max",
        HTML("parktime (min)"),
        min = min(thesisdata$parktime),
        max = max(thesisdata$parktime),
        value = 59,
        step = 1),
     
      sliderInput(
        "walktime_max",
        HTML("walktime (min)"), 
        min = min(thesisdata$walktime),
        max = max(thesisdata$walktime),
        value = 59,
        step = 1),
     
      actionButton(
        "resetParkWalk", 
        HTML("<i class='icon history'></i>Revert values to default (59&nbsp;min)")),
     
      HTML("</div>"),
     
     
      ### 6.2.3 Active variables ----
      # Select walktime or parktime
      HTML("<label>Currently active variables",
           "<p id='smalltext'>(Affects sections 1&mdash;8)</p>",
           "</label>",
           "<div id='contents'>"),
      selectInput(
        "resp", 
        HTML("Response (continuous)"),
        names(thesisdata[continuous])),
     
      # likert, parkspot, timeofday, artificial, ykr_zone, subdiv
      selectInput(
        "expl",
        "Explanatory (ordinal)", 
        names(thesisdata[ordinal])),
     
      # These are changed with the observer function
      checkboxGroupInput(
        "checkGroup",
        "Select inactive groups in current explanatory variable",
        choiceNames = c("Item A", "Item B", "Item C"),
        choiceValues = c("a", "b", "c")),
      HTML("</div>",
           "</div>"),
     
     
      ### 6.2.4 Histogram ----
      # Allow user to access histogram binwidth
      HTML("<div id='hist-settings-link'>",
           "<label>2 Histogram",
           "<a id='smalltext' href='#histlink'><i class='icon link' title='Go to the plot'></i></a></label>",
           "<div id='contents'>"),
      sliderInput(
        "bin",
        HTML("Binwidth for the current response variable"), 
        min = 1, 
        max = 10, 
        value = 2),
      HTML("</div>",
           "</div>"),
     
     
      ### 6.2.5 Barplot ----
      # Provide user possibility to see distribution of answers within the
      # ordinal variables.
      # The values of this conditionalPanel are changed with the observer
      # function
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
       
        HTML("<div id='barplot-settings-link'>",
             "<label>3 Distribution of ordinal variables",
             "<a id='smalltext' href='#barplotlink'><i class='icon link' title='Go to the plot'></i></a></label>",
             "<div id='contents'>"),
        selectInput(
          "barplot", 
          HTML("Y axis for the barplot"),
          names(thesisdata[c("zipcode", "likert", "walktime")]),
        ),
        HTML("</div>",
            "</div>")
      ),
     
     
      ### 6.2.6 Inactive subdivisions ----
      # Select to inactivate subdivs. Overrides all options (except interactive 
      # map) 
      HTML("<div id='subdiv-settings-link'>",
           "<label>Select inactive subdivisions",
           "<p id='smalltext'>",
           "(Affects sections 1&mdash;8. Please be aware that these selections",
           "override Explanatory (ordinal) variable 'subdiv')</p></label>",
           "<div id='contents'>"),
      checkboxGroupInput(
        "subdivGroup",
        NULL,
        choiceNames = sort(as.character(unique(thesisdata$subdiv))),
        choiceValues = sort(as.character(unique(thesisdata$subdiv)))),
     
      # Reset inactivations with this button
      actionButton(
        "resetSubdivs", 
        HTML("<i class='icon history'></i>Clear all selections")),
      HTML("</div>",
           "</div>"),
     
     
      ### 6.2.7 Interactive map ----
      # Interactive map Jenks breaks options
      HTML("<div id='intmap-settings-link'>",
           "<label>9 Interactive map",
           "<a id='smalltext' href='#intmaplink'><i class='icon link' title='Go to the map'></i></a></label>",
           "<div id='contents'>"),
      checkboxGroupInput(
        "kunta",
        HTML("Active municipalities"),
        choiceNames = c("Helsinki", "Vantaa", "Espoo", "Kauniainen"),
        choiceValues = c("091", "092", "049", "235")),
     
      selectInput(
        "karttacol",
        HTML("Jenks breaks parameter"),
        c("jenks_answer_count", "jenks_park_mean", "jenks_park_median", 
          "jenks_walk_mean", "jenks_walk_median", "jenks_artificial")),
     
      sliderInput(
        "jenks_n",
        "Amount of classes",
        min = 2, 
        max = 8, 
        value = 5),
     
      # Layer options: on-off switches
      HTML("<label class='control-label'>Layer options</label>",
           "<div class='onoff-container'>",
           "<div class='onoff-div'><b>Postal code areas</b><br>"),
      # Switch for interactive map labels
      HTML("<label class='control-label onoff-label' for='show_postal_labels'>Labels</label>"),
      shinyWidgets::switchInput(
        inputId = "show_postal_labels", 
        size = "mini",
        value = TRUE),
      HTML("</div>"),
     
      # Switches for muns
      HTML("<div class='onoff-div'><b>Municipalities</b><br>",
           "<label class='control-label onoff-label' for='show_muns'>Boundaries</label>"),
      shinyWidgets::switchInput(
        inputId = "show_muns", 
        size = "mini",
        value = TRUE),
      
      HTML("<label class='control-label onoff-label' for='show_muns_labels'>Labels</label>"),
      shinyWidgets::switchInput(
        inputId = "show_muns_labels", 
        size = "mini",
        value = FALSE),
      HTML("</div>"),
     
      # Switches for subdivisions
      HTML("<div class='onoff-div'><b>Subdivisions</b><br>",
           "<label class='control-label onoff-label' for='show_subdivs'>Boundaries</label>"),
      shinyWidgets::switchInput(
        inputId = "show_subdivs", 
        size = "mini",
        value = FALSE),
      
      HTML("<label class='control-label onoff-label' for='show_subdivs_labels'>Labels</label>"),
      shinyWidgets::switchInput(
        inputId = "show_subdivs_labels", 
        size = "mini",
        value = FALSE),
      HTML("</div>",
           "</div>",
           "</div>",
           "</div>",
           "<p id='version-info'>Analysis app version 18.7.2020</p>"),
      
      width = 3
    ),
    
    
    ### 6.3 mainPanel layout ---------------------------------------------------
    mainPanel(
      h2("Sampo Vesanen MSc thesis research survey results"),
      p("Welcome to the analysis application for the survey results for the",
        "thesis 'Parking of private cars and spatial accessibility in Helsinki",
        "Capital Region'. Please see the thesis and GitHub repository for",
        "additional information and instructions how to use this application."),
      hr(),
      
      # Descriptive statistics
      HTML("<div id='descrilink'>",
           "<h3>1 Descriptive statistics&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('descri','descrilink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_descri",
                   label = HTML("<i class='icon file' title='Download this table as csv'></i>")),
      HTML("</h3>"),
      tableOutput("descri"),
      HTML("</div>"),
      hr(),
      
      # Histogram
      HTML("<div id='histlink'>",
           "<h3>2 Histogram&ensp;",
           "<a href='#hist-settings-link'><i class='icon wrench' title='Go to histogram settings'></i></a>",           
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('hist','histlink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_hist",
                   label = HTML("<i class='icon file' title='Download hi-res version of this plot (png)'></i>")),
      HTML("</h3>"),
      ggiraphOutput("hist", height = "400px"),
      HTML("</div>"),
      hr(),
      
      # Barplot
      HTML("<div id='barplotlink'>",
           "<h3>3 Distribution of ordinal variables&ensp;",
           "<a href='#barplot-settings-link'><i class='icon wrench' title='Go to barplot settings'></i></a>",           
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('barplot_wrap','barplotlink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_barplot",
                   label = HTML("<i class='icon file' title='Download hi-res version of this plot (png)'></i>")),
      HTML("</h3>",
           "<div id='barplot_wrap'>",
           "<p>This plot is active when <tt>likert</tt>, <tt>parkspot</tt>, or", 
           "<tt>timeofday</tt> is selected as the explanatory (ordinal)",
           "variable.</p>"),
      conditionalPanel(
        condition = 
          "input.expl == 'likert' || input.expl == 'parkspot' || input.expl == 'timeofday'",
        ggiraphOutput("barplot_ord", height = "400px"),
      ),
      HTML("</div>"),
      hr(),
      HTML("</div>"),
      
      # Boxplot
      HTML("<div id='boxplotlink'>",
           "<h3>4 Boxplot&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('boxplot','boxplotlink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_boxplot",
                   label = HTML("<i class='icon file' title='Download hi-res version of this plot (png)'></i>")),
      HTML("</h3>"),
      ggiraphOutput("boxplot", height = "500px"),
      HTML("</div>"),
      hr(),
      
      # Levene's test. Significance legend is inserted with JavaScript
      HTML("<div id='levenelink'>",
           "<h3>5 Test for homogeneity of variances (Levene's test)&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('levene_wrap','levenelink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_levene",
                   label = HTML("<i class='icon file' title='Download this table as csv'></i>")),
      HTML("</h3>",
           "<div id='levene_wrap'>"),
      p("Look for p-value > 0.05 (0.05 '.' 0.1 ' ' 1) (variance across groups",
        "is not statistically significant) for ANOVA test to be meaningful. If",
        "p < 0.05, null hypothesis is rejected and it can be concluded that there",
        "is a difference between the variances in the population. If p < 0.05,",
        "employ Brown-Forsythe test."),
      tableOutput("levene"),
      HTML("</div></div>"),
      hr(),
      
      # ANOVA
      HTML("<div id='anovalink'>",
           "<h3>6 One-way analysis of variance (One-way ANOVA)&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('anova_wrap','anovalink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_anova",
                   label = HTML("<i class='icon file' title='Download this table as csv'></i>")),
      HTML("</h3>",
           "<div id='anova_wrap'>"),
      tableOutput("anova"),
      HTML("</div>",
           "</div>"),
      hr(),
      
      # Brown & Forsythe
      HTML("<div id='brownlink'>",
           "<h3>7 Brown-Forsythe test&ensp;",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('brown_wrap', 'brownlink')\"><i class='icon eyeslash' title='Hide element'></i></button></button>"),
      downloadLink("dl_brown",
                   label = HTML("<i class='icon file' title='Download this output as txt'></i>")),
      HTML("</h3>",
           "<div id='brown_wrap'>"),
      p("Look for a statistically significant difference between the selected", 
        "explanatory variable. Brown-Forsythe test is less likely than the",
        "Levene test to incorrectly declare that the assumption of equal", 
        "variances has been violated. Please note that Brown-Forsythe test",
        "fails when selected response variable maximum value is set to 0. The",
        "test requires a p.value that's not NaN."),
      verbatimTextOutput("brownf"),
      HTML("</div>",
           "</div>"),
      hr(),
      
      # Interactive map
      HTML("<div id='intmaplink'>",
           "<h3>8 Survey results on research area map&ensp;",
           "<a href='#intmap-settings-link'><i class='icon wrench' title='Go to interactive map settings'></i></a>",
           "<a href='#stats-settings-link'><i class='icon chart' title='Go to active variables'></i></a>",
           "<a href='#subdiv-settings-link'><i class='icon mapmark' title='Go to inactive subdivisions'></i></a>",
           "<button id='showhidebutton' onclick=\"show_hide('interactive', 'intmaplink')\"><i class='icon eyeslash' title='Hide element'></i></button>"),
      downloadLink("dl_interactive",
                   label = HTML("<i class='icon file' title='Download hi-res version of this figure (png)'></i>")),
      HTML("</h3>"),
      girafeOutput("interactive"),
      HTML("</div>"),
      hr(),
      
      # Data providers
      h3("Data providers"),
      HTML("<a href='https://hri.fi/data/dataset/paakaupunkiseudun-aluejakokartat'>Municipality subdivisions</a>",
           "(C) Helsingin, Espoon, Vantaan ja Kauniaisten mittausorganisaatiot",
           "2011. Aineisto on muokkaamaton. License ",
           "<a href='https://creativecommons.org/licenses/by/4.0/deed.en'> CC BY 4.0</a>.",
           
           "<br><a href='https://www.stat.fi/tup/paavo/index_en.html'>Postal code area boundaries</a>",
           "(C) Statistics Finland 2019. Retrieved 27.6.2019. License ",
           "<a href='https://creativecommons.org/licenses/by/4.0/deed.en'>CC BY 4.0</a>.",
           
           "<br><a href='http://urn.fi/urn:nbn:fi:csc-kata00001000000000000226'>Regional population density 2012</a>",
           "(C) Statistics Finland 2019. Retrieved 13.3.2020. License ",
           "<a href='http://www.nic.funet.fi/index/geodata/tilastokeskus/Tilastokeskus_terms_of_use_2018.pdf'>Other (Open)</a>.",
           
           "<br><a href='http://metatieto.ymparisto.fi:8080/geoportal/catalog/search/resource/details.page?uuid={26EEEBBB-FB5C-4045-B6DF-439F9B7D5C46}'>Corine Land Cover 2018</a>",
           "(C) Finnish Environment Institute 2018. Retrieved 13.7.2020. License ",
           "<a href='https://creativecommons.org/licenses/by/4.0/deed.en'>CC BY 4.0</a>.",
           
           "<br><a href='http://metatieto.ymparisto.fi:8080/geoportal/catalog/search/resource/details.page?uuid={B374BBB2-1EDF-4CF6-B11B-04E0017E9A26}'>Yhdyskuntarakenteen vyohykkeet 2017</a>",
           "(C) Finnish Environment Institute 2019. Retrieved 27.6.2019. License ",
           "<a href='https://creativecommons.org/licenses/by/4.0/deed.en'>CC BY 4.0</a>.")
    )
  )
))
shinyApp(ui = ui, server = server)