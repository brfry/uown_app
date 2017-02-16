#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#-------------------------------------------------------------------------------
#Import libraries needed
#-------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl) # dev version installed. converting excel
library(httr) # for url
library(leaflet) # for interactive maps

#---------------------------------------------------------------------------
# import data from url on UOWN. This will keep it up to date unless name is changed.
# Tutorial here: http://yokekeong.com/reading-excel-files-with-readxl-r-package/
#-----------------------------------------------------------------------------

# specify where data are stored. Concerns if they update file will have a 
# different name. Probably a way to have a function try several years (i.e. start 
# at current year, if no url found, year-1, etc.)
  url <- 'http://uown.org/UOWN-Redesign/SciMon/Data/UOWN_alldata_2001_2015_R.xlsx'

# download onto disk
  download.file(url, "uown_wq1.xlsx", quiet=TRUE, mode = 'wb')

#GET(url, write_disk("uown_wq1.xlsx", overwrite=TRUE)) # this was not working.

# read into R
  uown_wq <- read_excel("uown_wq1.xlsx", sheet = 1) # pull into R. 
  
# Read in coordinates sent by Kara. Also contains landuse data. 
    uown_latlong <- read_excel("Copy of UOWN_Sampling_Points_FINAL.xlsx")
  
#------------------------------------------------------------------------------
# Clean and format data
#------------------------------------------------------------------------------
  
# set column characteristics
  uown_wq$CON <- as.numeric(uown_wq$CON)
  uown_wq$WS <- as.factor(uown_wq$WS)
  uown_wq$ID <- as.factor(uown_wq$ID)
  uown_wq$BS <- as.numeric(uown_wq$BS)
  uown_wq$YR <- as.factor(uown_wq$YR)
  uown_wq$TUR <- as.numeric(uown_wq$TUR)
  uown_wq$VS <- as.numeric(uown_wq$VS)
  uown_wq$pH <- as.numeric(uown_wq$pH)
  uown_wq$NO3 <- as.numeric(uown_wq$NO3)
  uown_wq$EC <- as.numeric(uown_wq$EC)
  uown_wq$QTR <- as.factor(uown_wq$QTR)
  uown_wq$Mon <- as.factor(uown_wq$Mon)
 

# Trim white space
  uown_wq$WS <- trimws(uown_wq$WS)
  
# add column for SiteID to wq df in order to integrate with latlong df
  uown_wq <- uown_wq %>%
          unite(SiteID, WS, ID, 
                sep = "", remove = FALSE) # no space to sep, keep original columns
  
  
# make a vector of only SiteID
  uown_wq_siteIDs <- as.vector(uown_wq$SiteID)
  
# Rename columns in latlong dataframe
  uown_latlong <- uown_latlong %>%
          rename(Latitude = LAT_N_13_1, Longitude = LON_N_13_1)
  
# Specify column for watershed  
  
  
# set new dataframe where all columns are specified as numeric for use with
# regression exploration
  uown_wq_numeric <- uown_wq  %>%
          map_if(is.character, as.numeric) # using purrr to convert all columns
          
  uown_wq_numeric<-  bind_rows(uown_wq_numeric) # bind lists into a dataframe
  
#------------------------------------------------------------------------------
# Calculate state water quality standards for later presentation
#------------------------------------------------------------------------------
  # http://epd.georgia.gov/georgia-water-quality-standards
  
  # Three tiers of the state's waters
        # 1 - existing instream water uses 
        # 2 - lower water quality to accomodate sconomic/social development
        # 3 - outstanding national resource waters
  
  # general parameter limits for all waters:
  
        # bacteria: fecal coliform geomean 200/100mL with 4 samples min. may-oct.
        # No E. coli standard. Federal E. coli std is ______
  
        # DO - 6.0 mg/L daily avg, 5mg/L at all times for trout streams. 1 mg lower for others.
  
        # Conductivity
  
        # Turbidity - all waters shall be free from turbidity which results in 
        # substantial visual contrast in a water body due to manmade activity.
        # Upstream appearance is to be compared to downstream point of activity,
        # allowing for mixing.
  
        # Nitrate - no criteria. Georgia currently developing NNC for TN and TP.
  
        # biology
  
        # visual?
  
  # criteria for drinking water supplies:
        # bacteria - fecal coliform geomean 200/100mL with 4 samples min. may-oct.
        # pH: 6-8.5
  
  # criteria for recreational waters
        # bacteria. coastal waters: monthly geomean enterococci 35 CFU/100ml, no
        # more than 10% exceedence of 130 cfu/100 ml.
          # other waters: E.coli 126 cfu/100 ml for 30day geomean. 10 percent 
        # max exceedance of 410 cfu/100 ml during this 30 days.
  
        # pH - 6.0 - 8.5
  
        # Temperature - not to exceed 90F.
  
  
  # most of Oconee river and tribs area calssified by Georgia as "drinking water"
        # 
  
  
  
  
#------------------------------------------------------------------------------
# Begin shiny part:
#------------------------------------------------------------------------------
  
  ui <- fluidPage(
          
          # Application title
          titlePanel("UOWN Data Exploration"),
          
          h3("Alpha Version 1.03 (2-15-2017)"),
          
          h5(
             p("This data exploration tool is intended for use by Upper Oconee Watershed
             Network (UOWN), academia, and members of the public. The data presented
             here were collected by UOWN, and retrieved from 
               UOWN via the"), 
             tags$a(href = "http://uown.org/UOWN-Redesign/SciMon/index.php", 
                    "UOWN Data Repository"),
             
             p(h4("About the Upper Oconee Watershed Network")),
             
             p("The Upper Oconee Watershed Network is dedicated to protecting
                water resources and improving stream health in our watershed 
                through community-based advocacy, monitoring, education, and recreation."),
             
             p("The Upper Oconee Watershed Network (UOWN) was formed in January 
               2000 in response to citizen concern about the region's rapid 
               growth and its impact to local streams and rivers. UOWN members 
               actively engage in various advocacy, education and stream monitoring 
               initiatives in an effort to raise community awareness about local 
               water resource issues and to facilitate a cooperative spirit for 
               long-term watershed protection."),
             
             p("Located in Athens, Georgia, UOWN is a 75+ member organization
                goverend by a volunteer Board of Directors.")),
          
          #Sidebar with input options
          sidebarLayout(
                  sidebarPanel(
                          # select predictor param
                          selectInput(inputId = "xparam", 
                                      label = "Select x parameter",
                                      choices = c("Watershed" = "WS",
                                                  "Quarter" = "QTR",
                                                  "Year" = "YR",
                                                  "month" = "Mon"),
                                      selected = "Year"),
                          
                          # select response param
                          selectInput(inputId = "yparam", 
                                      label = "Select y parameter",
                                      choices = c("Conductivity" = "CON", 
                                                  "Biological Score" = "BS",
                                                  "Turbidity" = "TUR",
                                                  "Visual Score" = "VS",
                                                  "pH" = "pH",
                                                  "Nitrate" = "NO3",
                                                  "E. coli" = "EC")),
                          
                          # select z param (predictor for y)
                        #  selectInput(inputId = "zparam", 
                        #              label = "Select explanatory variable for y",
                        #              choices = c(names(uown_wq))),
                                              
                                              
                                         #     c("Conductivity" = "CON", 
                                          #        "Biological Score" = "BS",
                                           #       "Turbidity" = "TUR",
                                            #      "Visual Score" = "VS",
                                             #     "pH" = "pH",
                                              #    "Nitrate" = "NO3",
                                               #   "E. coli" = "EC")),
                          
                          # Facet by a parameter
                         # checkboxGroupInput(inputId = "facet_param", 
                          #                   label = "Facet by:",
                           #                  choices = c("Watershed" = "WS",
                            #                             "Quarter" = "QTR",
                             #                            "Year" = "YR",
                              #                           "month" = "Mon"),
                               #              selected = "Quarter"),
                          
                      
                       
                        
                        # select a stream to examine
                         
                         # features to add
                         h3("Features still in development:"),
                         h5(" - spatial exploration and analysis"),
                         h5(" - Modeling (e.g. ANOVA)"),
                         h5(" - Removal of potentially influential outliers"),
                         h5(" - Apply a log transformation to selected parameter"),
                         h5(" - further suggestions?")
                         
                  ),
         
          # Main panel
               # Make a tabset for plots, summary data, and table, and map
          mainPanel(
                  tabsetPanel(type = "tabs",
                              
                    tabPanel("Boxplot",
                             h4("Facet options for further dividing data"),
                             fluidRow(
                                     column(3,
                                            selectInput('facet_row', 'Facet Row', c("none" = '.',
                                                                     "Watershed" = "WS",
                                                                     "Quarter" = "QTR",
                                                                     "Year" = "YR",
                                                                     "month" = "Mon"))),
                                     column(3,
                                            selectInput('facet_col', 'Facet Column', c("none" = '.',
                                                                        "Watershed" = "WS",
                                                                        "Quarter" = "QTR",
                                                                        "Year" = "YR",
                                                                        "month" = "Mon")))
                             ),
                             plotOutput("plot1", height = 500),
                             h5("")),
                    
                    tabPanel("Scatterplot",
                             h5("How well can a single parameter explain another?",
                                p("Note that categorical predictors will not be used
                                  in the simple linear regression line shown in blue.")),
                             
                             # select z param (predictor for y)
                             selectInput(inputId = "zparam", 
                                         label = "Select explanatory variable for y",
                                         choices = c(names(uown_wq))),
                             
                             plotOutput("plot2", height = 500)),
                    
                    tabPanel("Summary statistics", 
                             h5("Table of summary statistics for y parameter, 
                                grouped by x parameter"),
                             tableOutput("table1")),
                    
                    tabPanel("Map of Stations", 
                             h5("Click on a location to view station"),
                             leafletOutput("stationMap", height = 500)),
                    
                    tabPanel("Data", dataTableOutput("raw_data")),
                    
                    tabPanel("Investigate a Site",  
                             selectInput(inputId = 'site_select', 
                                         label = 'Select a site to examine in detail',
                                         choices = c("none" = '.', uown_wq_siteIDs)),
                             fluidRow(
                                     column(6,
                                             h4("Boxplot for Selected Site"),
                                            plotOutput("plot3")),
                                     column(4,
                                            h4("Summary table for Selected Site"),
                                            tableOutput("table2"))))
                  
                 # h3("Boxplot of selected parameters"),
              
                  # scatterplot
               #   h3("How well can a single parameter explain another?"),
                 # plotOutput("plot2")
                  
                  # Table of summary statistics
                 # tableOutput("table1")  

                 )
         )
  ),
  # Footer disclaimer
  h6("Written by Beck R. Frydenborg (brfry11@gmail.com). Written in the
     programming language R (R Development Core Team, 2015. Vienna, Austria.
     www.r-project.org version 3.3.2 (2016-10-31)."),
  h6("Disclaimer: This product is not intended for regulatory decisions.")
 )
  
  
  
  server <- function(input, output, session) {
          
          # reactive dataset
          
          
  
         # make a boxplot 
        output$plot1 <- renderPlot({
                        p <- ggplot(uown_wq, 
                                    aes_string(x = input$xparam,
                                   y = input$yparam,
                                   fill = factor(input$yparam))) +
                        geom_boxplot() +
                                scale_fill_manual(values = c("green4"),
                                                  guide = guide_legend(title = NULL)) + # hide legend title
                                guides(fill = FALSE) # completely hide the legend
                                
                        
                # for facetting
                facets <- paste(input$facet_row, '~', input$facet_col)
                if (facets != '. ~ .')
                        p <- p + facet_grid(facets)  
                
                
                p
                
        })
          
        # make a scatterplot ox x and y using numeric dataframe
        output$plot2 <- renderPlot({
                ggplot(uown_wq, aes_string(x = input$zparam),
                                           y = input$yparam) +
                        geom_point(aes_string(y = input$yparam)) +
                        geom_smooth(aes_string(y = input$yparam),
                                    method = "lm")
        })
        
        
        # output a plot for site specific data explorations
        output$plot3 <- renderPlot({
                
                # for single site examination, filter for site input
                if (input$site_select != '.'){
                        uown_wq <- uown_wq %>%
                                filter(SiteID == input$site_select) 
                }
                
                p2 <- ggplot(uown_wq, 
                            aes_string(x = input$xparam,
                                       y = input$yparam,
                                       fill = factor(input$yparam))) +
                        geom_boxplot() +
                        scale_fill_manual(values = c("green4"),
                                          guide = guide_legend(title = NULL)) + # hide legend title
                        guides(fill = FALSE) # completely hide the legend
                p2
        })
        
        # output a summary table for single site tab
        output$table2 <-renderTable({
                if (input$site_select != '.'){
                        uown_wq <- uown_wq %>%
                                filter(SiteID == input$site_select) 
                }
                
                t2 <- uown_wq %>% 
                        select_(input$yparam, input$xparam) %>%
                        group_by_(input$xparam) %>%
                        summarise_each(funs(n = n(),
                                            mean(.,na.rm = TRUE),
                                            median(., na.rm = TRUE),
                                            sd(., na.rm = TRUE),
                                            min(., na.rm = TRUE),
                                            max(., na.rm = TRUE)))
                t2
        })
        
        
        # make a reactive dataframe
        uown_reactive <- reactive({
                uown_wq %>% select_(input$yparam, input$xparam) %>%
                        group_by_(input$xparam)
                
               # uown_wq %>% select_(input$yparam, input$xparam,
                #                    input$facet_row, input$facet_col) %>%
        })
        
        
        # make a simple summary table for reactive dataframe     
        output$table1 <- renderTable({

                test1 <- uown_reactive() %>% 
                       # select_(input$yparam, input$xparam) %>%
                        summarise_each(funs(n = n(),
                                           mean(.,na.rm = TRUE),
                                           median(., na.rm = TRUE),
                                           sd(., na.rm = TRUE),
                                           min(., na.rm = TRUE),
                                           max(., na.rm = TRUE)))
                
                # for facetting
               # facets <- paste(input$facet_row, '~', input$facet_col)
                
                #if
                #if (facets != '. ~ .')
                 #       test1 <- test1 %>% 
                 #       group_by_(input$xparam,
                 #                 input$facet_row,
                 #                 input$facet_col) %>%
                 #                summarise_each(funs(n = n(),
                 #                                   mean(.,na.rm = TRUE),
                 #                                   sd(., na.rm = TRUE)))
                
                test1
                })  
        
        # make map using leaflet
        
        #first specify station lat longs for points
       # points <- uown_latlong
       # points <- eventReactive(input$recalc, {
        #       cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
       #}, ignoreNULL = FALSE)
        
        output$stationMap <- renderLeaflet({
                leaflet(data = uown_latlong) %>%
                        addTiles() %>% # default background map
                        addCircleMarkers(data = uown_latlong, 
                                         popup = ~ as.character(SiteID))
            
        })
        
        output$raw_data <- renderDataTable(uown_wq,
                                           options = list(pageLength = 10)
        )
        
}

shinyApp(ui = ui, server = server)

