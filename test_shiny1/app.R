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

#---------------------------------------------------------------------------
# import data from url on UOWN. This will keep it up to date.
# Tutorial here: http://yokekeong.com/reading-excel-files-with-readxl-r-package/
#-----------------------------------------------------------------------------
# specify where data re stored. Concerns if they update file in 2016, will have a 
# different name. Probably a way to have a function try several years (i.e. start 
# at current year, if no url found, year-1, etc.)
  url <- 'http://uown.org/UOWN-Redesign/SciMon/Data/UOWN_alldata_2001_2015_R.xlsx'

# download onto disk
  download.file(url, "uown_wq1.xlsx", quiet=TRUE, mode = 'wb')

#GET(url, write_disk("uown_wq1.xlsx", overwrite=TRUE)) # this was not working.

# read into R
  uown_wq <- read_excel("uown_wq1.xlsx", sheet = 1) # pull into R. 
  
# Read in coordinates sent by Kara. Also contains landuse data. currently
  # not in directory
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
          
          h3("Alpha Version 1.01 (2-10-2017)"),
          
          h5(
             p("This data exploration tool is intended for use by Upper Oconee Watershed
             Network (UOWN), academia, and members of the public. The data presented
             here were collected by UOWN, and retrieved from 
               UOWN via: http://uown.org/UOWN-Redesign/SciMon/index.php."),
             
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
                          
                         h4("Facet options for further dividing data"),
                          selectInput('facet_row', 'Facet Row', c("none" = '.',
                                                                     "Watershed" = "WS",
                                                                  "Quarter" = "QTR",
                                                                  "Year" = "YR",
                                                                  "month" = "Mon")),
                                  
                                 # None='.', names(uown_wq))),
                          selectInput('facet_col', 'Facet Column', c("none" = '.',
                                                                     "Watershed" = "WS",
                                                                     "Quarter" = "QTR",
                                                                     "Year" = "YR",
                                                                     "month" = "Mon")),
                         
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
                              
                    tabPanel("Boxplot", plotOutput("plot1"),
                             h5("The y parameter is grouped by the x parameter")),
                    
                    tabPanel("Scatterplot",
                             h5("How well can a single parameter explain another?"),
                             
                             # select z param (predictor for y)
                             selectInput(inputId = "zparam", 
                                         label = "Select explanatory variable for y",
                                         choices = c(names(uown_wq))),
                             
                             plotOutput("plot2")),
                    
                    tabPanel("Summary statistics", tableOutput("table1"),
                             h5("Table of summary statistics for y parameter, 
                                grouped by x parameter"))
                  #  tabPanel("Map of Stations"),
                  #  tabPanel("Data")
                  
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
  h6("Written by Beck R. Frydenborg (beck@frecologic.com), Written in the
     programming language R (R Development Core Team, 2015. Vienna, Austria.
     www.r-project.org version 3.3.2 (2016-10-31)."),
  h6("Disclaimer: This product is not intended for regulatory decisions.")
 )
  
  
  
  server <- function(input, output, session) {
  
         # make a boxplot 
        output$plot1 <- renderPlot({
                        p <- ggplot(uown_wq, 
                                    aes_string(x = input$xparam,
                                   y = input$yparam,
                                   fill = factor(input$yparam))) +
                        geom_boxplot() +
                                scale_fill_manual(values = c("green4"),
                                                  guide = guide_legend(title = NULL)) 
                                
                        
                # for facetting
                facets <- paste(input$facet_row, '~', input$facet_col)
                if (facets != '. ~ .')
                        p <- p + facet_grid(facets)  
                
                p
                
        })
          
        # make a scatterplot ox x and y
        output$plot2 <- renderPlot({
                ggplot(uown_wq, aes_string(x = input$zparam,
                                           y = input$yparam)) +
                        geom_point() +
                        geom_smooth(method = "lm")
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
                                           sd(., na.rm = TRUE)))
                
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
        
}

shinyApp(ui = ui, server = server)

