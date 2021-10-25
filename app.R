#Purpose: To use health data to gain valuable insights to relationships and trends within the data
#using an interactive web app.
#Creator: Gabe Flores
#Last Modified: October 21st, 2021

#START OF SETUP
#Import libraries required to visualize data and display in a web app
library(shinythemes)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(rlang)
library(reshape2)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(stringr)
library(ggpubr)
library(broom)
library(DT)
library(zoo)
#End of Library Imports


#Import data that was exported after running the cleaning script
load('df.ofApollo.RData')
load('df.hr.RData')
#End of data Imports


#Create custom functions for data visualizations
#'df_rollingAvg': Takes in 3 vars. A df, period (size of the window), and minimum NAs to exclude a col from the output df
df_rollingAvg <- function(df, period, cutoffNa){
  df %>% 
    {if(period == 0){.} #if window is size 0, export original df
      else{
        df[,-1] %>% #exclude date col to prevent errors
        apply(2, function(x) rollmean(x, period, fill = NA)) %>% #calculate rolling mean column wise
        apply(2, function(x) round(x,1)) %>% #round calculations
        data.frame() %>%
        mutate(date = df[,1], .before = bw.in.lbs) %>% #reinsert original date col
        filter(rowMeans(is.na(.)) < .2) %>% #calculate average NA observations in a col
        select_if(~sum(!is.na(.)) > cutoffNa) #exclude cols with NA observation avg above threshold
        }
      }
}

#'hr_plotter': Takes heart rate data, and other visualization parameters to create a line plot of the heart rate data
#Variables: 'df.hr': input data frame; 'category'= type of heart rate data to be plotted e.g. sleep, workout, type
#Variables (cont.): 'coloring': option to color values using magnitude or type; 'index': used to select specific category ocurrence
#Variables (cont.): 'minDay' and 'maxDay' used to create x range for data viz
hr_plotter <- function(df.hr, category, coloring = 'value', index = 1, minDay = NA, maxDay = NA){
  df.hr <- df.hr %>% #filter the data based off of category and date range
    {if(category == 'workout plot'){
      filter(.,didExer.index == index)
    }
      else if(category == 'sleep plot'){
        filter(., wasAsleep.index == index)
      }
      else{
        if(!is.na(minDay) & is.na(maxDay)){
          filter(., as.Date(startDate) >= minDay)
        }
        else if(!is.na(minDay) & !is.na(maxDay)){
          filter(., as.Date(startDate) >= minDay & as.Date(startDate) <= maxDay)
        }
        else{.}
      }} %>%
    arrange(startDate) #sort the filtered data so that startDate is in ascending order
  df.hr %>% # map x,y, and color variables by coloring variable
    {if(coloring == 'weekday'){
      ggplot(., mapping = aes(x = as.POSIXct(startDate), y = value, color = weekdays(as.Date(startDate))))
    }
      else if(coloring == 'sleep'){
        ggplot(., mapping = aes(x = as.POSIXct(startDate), y =value, color = wasAsleep))
      }
      else if(coloring == 'workout'){
        ggplot(., mapping = aes(x = as.POSIXct(startDate), y = value, color = didExer))
      }
      else if(coloring == 'type'){
        ggplot(., mapping = aes(x = as.POSIXct(startDate), y = value, color = interaction(didExer, wasAsleep, lex.order = TRUE)))
      }
      else{
        ggplot(., mapping = aes(x = as.POSIXct(startDate), y = value, color = value))
      }} +
    geom_path(aes(group = 1), size = .9) + #define path parameters to connect observations
    geom_point(size = 1.25) +
    #If statement used to select appropriate title for the output plot
    {if(category == 'workout plot'){
      ggtitle(paste("Workout", index, "HR(t)", sep = " "))
    }
      else if(category == 'sleep plot'){
        ggtitle(paste("Sleep Occurrence", index, "HR(t)", sep = ' '))
      }
      else{
        if(coloring == 'type'){
          ggtitle("Activity HR(t) Plot")
        }
        else if(coloring == 'weekday'){
          ggtitle("Daily HR(t) Plot")
        }
        else{
          ggtitle("HR(t) Magnitude Plot")
        }
      }} +
    #Create appropriate subtitle dependent on output plot
    {if(category == 'workout plot') {
      labs(subtitle = paste(format(max(df.hr[['startDate']]), format = '%b %d'), 'From',
                            format(min(df.hr[['startDate']]), format = '%I:%M %p'), 'to',
                            format(max(df.hr[['startDate']]), format = '%I:%M %p')))
    }
      else if(category == 'sleep plot'){
        labs(subtitle = paste(format(max(df.hr[['startDate']]), format = '%b %d'), 'From',
                              format(min(df.hr[['startDate']]), format = '%I:%M %p'), 'to',
                              format(max(df.hr[['startDate']]), format = '%I:%M %p')))
      }
      else{
        labs(subtitle = paste(format(min(df.hr[['startDate']]), format = '%b %d'),
                              'to',
                              format(max(df.hr[['startDate']]),format = '%b %d'),
                              sep = ' '))
      }} +
    #visualization aesthetic mods
    xlab("Datetime of Observation (mmm dd - hh:mm)") +
    ylab("BPM") +
    theme(plot.title = element_text(face="bold", size = 18),
          plot.subtitle = element_text(size = 16),
          axis.title.x = element_text(face = 'bold', size = 14, angle = 0),
          axis.title.y = element_text(face = 'bold', size = 14, angle = 90),
          axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))
}

#'eye.of.apollo.scatter': takes in df and visualization parameters to produce a scatter plot or a time series line plot
#Inputs:
#'pCol','sCol','tCol': X-, Y-, and coloring variables, respectively
#'withLine': bool var, used to generate line plot using date of observations if TRUE
#'withStats': bool var, used to generate a regression line if TRUE
#'leadY': bool var, used to lead Y variable in records to account for time delay in effects of X Var on Y Var
eye.of.apollo.scatter <- function(df, pCol, sCol, tCol = NULL,
                                  withLine = NULL, withStats = FALSE,leadY = FALSE){
  df <- df %>%
    filter((!is.na(.[[pCol]]) & !is.na(.[[sCol]])) & !is.na(.[[tCol]])) #exclude any rows with NA observations
  df %>% #assign X,Y, and color variables to respective df cols
    ggplot(., mapping = aes(x = .data[[pCol]], y =.data[[sCol]], color = .data[[tCol]])) +
    geom_point(size = 4) +
    #Optional aesthetics to be generated
    {if(withLine)geom_path(size = 1)} +
    {if(withStats) geom_smooth(formula = y ~ x, method = "lm")} +
    #Title, theme, and label options
    ggtitle(paste(sCol, 'Vs.', pCol, sep = ' ')) +
    theme(plot.title = element_text(face="bold", color="black", size=28),
          plot.subtitle = element_text(size=20, face="italic", color="black"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text=element_text(size=16)) +
    labs(subtitle = paste(min(format(df[,'date'], format = '%b-%d')), "to",
                          max(format(df[,'date'], format = '%b-%d')), sep = " ")) +
    xlab(pCol) + ylab(sCol)
}

#'eye.of.apollo.hist': takes df and visualization parameters to produce a histogram
#Inputs: 'df' input data frame
#'pCol', variable to plot obesrvations on histogram
#'binNum', number of bins to plot by on histogram
#'sCol' & 'border', used to color in plot
eye.of.apollo.hist <- function(df, pCol, binNum = 30 , sCol = I('orange'), border = 'black'){
  border <- I(border) #assign border color based on user input
  df %>%
    #map x, color, and fill vars to visualization
    ggplot(mapping = aes(x = .data[[pCol]], color = border, fill = sCol)) +
    #set histogram viz parameters
    geom_histogram(bins = binNum, aes(y=..density..)) +
    #generate line down the mean of the histogram
    geom_vline(aes(xintercept = mean(.data[[pCol]])),
               color="blue", linetype="dashed", size=2.5) +
    #density histogram viz parameters
    geom_density(alpha=.3, fill="#FF6666") +
    #labeling and title aesthetics
    ggtitle(paste('Histogram Plot Using Variable Count of', pCol, sep = ' ')) +
    labs(subtitle = paste(min(format(df[,'date'], format = '%b-%d')), "to",
                          max(format(df[,'date'], format = '%b-%d')), sep = " ")) +
    xlab(pCol)
}

#'biometric_heatmap': used to produce a heat map using input df, variables of interest, and statistical parameters
# Input: 'minDay' and 'maxDay' used to create x range for data viz
# 'df' input data frame
# 'pVal' and 'rVal', max and min statistical values, respectively, set to filter out any insignificant or weak observations
# 'input_list', list of variables to generate heat map
# 'leadVars' list of variables to generate heat map by leading observation by a day
biometric_heatmap <- function(df, pVal, rVal, leadVars = NA, input_list = NA, minDay = NA, maxDay = NA){
  #generate correlation matrix using input data frame and exclude anything outside statistical parameters
  formatted_cors <- function(df, pVal, rVal){
    cors <- function(df) {
      # turn all three matrices (r, n, and P into a data frame)
      M <- Hmisc::rcorr(as.matrix(df))
      # return the three data frames in a list return(Mdf)
      Mdf <- map(M, ~data.frame(.x))
    }
    cors(df) %>%
      map(~rownames_to_column(.x, var="measure1")) %>%
      # format each data set (r,P,n) long
      map(~pivot_longer(.x, -measure1, "measure2")) %>%
      bind_rows(.id = "id") %>%
      pivot_wider(names_from = id, values_from = value) %>%
      #filter anything grater than specified PVal and anything less than specified RVal
      mutate(sig_p = ifelse(P < pVal, T, F),
             p_if_sig = ifelse(P < pVal, P, NA),
             r_if_sig = ifelse(abs(as.numeric(r)) > rVal, r, NA))
  }
  #filter data frame using list of input variable to be analyzed
  df <- df[,unique(append(input_list, c(leadVars, 'date')))] %>%
    #filter data frame based on date range
    {if(!is.na(minDay) & is.na(maxDay)){
      filter(., date >= minDay)
    }
      else if(!is.na(minDay) & !is.na(maxDay)){
        filter(., date >= minDay & date <= maxDay)
      }
      else{.}} %>%
    #for lead Variables, lead the observations by a record (i.e. push lead variables a day ahead)
    mutate_at(.vars = leadVars, .funs = function(x) lead(x, n = 1)) %>%
    select(-date) #exclude dates
  #use modified df to generate correlation matrix using custom function above
  formatted_cors(df, pVal, rVal) %>%
    #map measure, fill, and labels to plot
    ggplot(aes(measure1, measure2, fill = r, label = ifelse(!is.na(p_if_sig),  round(r_if_sig,2), NA))) +
    geom_tile() +
    #assign labels to correlation matrix
    labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in Health Metrics",
         subtitle= paste("Pearson's correlation coefficients above r = ", 
                         rVal, " & significant below p = ", pVal, sep = ''))+
    #assign colors to heat map depending on magnitude of the Pearson Correlation Coefficient
    scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
    #modifications to text, labels, and titles
    geom_text(size = 8) +
    theme(plot.title = element_text(face="bold", color="black", size=28),
          plot.subtitle = element_text(size=20, face="italic", color="black"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text=element_text(size=16)) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
}
#End of custom functions
#END OF SETUP


#START OF USER INTERFACE CODE
ui <- fluidPage(theme = shinytheme('cosmo'),
  navbarPage(title =  "Health Informatics",
             id = 'tabs',
             tabPanel(title = 'Home',
                      value = 'pg1',
                      #START HOME PAGE LAYOUT
                      imageOutput("homeLogo", height = "200px"),
                      br(),
                      hr(),
                      h2(strong("Project Description")),
                      p(style = "text-align: justify; text-indent:40px; font-size:20px",
                      "This web app focuses on analyzing health data and was developed with the intention to learn and practice R through
                      real life project experience. Data collection began on August 22nd, 2021 and is still currently being collected for 
                      data-driven decision making. The project's data sources include health data collected through an apple watch series 6 
                      and recorded text files in the notes app on my iPhone."),
                      br(),
                      p(style="text-align: justify; text-indent:40px; font-size : 20px",
                      "After the health data is collected, it is then cleaned and processed using R for future data analysis techniques. However,
                      nutritional notes taken are imported into excel where the nutrition of foods and supplements consumed are then compiled into
                      a daily averaged table. The excel file contains both health metrics such as weight and nutritional information. Once all data is
                      updated, r is used to clean and format all data into averaged statistics for data visualization and analysis. "),
                      br(),
                      p(style="text-align: justify; text-indent:40px; font-size : 20px",
                        "The web application currently open is separated into various tabs which each include their own data visualizations such
                        as scatter plots, heat maps, or histograms. Additionally, statistics are included with each visualization to get a feel for 
                        what behaviours the health data tends to make. Filters are also used to reduce quantity of data displayed and make it easier
                        to investigate relationships between variables within the data."),
                      p(style="text-align: justify; font-size : 20px",
                        h3(strong("Description of Visualizations:"))),
                      tags$div(
                        tags$ul(
                          tags$li(style="text-align: justify; font-size : 20px",
                          strong("Scatter Plot"), ", Up to three variables can be selected and used for both visualization and regression
                                  analysis purposes. The variables are broken down into Y, X1, & X2. However, X2 is exempt from regression
                                  analysis by default. If the user wishes to include X2 into the model, then the option is there as a check
                                  box. A date filter is also included for the user to focus on specific periods in time that are of interest.
                                  The last important note would be the lag Y variable check box. This is an available option to improve the
                                  regression model since nutrition the day before could affect the variable Y such as weight or BF%"),
                          tags$ul(  
                          tags$li(style="text-align: justify; font-size : 18px",
                                    em("Interesting Relationships:")),
                          tags$ul(
                            tags$li(style = "text-align: justify; font-size : 17px", 
                                    "change.in.bwInLbs Vs. calories.in.Cal"),
                            tags$ul(
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Lead the Y variable"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Check the Regression Line box"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Caloric maintenance can be determined by increasing the rolling window and calculating at what point 
                                      the regression line intercepts the x-axis.")
                            ),
                            tags$li(style = "text-align: justify; font-size : 17px", 
                                    "activeEnergy.inCal Vs. workVol.inLbs "),
                            tags$ul(
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include regrssion line"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Set X2 to 'numSteps'"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include X2 in Regression Analysis")
                            ),
                            tags$li(style = "text-align: justify; font-size : 17px", 
                                    "basalEnergy.inCal Vs. avg.sleep.hr.bpm"),
                            tags$ul(
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include Regression Line"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Set X2 to 'numSteps"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include X2 in regression analysis")
                            )
                            ,                            
                            tags$li(style = "text-align: justify; font-size : 17px", 
                                    "workVol.inLbs Vs. numReps"),
                            tags$ul(
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include Regression Line"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Set X2 to 'numSets"),
                              tags$li(style = "text-align: justify; font-size : 16px",
                                      "Include X2 in regression analysis")
                            )
                          )
                          )
                          )
                      ),
                      tags$ul(
                        tags$li(style="text-align: justify; font-size : 20px",
                                strong("Heat Map"), ", relationships between variabels such as basalEnergy.inCal and avg.sleep.hr.bpm were 
                                found using a heat map. The heat map initializes without any selected variables leading to an initial error
                                message which will be fixed in future versions. However, after a selection by the user is made, the heat map
                                will display the pearson's correlation coefficient between variables. The significance value (p value) can be
                                adjusted by the user to a minimum of the observations being observed occuring 1 time out of a 100 or to a
                                maximum of 10 times of a 100. Furthermore, The minimum R coefficient can also be adjusted to filter out
                                variables that do not have a strong linear relationship between them. Filters such as date range and 
                                a rolling window were also kept to verify statistics between the scatter plot and heat map produced for
                                consistency, but can be used to focus on specific time periods as well. ")
                      ),
                      tags$ul(
                        tags$li(style="text-align: justify; font-size : 20px",
                                strong("Heart Rate Plot"), ", this tab was added to help the user visualize the patterns of heart rate
                                values throughout the day. The plot can be used to focus on heart rate values during individual workout
                                or sleep sessions. An example would be to do the following:"),
                        tags$ul(
                          tags$li(style="text-align: justify; font-size : 18px",
                                  "For the input 'Select Plot Type:', select 'workout'."
                          ),
                          tags$li(style="text-align: justify; font-size : 18px",
                                  "The coloring option will not affect the observation very much. However, it is recommended
                                  to set the option to 'value'."),
                          tags$li(style="text-align: justify; font-size : 18px",
                                  "Lastly, the user can observe the differences between workout sessions by varying the index
                                  number in the 'Select Index' tab.")
                        ),
                        tags$li(style="text-align: justify; font-size : 20px",
                          "Notice how index 11, 17, and 23 rise in a linear manner in the start of the 
                          session. However, all three sessions begin to taper off as time evolves. This is much different
                          in comparison to sessions 18 and 24, where there are peaks and valleys in the plot. This demonstrates
                          the difference between cardio and traditional resistance training and their affects on the heart."
                        )
                      ),
                      tags$ul(
                        tags$li(style="text-align: justify; font-size : 20px",
                                strong("Histogram Plot"), ", this tab contains a density histogram with a select input to observe the 
                                distribution of observations over a given time period using the filters provided. Additonally, a
                                statistical summary is produced below on the left hand side of the plot. The mean  in the statistical 
                                summary corresponds to the dashed blue line in the plot. To the right of the statistical summary is a
                                data table with two columns one being the 'date' column and the other being the selected variable.")
                        ),
                      hr()
                      #END OF HOME PAGE LAYOUT
             ),
             tabPanel(title = 'Scatter Plot',
                      value = 'pg2',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "txtY", label = '', choices = uiOutput('txtY')),
                                     selectInput(inputId = "txtX1", label = '', choices = uiOutput('txtX1')),
                                     selectInput(inputId = "txtX2", label = '', choices =uiOutput('txtX2')),
                                     dateRangeInput('datesS', "Date Range:",
                                         start = min(df.ofApollo$date),
                                         end = max(df.ofApollo$date),
                                         min = min(df.ofApollo$date),
                                         max = max(df.ofApollo$date),
                                         separator = ' to '),
                                     checkboxInput(inputId = "leadY", label = "Lead Y observations by a day", value = FALSE),
                                     checkboxInput(inputId = "lineS", label = "Connect observations Using a Line:", value = FALSE),
                                     checkboxInput(inputId = "statsS", label = "Include Regression Line:", value = FALSE),
                                     checkboxInput(inputId = 'var3', label = "Include X2 In Regression Analysis:", value = FALSE),
                                     sliderInput(inputId = 'kWindowPg2', label = 'Select Integer for Rolling Average Window:', min = 0, max = 10, value = 0, step = 1)
                                     ),
                        mainPanel(width = 9,
                          fluidRow(
                            column(12,plotOutput('plot_scatter', hover = "plot_hover", brush = "plot_brush", height = 350))
                            ),
                          fluidRow(
                            column(6, verbatimTextOutput("info"),
                            tags$head(tags$style(HTML("
                            #info {
                            font-size: 13px;
                            }
                                                      ")))
                            ),
                            column(6, style='padding-bottom:0px;', verbatimTextOutput('lmReg'),
                            tags$head(tags$style(HTML("
                            #lmReg {
                            font-size: 13px;
                            }
                                                      ")))
                            )
                          ),
                          fluidRow(
                            column(6,dataTableOutput('aggregatedData')),
                            column(6, style='padding-top:0px;', verbatimTextOutput('modelSummary'),
                            tags$head(tags$style(HTML("
                            #modelSummary {
                            font-size: 12px;
                            }
                                                      ")))
                            )
                            )
                          )
                      )
             ),
             tabPanel(title = 'Heat Plot',
                      value = 'pg3',
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = 'rVal',
                                      label = 'Filter by minimum r squared value:',
                                      min = 0,
                                      max = 1,
                                      value = 0,
                                      step = .1),
                          sliderInput(inputId = 'pVal', 
                                      label = 'Filter by minimum p value threshold:',
                                      min = 0,
                                      max = .1,
                                      value = .01,
                                      step = .01),
                          dateRangeInput(inputId = 'datesHM', label = "Date Range:",
                                         start = min(df.ofApollo$date),
                                         end = max(df.ofApollo$date),
                                         min = min(df.ofApollo$date),
                                         max = max(df.ofApollo$date),
                                         separator = ' to '),
                          sliderInput(inputId = 'kWindowPg3', label = 'Select Integer for Rolling Average Window:', min = 0, max = 10, value = 0, step = 1),
                          fluidRow(
                            splitLayout(
                              checkboxGroupInput(inputId = 'heatMapCols', label = 'Select Columns To Analyze:',
                                                choices = uiOutput('heatMapCols')),
                              checkboxGroupInput(inputId = 'heatMapLagCols', label = 'Select Columns To Analyze After Lag:',
                                                choices = uiOutput('heatMapLagCols'))
                            )
                          )
                        ),
                        mainPanel(
                          plotOutput(outputId = 'heatMap', height = "700px"),
                          dataTableOutput(outputId = "text")
                          )
                      )
             ),
             tabPanel(title = 'Heart Rate Plot',
                      value = 'pg4',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput(inputId = "txt0",
                                      label =  "Select Plot Type:",
                                      choices =  c('workout plot', 'sleep plot', 'cumulative plot'),
                                      selected = 'cumulative plot'),
                          selectInput(inputId = "txtCol",
                                      label = "Select Coloring Options",
                                      choices = c('workout', 'sleep', 'weekday', 'type', 'value'),
                                      selected = 'weekday'),
                          numericInput(inputId = "hr_index",
                                       label = "Select Index:",
                                       value = 1,
                                       min = 1,
                                       max = max(df_hr$wasAsleep.index),
                                       step = 1),
                          dateRangeInput(inputId = 'dates',
                                         label = "Date Range:",
                                         start = max(df.ofApollo$date) - 7,
                                         end = max(df.ofApollo$date),
                                         min = min(df.ofApollo$date),
                                         max = max(df.ofApollo$date),
                                         separator = ' to '),
                          checkboxInput(inputId = "dateButtonHR",
                                        label = "Date Entry",
                                        value = FALSE)
                        ),
                        mainPanel(width = 9,
                                  fluidRow(column(12, plotOutput(outputId = 'hrPlot', brush = 'HRplot_brush'))),
                                  fluidRow(
                                    column(6, verbatimTextOutput(outputId = 'HRinfo')),
                                    column(6, dataTableOutput(outputId = 'HRaggregatedData'))
                                  )
                        )
                      )
             ),
             tabPanel(title = 'Histogram Plot',
                      value = 'pg5',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput(inputId = "txt4",
                                      label = "Select variable:",
                                      choices = colnames(df.ofApollo)[-1],
                                      selected = 'bw.in.lbs'),
                          sliderInput('bins', 'Select number of Bins:', value = 7, step = 1, min = 1, max = 50),
                          dateRangeInput(inputId = 'datesH',
                                         label = "Date Range:",
                                         start = min(df.ofApollo$date),
                                         end = max(df.ofApollo$date),
                                         min = min(df.ofApollo$date),
                                         max = max(df.ofApollo$date),
                                         separator = ' to ')
                          ),
                        mainPanel(width = 9,
                          plotOutput(outputId = 'histPlot'),
                          fluidRow(
                          column(4,verbatimTextOutput(outputId = 'histStats'), style='padding-top:8px;',
                                 tags$head(tags$style(HTML("
                            #histStats {
                            font-size: 16px;
                            }
                                                      ")))),
                          column(8,dataTableOutput(outputId = 'histDT'), style='padding-top:8px;')
                          )
                          )
                        )
                      )
             )
  )
server <- function(input, output, session){
  df.temp <- reactive({
    if(input$tabs == 'pg2'){
    df_rollingAvg(df.ofApollo, input$kWindowPg2, 5) %>%
        #filter NA y observations if Y var exists
        {if(input$txtY %in% colnames(.)){ filter(.,!is.na(.data[[input$txtY]])) }
          else { filter(., !is.na(.data[['bw.in.lbs']])) } #if Y var doesn't exists, filter using 'bw.in.lbs'
        } %>%
        #lead Y var if user has selected the input
        { if(input$leadY){ mutate(., !!as.symbol(input$txtY) := c(tail(.data[[input$txtY]], -1), NA)) }
          else{.}} %>%
        # filter na observations for both X1 and X2
          filter(!is.na(.data[[input$txtX1]]) & !is.na(.data[[input$txtX2]])) %>%
        #filter using date range
        {if(!is.na(input$datesS[1]) & is.na(input$datesS[2])){ filter(., date >= input$datesS[1]) }
          else if(!is.na(input$datesS[1]) & !is.na(input$datesS[2])){ filter(., date >= input$datesS[1] & date <= input$datesS[2]) }
          else{.}
        }
    }
    else if(input$tabs == 'pg3'){
      df_rollingAvg(df.ofApollo, input$kWindowPg3, 5) %>%
        #filter using date range
        {if(!is.na(input$datesHM[1]) & is.na(input$datesHM[2])){ #if max date range does not exists
          filter(., date >= input$datesHM[1])}
          else if(!is.na(input$datesHM[1]) & !is.na(input$datesHM[2])){ #if both date ranges exists
            filter(., date >= input$datesHM[1] & date <= input$datesHM[2])}
          else{.}}# if both dates don't exists
    }
      else{
    df.ofApollo %>%
          #date filter
          {if(!is.na(input$datesH[1]) & is.na(input$datesH[2])){
            filter(., date >= input$datesH[1])} #filter if max date range does not exist
            else if(!is.na(input$datesH[1]) & !is.na(input$datesH[2])){
              filter(., date >= input$datesH[1] & date <= input$datesH[2])} #if both date inputs exist
            else{.}} # if no dates are selecfted    
  }
  })
  #server section: scatter plot processing
  observeEvent(input$kWindowPg2,{
    # Can also set the label and select items
    updateSelectInput(session, "txtY",
                      label = "Select Y variable:",
                      choices = colnames(data.frame(df.temp())),
                      selected = ifelse(input$txtY %in% colnames(data.frame(df.temp())),
                                        input$txtY,
                                        'bw.in.lbs')
    )
    updateSelectInput(session, "txtX1",
                      label = "Select X1 variable:",
                      choices = colnames(data.frame(df.temp())),
                      selected = ifelse(input$txtX1 %in% colnames(data.frame(df.temp())),
                                        input$txtX1,
                                        'date')
    )
    updateSelectInput(session, "txtX2",
                      label = "Select X2 variable:",
                      choices = colnames(data.frame(df.temp())),
                      selected = ifelse(input$txtX2 %in% colnames(data.frame(df.temp())),
                                        input$txtX2,
                                        'calories.in.Cal')
    )
  }) #update select Inputs on Scatter Plot Tab
  output$plot_scatter <- renderPlot({
    eye.of.apollo.scatter(df.temp(), input$txtX1, input$txtY, input$txtX2,
                          input$lineS, input$statsS,input$leadY)
  })#generate scatter plot
  #code plot to be interactive
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      if(input$txtX1 == 'date'){
        paste0("x: ", as.Date(e$x, origin = "1970-01-01"),"\t y: ", round(e$y,1), "\n")
      }
      else{
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      if(input$txtX1 == 'date'){
        paste0("xmin: ", as.Date(e$xmin, origin = "1970-01-01"), " xmax: ", as.Date(e$xmax, origin = "1970-01-01"),"\n", 
              "ymin: ", round(e$ymin, 1), " ymax: ", round(e$ymax, 1))
      }
      else{
        paste0("xmin: ", round(e$xmin, 1), " xmax: ", round(e$xmax, 1), "\n", 
               "ymin: ", round(e$ymin, 1), " ymax: ", round(e$ymax, 1))
      }
    }
    paste0(
      "Hover: ", xy_str(input$plot_hover),
      "Box Selection: \n", xy_range_str(input$plot_brush)
    )
  }) #display selected plot information
  output$homeLogo <- renderImage({
    list(src = "homeLogo.png",
         width = "100%")
  }, deleteFile = FALSE)
  
  output$aggregatedData <- renderDataTable({
    df.temp() %>%
      select("date",input$txtX1, input$txtX2, input$txtY) %>%
      filter(date >= input$datesS[1] & date <= input$datesS[2]) %>%
      {if(is.null(input$plot_brush)){.}
        else{
          filter(., .[[input$txtX1]] >= input$plot_brush$xmin & .[[input$txtX1]] <= input$plot_brush$xmax)
          }
        } %>%
      {if(is.null(input$plot_brush)){.}
        else{
          filter(., .[[input$txtY]] >= input$plot_brush$ymin & .[[input$txtY]] <= input$plot_brush$ymax)
          }
        } %>%
      DT::datatable(options = list(lengthMenu = list(c(5, 8, 10, 15, -1), c('5', '8', '10', '15', 'All'))))
  }) #render DT using interactive and date filters
  myformula <- reactive({ if(!input$var3){
    as.formula(paste(input$txtY, " ~ ", input$txtX1))
  }
    else{
      as.formula(paste(input$txtY, " ~ ", input$txtX1, '+', input$txtX2))
    }
  }) # create regression formula
  output$lmReg <- renderPrint({
    myformula()
  }) #display regression formula
  output$modelSummary <- renderPrint({
    summary(lm(myformula(), df.temp()))
  }) #print linear model summary
  # end of Scatter Plot Tab Server Functions
  
  #server section: heat map processing
  observeEvent(input$kWindowPg3,{
    pre_selected <- input$heatMapCols
    pre_selectedLag <- input$heatMapLagCols
    updateCheckboxGroupInput(session, inputId = 'heatMapCols', label = 'Select Columns To Analyze', 
                             choices = colnames(data.frame(df.temp()))[-1],
                             selected = input$heatMapCols
                             )
    
    pres_selectedLag <- input$heatMapLagCols
    updateCheckboxGroupInput(session, inputId = 'heatMapLagCols', label = 'Select Columns To Analyze Post Lag', 
                             choices = colnames(data.frame(df.temp()))[-1],
                             selected = input$heatMapLagCols
                             )
  })
  output$heatMap <- renderPlot({
    biometric_heatmap(df.temp(), input$pVal, input$rVal, input$heatMapLagCols, input$heatMapCols,
                      minDay = input$datesHM[1], maxDay = input$datesHM[2])
  })

  #End, server section: histogram processing
  
  #Start, server section: Heart Rate Processing
  output$hrPlot <- renderPlot({
    hr_plotter(df_hr, category = input$txt0, coloring = input$txtCol, index = input$hr_index,
               minDay = input$dates[1], maxDay = input$dates[2])
  })
  output$HRinfo <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      else{
        paste0("xmin: ", as.POSIXct(e$xmin, format="%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00"),
               " xmax: ", as.POSIXct(e$xmax, format="%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00"),"\n", 
               "ymin: ", round(e$ymin, 1), " ymax: ", round(e$ymax, 1))
      }
    }
    paste0(
      "Box Selection: \n", xy_range_str(input$HRplot_brush)
    )
  }) #display selected plot information
  output$HRaggregatedData <- renderDataTable({
    df_hr %>%
      select(startDate,value) %>%
      {if(is.null(input$HRplot_brush)){.}
        else{
          filter(., startDate >= input$HRplot_brush$xmin & startDate <= input$HRplot_brush$xmax)
        }
      } %>%
      {if(is.null(input$HRplot_brush)){.}
        else{
          filter(., value >= input$HRplot_brush$ymin & value <= input$HRplot_brush$ymax)
        }
      } %>%
      DT::datatable(options = list(lengthMenu = list(c(5, 8, 10, 15, -1), c('5', '8', '10', '15', 'All'))))
  }) #render DT using interactive and date filters
  #End, server section: Heart Rate Processing
  
  #Start, server section: Histogram Processing
  output$histPlot <- renderPlot({
    eye.of.apollo.hist(df.temp(), input$txt4, input$bins)
  })
  output$histStats <- renderPrint({
    cbind(round(summary(df.temp()[[input$txt4]]), 1))
  })
  output$histDT <- renderDataTable({
    df.temp()[,c('date',input$txt4)] %>%
      DT::datatable(options = list(lengthMenu = list(c(5, 8, 10, 15, -1), c('5', '8', '10', '15', 'All'))))
  })
}
shinyApp(ui = ui,server = server)