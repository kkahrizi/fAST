#Kamin Kahrizi, DiAssess 2018
library(shiny)
version<-0.1
appName <- paste("fLAMP Analysis Software Tool v",version,sep='')
shinyUI(fluidPage(
titlePanel(appName),
                sidebarLayout(sidebarPanel(h2("Options"),
                                           br(),
                                           directoryInput('directory', label = 'Select a folder',value = '~'),
                                           checkboxInput('convertToMins', label = 'Convert cycles to minutes', 
                                                         value = TRUE),
                                           numericInput('cycleConversionFactor', label = 'Seconds per cycle',
                                                        value = 38, min = 1, max = 1000, step = 1),
                                           numericInput('TTRoffset', label = 'Time offset (seconds added to time, can be negative)',
                                                        value = 0, min = -1E4, max = 1E4, step = 10), 
                                           radioButtons('TTRmethod', 'Select a TTR method',
                                                        choices = c('Midpoint','Regression'), 
                                                        selected = 'Regression'),
                                           numericInput('baselineStart', label = 'Baseline Start cycle',
                                                     value = 2, min = 1, max = 119, step = 1),
                                           numericInput('baselineEnd', label = 'Baseline End Cycle',
                                                     value = 8, min = 2, max = 120, step = 1),
                                           numericInput('minDiff', label = 'Amplitude Threshold',
                                                        value = 500, min = 0, max = 10000, step = 100),
                                           numericInput('numColumns', label = 'Number of plot columns',
                                                     value = 4, min = 2, max = 6, step = 1),
                                           numericInput('plotHeight', label = 'Height of plot area',
                                                     value = 600, min = 100, max = 10000, step = 100),
                                           br(), br(), br(), br(),
                                           actionButton(inputId = "analyze", label = "Re-Analyze!"),
                                           downloadButton("savePreferences", "Save preferences")),
                               mainPanel(h2("Amplification Plots"),
                                         actionButton(inputId = 'saveToFile', label = "Save data to folder"),
                                         br(), br(), br(), br(), br(),
                                         plotOutput("Plots")
                                         )
                             )
                  )
)
