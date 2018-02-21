library(shiny)
version<-0.1
appName <- paste("fLAMP Analysis Software Tool v",version,sep='')
shinyUI(fluidPage(
titlePanel(appName),
                sidebarLayout(sidebarPanel(h2("Options"),
                                           br(),
                                           directoryInput('directory', label = 'Select a folder',value = '~'),
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           actionButton(inputId= "analyze", label = "Analyze!")),
                               mainPanel(h2("Amplification Plots"),
                                         plotOutput("Plots"))
                             )
                  )
)
