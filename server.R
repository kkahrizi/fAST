#Kamin Kahrizi, DiAssess 2018
if(!require(shiny)){install.packages("shiny")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gtable)){install.packages("gtable")}
if(!require(tidyr)){install.packages("tidyr")}
#if(!require(gridExtra)){install.packages("gridExtra")}
#if(!require(grid)){install.packages("grid")}

#Variables to find appropriate files 
signalFileToken <- "Quantification Amplification Results_SYBR.csv"
labelFileToken <- "Quantification Summary_0.csv"

#Initialize global variables
reactive_data <- reactiveValues()

#Function to show error dialog
show_error_dialog <- function(errorString){
      showModal(modalDialog(
                title = "ERROR!",
                errorString,
                easyClose = FALSE 
      ))
       
}

#Function to scan through files in path, return a data frame of sample file with labels as column headers
#e.g. Time Sample1 Sample2 
#        1    0.4    0.5   
#        2    0.4    0.5   
annotate_data <- function(path, convertToMins, conversionFactor, TTRoffset){
        #Scan through files in path for those matching sample and label tokens 
        files_in_folder<- list.files(path, full.names = TRUE)
        signalFileName <- NULL
        labelFileName <- NULL 
        for (fileName in files_in_folder){
            if ( grepl(signalFileToken, fileName) ){
                signalFileName <- fileName
            } else if ( grepl(labelFileToken, fileName) ){
                labelFileName <- fileName
            }
        
        } 

        #Shows dialog box if files could not be found
        if(is.null( signalFileName ) | is.null( labelFileName )){
            show_error_dialog("Missing either a data file, or sample file. Please check selected directory.")
            return(NA)
        }

        #Now that we know all files of interest exist, we read from them
        result <- tryCatch({
            signal_df <- data.frame(read.table(signalFileName, header = TRUE , sep = ',', blank.lines.skip = TRUE))
            signal_df <- signal_df[,colSums(is.na(signal_df)) < nrow(signal_df)]

        }, error = function(err) {
            show_error_dialog( paste("There is something wrong with:", signalFileName) )
            return(NA)     
        }) #END reading from signal file

        result <- tryCatch({
            label_df <- read.table(labelFileName, header = TRUE , sep = ',', blank.lines.skip = TRUE)
        }, error = function(err) {
            show_error_dialog(paste("There is something wrong with:", labelFileName))
            return(NA) 
        }) #END reading from label file

         
        #Convert from wide format to long
        signal_df_long <- gather( data = signal_df, key = "Well", value = "RFU", -Cycle)

        #If convertToMins is TRUE, convert cycles to minutes using conversionFactor
        if(convertToMins) {
            signal_df_long$Cycle <- (signal_df_long$Cycle * conversionFactor + TTRoffset) / 60
        }

        #Add columns with sample name and replicate number
        signal_df_long$Replicate <- 1
        signal_df_long$Sample <- NA
        for (well in unique(signal_df_long$Well)){
            #Well coordinates in signal file have no leading zeroes, but those in the label file do, so we make a conversion 
            label <- paste(substr(well, 1, 1), formatC(strtoi(substr(well, 2, 3)), width = 2, flag = "0"), sep = '')
            sample <- as.character(label_df$Sample[label_df$Well == label]) 
            if ( sample %in% signal_df_long$Sample ) {
                numOccurences = sum(!is.na(unique(signal_df_long$Well[signal_df_long$Sample == sample])))
                signal_df_long$Replicate[signal_df_long$Well == well] = signal_df_long$Replicate[signal_df_long$Well == well] + numOccurences 
            }
            signal_df_long$Sample[signal_df_long$Well == well] = sample 
             
        }
       
       signal_df_long$Replicate <- factor(signal_df_long$Replicate)
       return(signal_df_long) 
}

##Function to make a separate subplot for each unique column name in df
#makeSubPlots <- function(df){
#        plots <- list()
#        index = 1;
#        for (cond in unique(df$Sample)){
#            local({
#               print(cond)
#               toPlot <- df[df$Sample == cond, ]
#               plot <- ggplot(toPlot, aes( x = Cycle, y = RFU, col = Well )) +
#                    geom_line() +
#                    xlab("Cycle") +
#                    ylab("RFU") +
#                    ggtitle(cond)
#               plots[[index]] <<- plot
#               })
#            index = index + 1
#        } 
#        grobz <- lapply(plots, ggplotGrob)
#        format <- matrix(unlist(1:(index-1)), ncol = numPlotColumns, byrow = TRUE)
#
#        gt <- arrangeGrob(grobs = grobz, layout_matrix = format)
#y
#        return(gt)
#}


#Function to get TTR by midpoint method (TTR = midpoint of peak and baseline value)
getTTR_midpoint <- function(df_unsorted, baselineStart, baselineEnd, minDiff){
    df <- df_unsorted[order(df_unsorted$Cycle),]
    TTR_df <- data.frame(Well = NA, Sample = NA, TTR = NA, TTRSig = NA)
    TTRindex = 1
    for (well in unique(df$Well)) {
       if (TTRindex > 1){
                 TTR_df[nrow(TTR_df)+1,] <- NA
          
       }
       TTR_df$Well[TTRindex]  = well
       TTR_df$Sample[TTRindex] = as.character(unique(df$Sample[df$Well == well]))
       well_subset <- df[df$Well == well,]
       baselineValue <- mean(well_subset$RFU[ well_subset$Cycle > baselineStart & well_subset$Cycle < baselineEnd  ])
       peakValue <- max(well_subset$RFU)
                  
       if( peakValue - baselineValue < minDiff  ) {
            TTR_df$TTR[TTRindex] = NA
            TTR_df$TTRSig[TTRindex] = NA
          
        } else {
            midpoint = (baselineValue + peakValue ) / 2
            thresholdCycle = min(which(well_subset$RFU > midpoint))
            TTR_df$TTR[TTRindex] = well_subset$Cycle[thresholdCycle]
            TTR_df$TTRSig[TTRindex] = well_subset$RFU[thresholdCycle]
                
        }
            TTRindex = TTRindex + 1
            
    }
    return(TTR_df)

}

#Function to get TTR as inflection point of logistic fit curve
getTTR_logfit <- function(df_unsorted, minDiff){
  df <- df_unsorted[order(df_unsorted$Cycle),]
  TTR_df <- data.frame(Well = NA, Sample = NA, TTR = NA, TTRSig = NA, Scal = NA)
  TTRindex = 1
  for (well in unique(df$Well)) {
    if (TTRindex > 1){
      TTR_df[nrow(TTR_df)+1,] <- NA
      
    }
    TTR_df$Well[TTRindex]  = well
    TTR_df$Sample[TTRindex] = as.character(unique(df$Sample[df$Well == well]))
    well_subset <- df[df$Well == well,]
    baselineValue <- min(well_subset$RFU)
    peakValue <- max(well_subset$RFU)
    baselineCorrected <- well_subset
    baselineCorrected$RFU <- baselineCorrected$RFU - baselineValue + 1
    fitted <- nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal), data = baselineCorrected)
   
    asm <- summary(fitted)$coefficients[1]
    xmid <- summary(fitted)$coefficients[2]
    scal <- summary(fitted)$coefficients[3]
    
    if( peakValue - baselineValue < minDiff  ) {
      TTR_df$TTR[TTRindex] = NA
      TTR_df$TTRSig[TTRindex] = NA
      TTR_df$Scal[TTRindex] = NA
      
    } else {
      
      TTR_df$TTR[TTRindex] = xmid
      TTR_df$TTRSig[TTRindex] = SSlogis(xmid, asm, xmid, scal) + baselineValue
      TTR_df$Scal[TTRindex] = scal
      
    }
    TTRindex = TTRindex + 1
    
  }
  return(TTR_df)
}

#Function to make subplots and return a plot to be saved/displayed
makeSubPlots <- function(df, TTRdf, numPlotColumns, convertToMins){ 
    if(convertToMins){
        xAxis <- "Time (minutes)"
    } else {
        xAxis <- "Cycle"
    }
    df$Replicate <- factor(df$Replicate)
    plot <- ggplot(df, aes( x = Cycle, y = RFU, col = Replicate  )) +
    geom_line() + 
    xlab(xAxis) +
    ylab("RFU") + 
    facet_wrap(~Sample, ncol = numPlotColumns) +
    geom_point(aes(x = TTR, y = TTRSig), data = TTRdf, inherit.aes = FALSE ) 
  return(plot)
}



shinyServer(function(input, output, session) {
                observeEvent(
                               ignoreNULL = TRUE,
                               eventExpr = {
                                       input$directory
                               },
                               handlerExpr = {
                                   if (input$directory > 0) {
                                             
                                             # launch the directory selection dialog with initial path read from the widget
                                             reactive_data$path = choose.dir(default = readDirectoryInput(session, 'directory'))
                                             # update the widget value
                                             updateDirectoryInput(session, 'directory', value = reactive_data$path)
                                             
                                             #Search directory and extract relevant data from signal and label file    
                                             reactive_data$signal_df <- annotate_data(reactive_data$path, input$convertToMins,
                                                                                     input$cycleConversionFactor, 
                                                                                     input$TTRoffset)
                                            
                                             if (all(is.na(reactive_data$signal_df))){
                                                 return()
                                             } 
                                             if (input$TTRmethod == 'Midpoint'){
                                               reactive_data$TTRdf <- getTTR_midpoint(reactive_data$signal_df, 
                                                                        input$baselineStart,
                                                                        input$baselineEnd,
                                                                        input$minDiff)
                                             } else if (input$TTRmethod == 'Regression'){
                                               reactive_data$TTRdf <- getTTR_logfit(reactive_data$signal_df, input$minDiff)
                                             }
                                   
                              
                                             #For each unique sample name, make a subplot with each amplification curve
                                             reactive_data$output_plot <- makeSubPlots(reactive_data$signal_df,
                                                                                       reactive_data$TTRdf, input$numColumns,
                                                                                       input$convertToMins)
                                             output$Plots <- renderPlot({
                                                print(reactive_data$output_plot)
                                             }, height = input$plotHeight)
                                   }
                               }

                             )
                 observeEvent(
                              ignoreNULL = TRUE, 
                              eventExpr = {
                                input$analyze
                              },
                                handlerExpr = {
                                  reactive_data$signal_df <- annotate_data(reactive_data$path, input$convertToMins, 
                                                                                      input$cycleConversionFactor,
                                                                                      input$TTRoffset)
                                  if (all(is.na(reactive_data$signal_df))){
                                    return()
                                  } 
                                  if (input$TTRmethod == 'Midpoint'){
                                    reactive_data$TTRdf <- getTTR_midpoint(reactive_data$signal_df, 
                                                                    input$baselineStart,
                                                                    input$baselineEnd,
                                                                    input$minDiff)
                                  } else if (input$TTRmethod == 'Regression'){
                                    reactive_data$TTRdf <- getTTR_logfit(reactive_data$signal_df, input$minDiff)
                                  }
                                  reactive_data$output_plot <- makeSubPlots(reactive_data$signal_df,
                                                                            reactive_data$TTRdf, input$numColumns,
                                                                            input$convertToMins)
                                  output$Plots <- renderPlot({
                                    print(reactive_data$output_plot)
                                  }, height = input$plotHeight)
                              }
                              
                            )
                
})
