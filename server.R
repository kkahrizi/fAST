#Kamin Kahrizi, DiAssess 2018
if(!require(shiny)){install.packages("shiny")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gtable)){install.packages("gtable")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(testit)){install.packages("testit")}
#if(!require(gridExtra)){install.packages("gridExtra")}
#if(!require(grid)){install.packages("grid")}

#Variables to find appropriate files 
signalFileToken <- "Quantification Amplification Results_SYBR.csv"
labelFileToken <- "Quantification Summary_0.csv"
preferenceFile <- "preferences.csv"
TTRfile <- "TTR_replicates.csv"
rawDataFile <- "raw_data_long.csv"
amplificationCurvesFile <- "amplification_curves.png"

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
    #First extract data for just one well
    TTR_df$Well[TTRindex]  = well
    TTR_df$Sample[TTRindex] = as.character(unique(df$Sample[df$Well == well]))
    well_subset <- df[df$Well == well,]
    if(unique(well_subset$Sample) == "Pan NTCs"){
      print(well_subset)
    }
    #Find baseline and peak values to use to target the fit window 
    #around the linear phase. The window is defined as starting from
    #15 data points before the midpoint and extending to the peak value within
    #35 data points after the midpoint 
    baselineValue <- mean(well_subset$RFU[1:6])
    peakValue <- max(well_subset$RFU)
    midValue = (baselineValue + peakValue) / 2
    
    asm <- NA
    xmid <- NA
    scal <- NA
    midfitValue <- NA
    if (midValue - baselineValue > minDiff) {
      midpoint = min(which(well_subset$RFU > midValue))
      peakpoint = which(well_subset$RFU == 
                          max(well_subset$RFU[well_subset$Cycle < midpoint + 35]))
      
      fitWindow <- well_subset[(midpoint - 10):peakpoint,]
      
      fitWindow_baseline <- min(fitWindow$RFU)
      fitWindow$RFU <- fitWindow$RFU - fitWindow_baseline + 1
      
      if (!has_error(fitted <-
                     nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal),
                         data = fitWindow))) {
        asm <- summary(fitted)$coefficients[1]
        xmid <- summary(fitted)$coefficients[2]
        scal <- summary(fitted)$coefficients[3]
        if (xmid < 0 | is.na(xmid)) {
          showNotification(
            paste(
              "Some of your positive data could not fit a logistic curve. Reverting to midpoint method for well",
              well,
              sep = ' '
            )
          )
          xmid = well_subset$Cycle[midpoint]
          scal = NA
        } else {
          #For poor fits, value at midpoint is sometimes a smidge off real data.
          #Correct by adding the smallest difference from the real data at the closest cycle
          closestCycleIndex <- which.min(abs(well_subset$Cycle - xmid))
          closestCycle_RFU <- well_subset$RFU[closestCycleIndex]
          midfitValue <- closestCycle_RFU
          # midfitValue <- SSlogis(xmid, asm, xmid, scal) + fitWindow_baseline
          # differences <- well_subset$RFU - midfitValue
          # midfitCorrection <- differences[which.min(abs(differences))]
          # midfitValue = midfitValue + midfitCorrection
          # 
          
        }
        
      } else {
        showNotification(
          paste(
            "Some of your positive data could not fit a logistic curve. Reverting to midpoint method for well",
            well,
            sep = ' '
          )
        )
        xmid = well_subset$Cycle[midpoint]
        scal = NA
      }
      
    }
    
    TTR_df$TTR[TTRindex] = xmid
    TTR_df$TTRSig[TTRindex] = midfitValue
    TTR_df$Scal[TTRindex] = scal
    
    
    # baselineCorrected <- well_subset
    # baselineCorrected$RFU <- baselineCorrected$RFU - baselineValue + 1
    # dropped = 0
    # maxDropped <- 50
    # 
    # asm <- NA
    # xmid <- NA
    # scal <- NA
    # if (!(peakValue - baselineValue < minDiff)) {
    #   result <- tryCatch({
    #     while (has_error(nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal), data = baselineCorrected)) &
    #            dropped < maxDropped) {
    #       baselineCorrected <-
    #         subset(baselineCorrected,
    #                Cycle < max(baselineCorrected$Cycle) - 1)
    #       dropped = dropped + 1
    #     }
    #     fitted <-
    #       nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal), data = baselineCorrected)
    #     
    #     asm <- summary(fitted)$coefficients[1]
    #     xmid <- summary(fitted)$coefficients[2]
    #     if (xmid < 0) {
    #       showNotification("Some of your data could not fit a logistic curve,
    #                        possibly because it was negative")
    #       xmid = 0
    #     }
    #     scal <- summary(fitted)$coefficients[3]
    #   }, error = function(err) {
    #     show_error_dialog(
    #       paste(
    #         "There may be something wrong with log regression of this data. Try
    #         the midpoint TTR formula and let Kamin know"
    #       )
    #       )
    #     return(NA)
    #   }) #END reading from label file
    #   }
   
    
    # if( is.na(asm) | is.na(xmid) | is.na(scal)  ) {
    #   TTR_df$TTR[TTRindex] = NA
    #   TTR_df$TTRSig[TTRindex] = NA
    #   TTR_df$Scal[TTRindex] = NA
    # } else {
    #   
    #   TTR_df$TTR[TTRindex] = xmid
    #   TTR_df$TTRSig[TTRindex] = SSlogis(xmid, asm, xmid, scal) + baselineValue
    #   TTR_df$Scal[TTRindex] = scal
    #   
    # }
    TTRindex = TTRindex + 1
    
  }
  return(TTR_df)
}

#Function to make subplots and return a plot to be saved/displayed
makeSubPlots <- function(df, TTRdf, numPlotColumns, convertToMins, maxCycles){ 
    if(!is.na(maxCycles)){
      df <- subset(df, df$Cycle < maxCycles)
      TTRdf <- subset(TTRdf, TTRdf$TTR < maxCycles)
    }
  
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
  #Load user preferences from preference file, and apply them to respective fields
  result <- tryCatch({
    userPreferences <- read.csv(preferenceFile)
    updateDirectoryInput(session, 'directory', value = as.character(userPreferences$defaultFolder))
    reactive_data$path = userPreferences$defaultFolder
    updateNumericInput(session = session, inputId = "cycleConversionFactor", 
                       value = as.numeric(userPreferences$conversionFactor) )  
    updateNumericInput(session = session, inputId = "TTRoffset", 
                       value = as.numeric(userPreferences$TTRoffset) ) 
    updateNumericInput(session = session, inputId = "baselineStart", 
                       value = as.numeric(userPreferences$baselineStart) )
    updateNumericInput(session = session, inputId = "baselineEnd", 
                       value = as.numeric(userPreferences$baselineEnd) )
    updateNumericInput(session = session, inputId = "minDiff", 
                       value = as.numeric(userPreferences$amplitudeThreshold) )
    updateNumericInput(session = session, inputId = "plotHeight", 
                       value = as.numeric(userPreferences$plotHeight) )
    updateNumericInput(session = session, inputId = "numColumns", 
                       value = as.numeric(userPreferences$numPlotColumns) )
    updateNumericInput(session = session, inputId = "plotLimit",
                       value = as.numeric(userPreferences$plotLimit))
    updateCheckboxInput(session = session, inputId = "convertToMins", 
                        value = as.logical(userPreferences$convert))
    updateRadioButtons(session = session, inputId = "TTRmethod",
                       selected = as.character(userPreferences$Method))
    showNotification("Preferences loaded successfully.")
  }, error = function(err) {
    showNotification("Could not find a preferences folder. Using app defaults")
    return(NA) 
  }) 
  
  
  
  #Observe event for saving data
  #Saves preferences to same folder as raw data
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$saveToFile
               },
               handlerExpr = {
                result <- tryCatch({
                   TTRloc <- paste(reactive_data$path,TTRfile, sep = "/")
                   Rawloc <- paste(reactive_data$path,rawDataFile, sep = "/")
                   plotLoc <- paste(reactive_data$path, amplificationCurvesFile, sep = "/")
                  
                    #If converted cycles to minutes, change the header in the file
                   Signal_df_toSave <- reactive_data$signal_df
                   colnames(Signal_df_toSave)[which(names(Signal_df_toSave) == "Cycle")] <- "Time"
                  
                   write.csv(reactive_data$TTRdf, TTRloc, row.names = FALSE)
                   write.csv(Signal_df_toSave, Rawloc, row.names = FALSE)
                   ggsave(plotLoc, scale = 2, reactive_data$output_plot)
                   showNotification("Data saved successfully")
                 }, error = function(err) {
                   show_error_dialog("Unable to write data file. Make sure no files from selected
                                     directory are open")
                   return(NA) 
                 }) 
                 
                 
               })
  

  #Observe event for preference saving
  #Saves preferences to file called "preferences.csv" in the shiny installation folder
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$savePreferences
               },
               handlerExpr = {
                 preferenceData <- data.frame(defaultFolder = reactive_data$path,
                                              convert = input$convertToMins,
                                              conversionFactor = input$cycleConversionFactor,
                                              TTRoffset = input$TTRoffset,
                                              Method = input$TTRmethod,
                                              baselineStart = input$baselineStart,
                                              baselineEnd = input$baselineEnd,
                                              amplitudeThreshold = input$minDiff,
                                              numPlotColumns = input$numColumns,
                                              plotLimit = input$plotLimit,
                                              plotHeight = input$plotHeight)
                 
                 result <- tryCatch({
                   write.csv(preferenceData, preferenceFile, row.names = FALSE)
                   showNotification("Preferences saved successfully")
                 }, error = function(err) {
                   show_error_dialog("Unable to write preferences file. Make sure preferences.csv is not open
                                           and that working directory is set to shiny installation.")
                   return(NA) 
                 }) #END reading from label file
                 
                
               })
  
  
  
  #Observe event for the folder selection dialog
  #Upon successful selection of a folder, will scan its contents and extract relevant data
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$directory
               },
               handlerExpr = {
                 if (input$directory > 0) {
                   # launch the directory selection dialog with initial path read from the widget
                   tempPath = choose.dir(default = readDirectoryInput(session, 'directory'))
                   
                   
                   #Bug in file selection implementation in macs causes path to be a list where the first few elements are 
                   #error reports. Path is always the last element, so extract there
                   if (length(tempPath) > 1){
                     tempPath <- tempPath[length(tempPath)]
                   }
                   
                   #No file selected, or some strange problem
                   if (is.na(tempPath) | grepl('error', tempPath, ignore.case = TRUE)){
                     showNotification("No file selected")
                     return()
                   }
                   
                   #Path is probably good, store in reactive_data
                   reactive_data$path = tempPath
                   
                   # update the widget value
                   updateDirectoryInput(session, 'directory', value = reactive_data$path)
                  
            
                   #Search directory and extract relevant data from signal and label file
                   reactive_data$signal_df <-
                     annotate_data(
                       reactive_data$path,
                       input$convertToMins,
                       input$cycleConversionFactor,
                       input$TTRoffset
                     )
                   write.table(reactive_data$signal_df, file = "df3.csv", sep = ",")
                   if (all(is.na(reactive_data$signal_df))) {
                     return()
                   }
                   if (input$TTRmethod == 'Midpoint') {
                     reactive_data$TTRdf <- getTTR_midpoint(
                       reactive_data$signal_df,
                       input$baselineStart,
                       input$baselineEnd,
                       input$minDiff
                     )
                   } else if (input$TTRmethod == 'Regression') {
                     reactive_data$TTRdf <-
                       getTTR_logfit(reactive_data$signal_df, input$minDiff)
                     
                   }
                   
                   
                   #For each unique sample name, make a subplot with each amplification curve
                   reactive_data$output_plot <-
                     makeSubPlots(
                       reactive_data$signal_df,
                       reactive_data$TTRdf,
                       input$numColumns,
                       input$convertToMins,
                       input$plotLimit
                     )
                   output$Plots <-
                     renderPlot({
                       print(reactive_data$output_plot)
                     }, height = input$plotHeight)
                 }
               })
  #Observe event for re-analyze data. Repeats everything as in folder selection,
  #except folder selection
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$analyze
               },
               handlerExpr = {
                 reactive_data$signal_df <-
                   annotate_data(
                     reactive_data$path,
                     input$convertToMins,
                     input$cycleConversionFactor,
                     input$TTRoffset
                   )
                 if (all(is.na(reactive_data$signal_df))) {
                   return()
                 }
                 if (input$TTRmethod == 'Midpoint') {
                   reactive_data$TTRdf <- getTTR_midpoint(
                     reactive_data$signal_df,
                     input$baselineStart,
                     input$baselineEnd,
                     input$minDiff
                   )
                 } else if (input$TTRmethod == 'Regression') {
                   reactive_data$TTRdf <-
                     getTTR_logfit(reactive_data$signal_df, input$minDiff)
                   
                 }
                 reactive_data$output_plot <-
                   makeSubPlots(
                     reactive_data$signal_df,
                     reactive_data$TTRdf,
                     input$numColumns,
                     input$convertToMins,
                     input$plotLimit
                   )
                 output$Plots <- renderPlot({
                   print(reactive_data$output_plot)
                 }, height = input$plotHeight)
               })
                
})
