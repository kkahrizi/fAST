#Kamin Kahrizi, DiAssess 2018
library(shiny)
library(ggplot2)
library(gtable)
library(tidyr)
library(gridExtra)
#Variables to find appropriate files 
signalFileToken <- "Quantification Amplification Results_SYBR.csv"
labelFileToken <- "Quantification Summary_0.csv"
numPlotColumns <- 4

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
annotate_data <- function(path){
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

#Function to make a separate subplot for each unique column name in df
makeSubPlots <- function(df){
        plots <- list()
        index = 1;
        for (cond in unique(df$Sample)){
            local({
               print(cond)
               toPlot <- df[df$Sample == cond, ]
               plot <- ggplot(toPlot, aes( x = Cycle, y = RFU, col = Well )) +
                    geom_line() +
                    xlab("Cycle") +
                    ylab("RFU") +
                    ggtitle(cond)
               plots[[index]] <<- plot
               })
            index = index + 1
        } 
        grobz <- lapply(plots, ggplotGrob)
        format <- matryix(unlist(1:(index-1)), ncol = numPlotColumns, byrow = TRUE)

        gt <- arrangeGrob(grobs = grobz, layout_matrix = format)

        return(gt)
}

#Function to get TTR by midpoint method (TTR = midpoint of peak and baseline value)
getTTR <- function(df, baselineStart, baselineEnd, minDiff){
    TTR_df <- data.frame(Well = NA, Sample = NA, TTR = NA)
    TTRindex = 1 
    for (well in unique(df$Well)) {
        TTR_df$Well[TTRindex]  = well
        TTR_df$Sample[TTRindex] = df$Sample[df$Well == well]
        well_subset <- df[df$Well == well,]
        baselineValue <- mean(well_subset$RFU[ well_subset$Cycle > baselineStart & well_subset$Cycle < baselineEnd ])
        peakValue <- max(well_subset$RFU)
        if( peakValue - baselineValue < minDiff ) {
            TTR_df$TTR[TTRindex] = NA
        } else {
            midpoint = (baselineValue + peakValue ) / 2
            TTR_df$TTR[TTRindex] = df$Cycle[min(which(df$RFU > midpoint))] 
        }
        TTRindex = TTRindex + 1
    }
}

shinyServer(function(input, output, session) {
                observeEvent(
                               ignoreNULL = TRUE,
                               eventExpr = {
                                       input$directory
                               },
                               handlerExpr = {
                                   if (input$directory > 0) {
                                             # condition prevents handler execution on initial app launch
                                             
                                             # launch the directory selection dialog with initial path read from the widget
                                             path = choose.dir(default = readDirectoryInput(session, 'directory'))
                                             # update the widget value
                                             updateDirectoryInput(session, 'directory', value = path)
                                             
                                             print(path)
                                             #Search directory and extract relevant data from signal and label file    
                                             signal_df <- annotate_data(path)
                                             if (is.na(signal_df)){
                                                 return()
                                             } 
                                            # write.csv(signal_df, paste(path,"df3.csv", sep="/"))
                                             
                                             #For each unique sample name, make a subplot with each amplification curve
                                             output$Plots <- renderPlot({
                                                 gt <- makeSubPlots(signal_df)
                                                 grid.draw(gt)
                                               
                                             })
                                   }
                               }

                             )
})
