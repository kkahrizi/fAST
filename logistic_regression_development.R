library(nlme)
library(testit)
library(ggplot2)
df <-
  data.frame(read.table('matt_data.csv', sep = ',', header = TRUE))
amplitude_threshold = 800
for (well in unique(df$Well))
{
  # if (well != "H9") {
  #   next
  # }
  print(well)
  well_data <- subset(df, Well == well)
  
  
  baseline <- mean(well_data$RFU[1:6])
  peak <- max(well_data$RFU)
  midpeak <- (baseline + peak) / 2
  if(midpeak - baseline < amplitude_threshold){
    midpoint <- NA
    p1 <- ggplot(data = well_data, aes(x = Cycle, y = RFU)) +
      geom_line() + 
      labs(title = paste(well,'Negative', sep = ';'))
  } else {
    midpoint = well_data$Cycle[min(which(well_data$RFU > midpeak))]
    peakpoint = well_data$Cycle[which(well_data$RFU == 
                                        max(well_data$RFU[well_data$Cycle < midpoint + 20]))]
    fit_data <- subset( well_data, Cycle > midpoint - 10 & Cycle <= peakpoint )
    fitbaseline <- min(fit_data$RFU)
    fit_data$RFU <- fit_data$RFU - fitbaseline + 1
    
    if (!(has_error(fitted <- nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal),
                                 data = fit_data)))){
      
    
    fitted <- nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal),
          data = fit_data)
    
    y_h <-
      SSlogis(
        fit_data$Cycle,
        summary(fitted)$coefficients[1],
        summary(fitted)$coefficients[2],
        summary(fitted)$coefficients[3]
      )
    
    well_data_fit <- fit_data
    well_data_fit$RFU <- y_h + fitbaseline - 1
    xmid <- summary(fitted)$coefficients[2]
    MidRFU <-
      SSlogis(
        xmid,
        summary(fitted)$coefficients[1],
        summary(fitted)$coefficients[2],
        summary(fitted)$coefficients[3]
      )
    
    TTRdf <- data.frame(Cycle = xmid, RFU = MidRFU + fitbaseline - 1)
    p1 <- ggplot(data = well_data, aes(x = Cycle, y = RFU)) +
      geom_line(aes(color = 'data')) +
      geom_line(data = well_data_fit, aes(color = 'fit')) +
      geom_point(data = TTRdf, aes(color = 'TTR'), na.rm=TRUE) +
      labs(title = well)
    } else {
        print(paste("Couldn't fit log for well",well,sep = ':'))
    }
    
  }
  
  
  print(p1)
  
 
  # # if ((well_data$RFU[max(which(well_data$Cycle < n))] - well_data$RFU[1])/n > minThreshold &
  # #     maxThreshold > (well_data$RFU[max(which(well_data$Cycle < n))] - well_data$RFU[1])/n ){
  # #   well_data <- subset(well_data, well_data$Cycle > n)
  # #   break
  # # }
  # 
  # # print(well)
  # baseline <- mean(well_data$RFU[1:5])
  # well_data$RFU = well_data$RFU - baseline
  # well_data$RFU[well_data$RFU < 1] = 1
  # well_data_normal <- well_data
  # well_data_normal$RFU <-
  #   well_data_normal$RFU / max(well_data_normal$RFU)
  # 
  # #If data starts decreasing near the end of a reaction, interferes with logistic fit. To fix,
  # #dropped successively more points until fit works
  # # well_data_normal <- subset(well_data_normal, Cycle < 36)
  # try({
  #   dropped = 0
  #   while (has_error(nls(
  #     RFU ~ SSlogis(Cycle, Asym, xmid, scal),
  #     data = well_data_normal,
  #     trace = TRUE
  #   )) &
  #   dropped < length(well_data_normal$C)) {
  #     well_data_normal <-
  #       subset(well_data_normal,
  #              Cycle < max(well_data_normal$Cycle) - dropped)
  #     dropped = dropped + 1
  #     
  #   }
  # }, silent = TRUE)
  # print(paste(c('Dropped',dropped,'points'),sep=' '))
  # 
  # 
  # 
  # 
  # fitted <-
  #   nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal),
  #       data = well_data_normal,
  #       trace = TRUE)
  # 
  # y_h <-
  #   SSlogis(
  #     well_data_normal$Cycle,
  #     summary(fitted)$coefficients[1],
  #     summary(fitted)$coefficients[2],
  #     summary(fitted)$coefficients[3]
  #   )
  # 
  # well_data_fit <- well_data_normal
  # well_data_fit$RFU <- y_h
  # xmid <- summary(fitted)$coefficients[2]
  # MidRFU <-
  #   SSlogis(
  #     xmid,
  #     summary(fitted)$coefficients[1],
  #     summary(fitted)$coefficients[2],
  #     summary(fitted)$coefficients[3]
  #   )
  # 
  # print("Mid RFU")
  # print(MidRFU*max(well_data$RFU))
  # 
  # if (MidRFU*max(well_data$RFU) - min(well_data$RFU) < amplitude_threshold) {
  #   print('Drop sum points')
  #   #For n from 20 to 5, if average of first n cycles increases by over 20 RFU/cycle and less than
  #   #400 RFU/cycle, dump those first n cycles
  #   well_data$ReadNum <- 1:length(well_data$Cycle)
  #   minThreshold = 20
  #   maxThreshold = 100
  #   for (n in 25:5) {
  #     lfit <- lm(RFU ~ ReadNum, well_data, subset = well_data$ReadNum < n)
  #     print(n)
  #     print(coefficients(lfit)[2])
  #     if (!is.na(coefficients(lfit)[2]) &
  #         coefficients(lfit)[2] > minThreshold &
  #         coefficients(lfit)[2] < maxThreshold) {
  #       print(paste("Dropped",n,"of the first points.",sep=' '))
  #       well_data <- subset(well_data, well_data$ReadNum > n)
  #       break
  #     }
  #     # if ((well_data$RFU[max(which(well_data$Cycle < n))] - well_data$RFU[1])/n > minThreshold &
  #     #     maxThreshold > (well_data$RFU[max(which(well_data$Cycle < n))] - well_data$RFU[1])/n ){
  #     #   well_data <- subset(well_data, well_data$Cycle > n)
  #     #   break
  #     # }
  #   }
  # }
  # well_data$RFU = well_data$RFU - min(well_data$RFU) + 1
  # well_data_normal <- well_data
  # well_data_normal$RFU <-
  #   well_data_normal$RFU / max(well_data_normal$RFU)
  # 
  # #If data starts decreasing near the end of a reaction, interferes with logistic fit. To fix,
  # #dropped successively more points until fit works
  # # well_data_normal <- subset(well_data_normal, Cycle < 36)
  # try({
  #   dropped = 0
  #   while (has_error(nls(
  #     RFU ~ SSlogis(Cycle, Asym, xmid, scal),
  #     data = well_data_normal,
  #     trace = TRUE
  #   )) &
  #   dropped < length(well_data_normal$Cycle)) {
  #     well_data_normal <-
  #       subset(well_data_normal,
  #              Cycle < max(well_data_normal$Cycle) - dropped)
  #     dropped = dropped + 1
  #     
  #   }
  # }, silent = TRUE)
  # 
  # 
  # 
  # 
  # fitted <-
  #   nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal),
  #       data = well_data_normal,
  #       trace = TRUE)
  # 
  # y_h <-
  #   SSlogis(
  #     well_data_normal$Cycle,
  #     summary(fitted)$coefficients[1],
  #     summary(fitted)$coefficients[2],
  #     summary(fitted)$coefficients[3]
  #   )
  # 
  # well_data_fit <- well_data_normal
  # well_data_fit$RFU <- y_h
  # xmid <- summary(fitted)$coefficients[2]
  # MidRFU <-
  #   SSlogis(
  #     xmid,
  #     summary(fitted)$coefficients[1],
  #     summary(fitted)$coefficients[2],
  #     summary(fitted)$coefficients[3]
  #   )
  # 
  # #If it still doesn't pass amplitude threshold, we call it negative (it might have started amplifying)
  # if (MidRFU*max(well_data$RFU) - min(well_data$RFU) < amplitude_threshold) {
  #   xmid<- max(well_data$Cycle)
  #   MidRFU <- max(well_data_normal$RFU)
  # }
  # TTRdf <- data.frame(Cycle = xmid, RFU = MidRFU)
  # print(TTRdf)
  # p1 <- ggplot(data = well_data, aes(x = Cycle, y = RFU)) +
  #   geom_line(data = well_data, aes(color = 'data')) +
  #   geom_line(data = well_data_fit, aes(y = RFU * max(well_data$RFU), color =
  #                                         'fit')) +
  #   geom_point(data = TTRdf, aes(y = RFU * max(well_data$RFU), color = 'TTR'),
  #              na.rm=TRUE) +
  #   labs(title = well)
  # 
  # print(p1)
}
