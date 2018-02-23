df <- data.frame(read.table('df3.csv',sep=',', header=TRUE, row.names = 1))
for (well in unique(df$Well))
{
  fluB_rep1 <- subset(df, Well == well)
  print(well)
  fluB_rep1$RFU = fluB_rep1$RFU - min(fluB_rep1$RFU) + 1
  fluB_rep1_normal <- fluB_rep1
  fluB_rep1_normal$RFU <- fluB_rep1_normal$RFU / max(fluB_rep1_normal$RFU)
  
  fitted <- nls(RFU ~ SSlogis(Cycle, Asym, xmid, scal), data = fluB_rep1_normal)
  
  y_h <- SSlogis(fluB_rep1_normal$Cycle, summary(fitted)$coefficients[1], summary(fitted)$coefficients[2], 
                 summary(fitted)$coefficients[3])
  
  fluB_rep1_fit <- fluB_rep1
  fluB_rep1_fit$RFU <- y_h
  xmid <- summary(fitted)$coefficients[2]
  print(xmid)
  TTR <- data.frame(Cycle = xmid, RFU = SSlogis(xmid, summary(fitted)$coefficients[1], summary(fitted)$coefficients[2], 
                                                summary(fitted)$coefficients[3]) )
  
  p1 <- ggplot(data = fluB_rep1, aes(x = Cycle, y = RFU)) +
    geom_line(data = fluB_rep1, aes(color = 'data')) + 
    geom_line(data = fluB_rep1_fit, aes(y = RFU *max(fluB_rep1$RFU), color='fit')) + 
    geom_point(data = TTR, aes(y = RFU * max(fluB_rep1$RFU), color = 'TTR')) +
    geom_line(data = fluB_rep1_normal, aes(y = RFU * max(fluB_rep1$RFU), color = 'Normal')) + 
    scale_y_continuous(sec.axis = ~./max(fluB_rep1$RFU), name = 'Normalized data')
  
  print(p1)
}
