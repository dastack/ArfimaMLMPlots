library(ArfimaMLM)
load("full.house.replication.Rdata")

Mod1<- arfimaMLM(majpu.ydif~ minpu.xdif + policyvote + longsession + session1 +
            minpu.fd + majn1s.fd + majn2s.fd+medmedn1.fd +
            medmedn2.fd + majsize.fd + (1 + minpu.xdif|year),
          data=full.house, timevar="year")



ArfimaMLM.plot<- function(Model, CoefName, TimeVar, 
                          loess = NULL, title = NULL,
                          xaxis = NULL, yaxis = NULL) {
  #  Mod<- deparse(substitute(Model))
  Model1<- Model$result
  require(lme4)
  ran.coef<- coef(Model1)
  ran.coef2<- data.frame(ran.coef[[1]])
  rm(ran.coef)
  slope<-  ran.coef2[,CoefName]
  Date<- as.character(row.names(ran.coef2))
  Date<- as.numeric(Date)
  plot.frame<- data.frame(Date, slope)
  # plot.frame<- ran.coef2 %>%
  #   select_(Date, Coefname)
  require(ggplot2)
  ArfimaPlot<- ggplot(plot.frame, 
                      aes(x = Date, y = slope)) + 
    geom_line(aes())
  if (missing(title)) {
    ArfimaPlot<- ArfimaPlot
  } else ArfimaPlot<- ArfimaPlot + 
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  if (missing(loess)){
    ArfimaPlot<- ArfimaPlot
  } else if (loess == TRUE) {
#    ArfimaPlot<- ArfimaPlot
    ArfimaPlot<- ArfimaPlot + geom_smooth(aes(),
                method = "loess")
  } else if (loess == FALSE) {
    ArfimaPlot<- ArfimaPlot
  }
  if (missing(xaxis)) {
    ArfimaPlot<- ArfimaPlot
  } else ArfimaPlot<- ArfimaPlot + 
      xlab(xaxis)
  if (missing(yaxis)) {
    ArfimaPlot <- ArfimaPlot
  } else ArfimaPlot <- ArfimaPlot +
    ylab(yaxis)
  
  return(ArfimaPlot)
}


testfun<- ArfimaMLM.plot(Mod1, "minpu.xdif", year, loess = TRUE,
                         title = "Party Unity through Time",
                         xaxis = "Year", 
                         yaxis = "Slope on Unity")

print(testfun)
