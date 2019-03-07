library(ggplot2)
#library(reshape2)
#library(scales)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
# tom mac
if(Sys.info()[4] == "Toms-Air"){
  pcdir <- path.expand("~/git/snyder_proctorcreek/")
}
#Sumathy's window
if(Sys.info()[4]=="DZ2626USSINNATH"){
  pcdir  <- path.expand("c:/git/snyder_proctorcreek/")
}
#Sumathy's desktop
if(Sys.info()[4]=="DESKTOP-7UFGA86"){
  pcdir  <- path.expand("C:/Users/Sumathy/snyder_proctorcreek/")
}

pcdir_data_in <- paste(pcdir,'data_in/',sep='')
pcdir_data_out <- paste(pcdir,'data_out/',sep='')
pcdir_graphics <- paste(pcdir,'graphics/',sep='')
pcdir_src <- paste(pcdir,'src/',sep='')



pf_file <- paste(pcdir_data_in,"usgs2336526.csv", sep="")
file.exists(pf_file)
pf_data <- read.table(pf_file, header = TRUE, sep = ",")
View(pf_data)
dim(pf_data)
summary(pf_data)
colnames(pf_data)



# make sure dates are parsed properly
pf_data$Date <- as.Date(pf_data$Date, "%m/%d/%Y")
# add a 'Monthly' column with month breaks
pf_data$ym <- format(as.Date(pf_data$Date), "%Y-%m")
pf_data$mon <- format(as.Date(pf_data$Date), "%m")

# plots raw data(2002-2019)----------------------------------------------------------------------------------
png(filename="C:/git/snyder_proctorcreek/graphics/Precip_flow_bf.png",width=10, height=10, units="in",res=250)
P1<-ggplot(data = pf_data,aes(ym, precip_inch)) +labs(x = "", y = "Precipitation, total inches ")+
  stat_summary(fun.y = sum, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))

P2<-ggplot(data = pf_data,
                 aes(mon, precip_inch)) +labs(x = "", y = "Precipitation, mean inches ")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))

P3<-ggplot(data = pf_data,
                                 aes(mon, flow_cfs)) +labs(x = "", y = "Flow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))

P4<-ggplot(data = pf_data,
                        aes(mon, baseflow_cfs)) +labs(x = "", y = "Baseflow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(P2, P3, P4, cols=1)
dev.off()



# Extract day of the week (Saturday = 6)
pf_data$Week_Day <- as.numeric(format(pf_data$Date, format='%w'))

# Adjust end-of-week date (first saturday from the original Date)
pf_data$End_of_Week <- pf_data$Date + (6 - pf_data$Week_Day)

colnames(pf_data)
# Aggregate over week and climate division
weekly_mean_precipi<-aggregate(precip_inch~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)
weekly_mean_flow<-aggregate(flow_cfs~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)
weekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)

# add a 'Monthly' column with month breaks
weekly_mean_precipi$yyyym <- format(as.Date(weekly_mean_precipi$End_of_Week), "%Y-%m")
weekly_mean_precipi$Month <- format(as.Date(weekly_mean_precipi$End_of_Week), "%m")
typeof(weekly_mean_precipi$Month)
weekly_mean_precipi$Month<- as.numeric(as.character(weekly_mean_precipi$Month))
dry<-weekly_mean_precipi %>%
  filter(weekly_mean_precipi$Month > 6, weekly_mean_precipi$Month< 12)
wet<-weekly_mean_precipi %>%
  filter(weekly_mean_precipi$Month== 12| weekly_mean_precipi$Month< 7)#weekly_total_precipi$Month== 11|
# paired t-test
t.test(wet$precip_inch,dry$precip_inch)

boxplot(wet$precip_inch, at=1,xlim=c(0, 3))
boxplot(dry$precip_inch, at=2, add=TRUE)


#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
# make sure dates are parsed properly
pf_data$Date <- as.Date(pf_data$Date, "%m/%d/%Y")
#subset study_period
snyder_period <- subset(pf_data, Date > as.Date("2015-09-01")&Date < as.Date("2017-07-25") )

#bi_weekly_summary_precipitation
bi_weekly_summary <- snyder_period %>% 
  mutate(TwoWeeks = round_date(snyder_period$Date, "2 weeks")) %>%
  group_by(TwoWeeks) %>%
  summarise(mean_value = mean(precip_inch))
bi_weekly_summary
colnames(bi_weekly_summary)

#plot bi_weekly_precipitation
ggplot(data = bi_weekly_summary,aes(TwoWeeks, mean_value)) +geom_line()+
  labs(x = "", y = "Precipitation, total inches ")

# add a 'Monthly' column with month breaks
bi_weekly_summary$yyyym <- format(as.Date(bi_weekly_summary$TwoWeeks), "%Y-%m")
bi_weekly_summary$Month <- format(as.Date(bi_weekly_summary$TwoWeeks), "%m")
bi_weekly_summary$md <- format(as.Date(bi_weekly_summary$TwoWeeks), "%m-%d")

dry_period <- subset(bi_weekly_summary, md > "06-20"&md < "12-20")
wet_period <- subset(bi_weekly_summary, md > "12-20"|md < "06-20")

# paired t-test
t.test(wet_period$mean_value,dry_period$mean_value)
boxplot(wet_period$mean_value, at=1,xlim=c(0, 3))
boxplot(dry_period$mean_value, at=2, add=TRUE)
#-----------------------------------------------------------------------------------------------------------
#bi_weekly_summary_flow
colnames(snyder_period)
bi_weekly_flow <- snyder_period %>% 
  mutate(TwoWeeks = round_date(snyder_period$Date, "2 weeks")) %>%
  group_by(TwoWeeks) %>%
  summarise(mean_flow = mean(flow_cfs))
bi_weekly_flow
colnames(bi_weekly_flow)

#plot bi_weekly_precipitation
ggplot(data = bi_weekly_flow,aes(TwoWeeks, mean_flow)) +geom_line()+
  labs(x = "", y = "Flow, mean cfs ")

# add a 'Monthly' column with month breaks
bi_weekly_flow$yyyym <- format(as.Date(bi_weekly_flow$TwoWeeks), "%Y-%m")
bi_weekly_flow$Month <- format(as.Date(bi_weekly_flow$TwoWeeks), "%m")
bi_weekly_flow$md <- format(as.Date(bi_weekly_flow$TwoWeeks), "%m-%d")

dry_flow <- subset(bi_weekly_flow, md > "06-20"&md < "12-20")
wet_flow <- subset(bi_weekly_flow, md > "12-20"|md < "06-20")

# paired t-test
t.test(wet_flow$mean_flow,dry_flow$mean_flow)
boxplot(wet_flow$mean_flow, at=1,xlim=c(0, 3))
boxplot(dry_flow$mean_flow, at=2, add=TRUE)

#-----------------------------------------------------------------------------------------------------------------
#bi_weekly_summary_baseflow
colnames(snyder_period)
bi_weekly_bf <- snyder_period %>% 
  mutate(TwoWeeks = round_date(snyder_period$Date, "2 weeks")) %>%
  group_by(TwoWeeks) %>%
  summarise(mean_bf = mean(baseflow_cfs))
bi_weekly_bf
colnames(bi_weekly_bf)

#plot bi_weekly_precipitation
ggplot(data = bi_weekly_bf,aes(TwoWeeks, mean_bf)) +geom_line()+
  labs(x = "", y = "Baseflow, mean cfs ")

# add a 'Monthly' column with month breaks
bi_weekly_bf$yyyym <- format(as.Date(bi_weekly_bf$TwoWeeks), "%Y-%m")
bi_weekly_bf$Month <- format(as.Date(bi_weekly_bf$TwoWeeks), "%m")
bi_weekly_bf$md <- format(as.Date(bi_weekly_bf$TwoWeeks), "%m-%d")

dry_bf <- subset(bi_weekly_bf, md > "06-20"&md < "12-20")
wet_bf <- subset(bi_weekly_bf, md > "12-20"|md < "06-20")

# paired t-test
t.test(wet_bf$mean_bf,dry_bf$mean_bf)
boxplot(wet_bf$mean_bf, at=1,xlim=c(0, 3))
boxplot(dry_bf$mean_bf, at=2, add=TRUE)