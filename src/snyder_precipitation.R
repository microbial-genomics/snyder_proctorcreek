library(ggplot2)
library(reshape2)
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
#png(filename="C:/git/snyder_proctorcreek/graphics/Precip_flow_bf.png",width=10, height=10, units="in",res=250)
P1<-ggplot(data = pf_data,aes(ym, precip_inch)) +labs(x = "", y = "Precipitation, total inches ")+
  stat_summary(fun.y = sum, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))

P2<-ggplot(data = pf_data,aes(mon, precip_inch)) +labs(x = "", y = "Precipitation, mean inches ")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))

P3<-ggplot(data = pf_data,
                                 aes(mon, flow_cfs)) +labs(x = "", y = "Flow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))
P4<-ggplot(data = pf_data,
                        aes(mon, baseflow_cfs)) +labs(x = "", y = "Baseflow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))




# Extract day of the week (Saturday = 6)
pf_data$Week_Day <- as.numeric(format(pf_data$Date, format='%w'))

# Adjust end-of-week date (first saturday from the original Date)
pf_data$End_of_Week <- pf_data$Date + (6 - pf_data$Week_Day)
pf_data$Week<- as.numeric(format(pf_data$Date, format="%U")) #get week number (week starts on Sunday)
pf_data$End_of_Biweek <- pf_data$End_of_Week + ((pf_data$Week %% 2 == 0) * 7) #(If week number is even, add 7 to end of week date)


colnames(pf_data)
# Aggregate over week and climate division
weekly_mean_precipi<-aggregate(precip_inch~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)
weekly_mean_flow<-aggregate(flow_cfs~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)
weekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Week, FUN=mean, data=pf_data, na.rm=TRUE)

# Aggregate over bi_week and climate division
biweekly_mean_precipi<-aggregate(precip_inch~End_of_Biweek, FUN=mean, data=pf_data, na.rm=TRUE)
biweekly_mean_flow<-aggregate(flow_cfs~End_of_Biweek, FUN=mean, data=pf_data, na.rm=TRUE)
biweekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Biweek, FUN=mean, data=pf_data, na.rm=TRUE)

##----------------------------------------------------------------------------------------------------------
#Weekly_statistics_raw data------------------------------------------------------------------------------
# add a 'Monthly' column with month breaks
weekly_mean_precipi$yyyym <- format(as.Date(weekly_mean_precipi$End_of_Week), "%Y-%m")
weekly_mean_precipi$Month <- format(as.Date(weekly_mean_precipi$End_of_Week), "%m")
weekly_mean_precipi$md <- format(as.Date(weekly_mean_precipi$End_of_Week), "%m-%d")

typeof(weekly_mean_precipi$Month)

weekly_mean_precipi$Month<- as.numeric(as.character(weekly_mean_precipi$Month))

dry<-weekly_mean_precipi %>%
  filter(weekly_mean_precipi$Month > 6, weekly_mean_precipi$Month< 12)
wet<-weekly_mean_precipi %>%
  filter(weekly_mean_precipi$Month== 12| weekly_mean_precipi$Month< 7)#weekly_total_precipi$Month== 11|

t.test(wet$precip_inch,dry$precip_inch)

boxplot(wet$precip_inch, at=1,xlim=c(0, 3))
boxplot(dry$precip_inch, at=2, add=TRUE)

#snyder def
dry_period <- subset(weekly_mean_precipi, md > "06-20"&md < "12-20")
wet_period <- subset(weekly_mean_precipi, md > "12-20"|md < "06-20")

# paired t-test
t.test(wet_period$precip_inch,dry_period$precip_inch)

boxplot(wet_period$precip_inch, at=1,xlim=c(0, 3))
boxplot(dry_period$precip_inch, at=2, add=TRUE)

#bi_week_statistics--------------------------------------------------------------------------------------------
#Weekly_statistics_raw data------------------------------------------------------------------------------
# add a 'Monthly' column with month breaks
biweekly_mean_precipi$yyyym <- format(as.Date(biweekly_mean_precipi$End_of_Biweek), "%Y-%m")
biweekly_mean_precipi$Month <- format(as.Date(biweekly_mean_precipi$End_of_Biweek), "%m")
biweekly_mean_precipi$md <- format(as.Date(biweekly_mean_precipi$End_of_Biweek), "%m-%d")

typeof(biweekly_mean_precipi$Month)

biweekly_mean_precipi$Month<- as.numeric(as.character(biweekly_mean_precipi$Month))

bi_dry<-biweekly_mean_precipi %>%
  filter(biweekly_mean_precipi$Month > 6, biweekly_mean_precipi$Month< 12)
bi_wet<-biweekly_mean_precipi %>%
  filter(biweekly_mean_precipi$Month== 12| biweekly_mean_precipi$Month< 7)#weekly_total_precipi$Month== 11|

t.test(bi_wet$precip_inch,bi_dry$precip_inch)

boxplot(bi_wet$precip_inch, at=1,xlim=c(0, 3))
boxplot(bi_dry$precip_inch, at=2, add=TRUE)

#snyder def
bi_dry_period <- subset(biweekly_mean_precipi, md > "06-20"&md < "12-20")
bi_wet_period <- subset(biweekly_mean_precipi, md > "12-20"|md < "06-20")

# paired t-test
t.test(bi_wet_period$precip_inch,bi_dry_period$precip_inch)

boxplot(bi_wet_period$precip_inch, at=1,xlim=c(0, 3))
boxplot(bi_dry_period$precip_inch, at=2, add=TRUE)
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#Snyder_study_period

# make sure dates are parsed properly
pf_data$Date <- as.Date(pf_data$Date, "%m/%d/%Y")
#subset study_period
snyder_period <- subset(pf_data, Date > as.Date("2015-10-31")&Date < as.Date("2017-08-01") )

P5<-ggplot(data = snyder_period ,aes(ym, precip_inch)) +labs(x = "", y = "Precipitation, total inches ")+
  stat_summary(fun.y = sum, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))

P6<-ggplot(data = snyder_period,aes(mon, precip_inch)) +labs(x = "", y = "Precipitation, mean inches ")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))

P7<-ggplot(data = snyder_period,
           aes(mon, flow_cfs)) +labs(x = "", y = "Flow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))
P8<-ggplot(data = snyder_period,
           aes(mon, baseflow_cfs)) +labs(x = "", y = "Baseflow, mean cfs")+
  stat_summary(fun.y = mean, geom = "bar")+ theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y = element_text(vjust=0.5, size=15,colour="black"))






#bi_weekly_summary_precipitation
# Aggregate over bi_week and climate division
sp_biweekly_mean_precipi<-aggregate(precip_inch~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_flow<-aggregate(flow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)


  # add a 'Monthly' column with month breaks
sp_biweekly_mean_precipi$yyyym <- format(as.Date(sp_biweekly_mean_precipi$End_of_Biweek), "%Y-%m")
sp_biweekly_mean_precipi$Month <- format(as.Date(sp_biweekly_mean_precipi$End_of_Biweek), "%m")
sp_biweekly_mean_precipi$md <- format(as.Date(sp_biweekly_mean_precipi$End_of_Biweek), "%m-%d")

#plot bi_weekly_precipitation
p9<-ggplot(data = sp_biweekly_mean_precipi,aes(End_of_Biweek, precip_inch)) +geom_bar(stat="identity")+
  labs(x = "", y = "Precipitation, biweekly mean inches ")+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90,vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))

sp_dry_period <- subset(sp_biweekly_mean_precipi, md > "06-20"&md < "12-20")
sp_wet_period <- subset(sp_biweekly_mean_precipi, md > "12-20"|md < "06-20")

# paired t-test
t.test(sp_wet_period$precip_inch,sp_dry_period$precip_inch)
boxplot(sp_wet_period$precip_inch, at=1,xlim=c(0, 3))
boxplot(sp_dry_period$precip_inch, at=2, add=TRUE)

# Combine two data.frame
df <- rbind(sp_dry_period, sp_wet_period )

# Create variable Group
df$Group <- rep(c("Dry", "Wet"), c(dim(sp_dry_period)[1], dim(sp_wet_period)[1]))
b1<-ggplot(df, aes(x=Group, y=precip_inch,fill=Group)) + geom_boxplot()+
  labs(x = "Period", y = "Precipitation, biweekly mean inches ")+
  theme_bw()+theme(legend.position="none")+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))


#-----------------------------------------------------------------------------------------------------------
#bi_weekly_summary_flow
colnames(snyder_period)
# make sure dates are parsed properly
pf_data$Date <- as.Date(pf_data$Date, "%m/%d/%Y")
#subset study_period
snyder_period <- subset(pf_data, Date > as.Date("2015-10-31")&Date < as.Date("2017-08-01") )

#bi_weekly_summary_flow
# Aggregate over bi_week and climate division
sp_biweekly_mean_precipi<-aggregate(precip_inch~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_flow<-aggregate(flow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)


# add a 'Monthly' column with month breaks
sp_biweekly_mean_flow$yyyym <- format(as.Date(sp_biweekly_mean_flow$End_of_Biweek), "%Y-%m")
sp_biweekly_mean_flow$Month <- format(as.Date(sp_biweekly_mean_flow$End_of_Biweek), "%m")
sp_biweekly_mean_flow$md <- format(as.Date(sp_biweekly_mean_flow$End_of_Biweek), "%m-%d")

#plot bi_weekly_precipitation
p10<-ggplot(data = sp_biweekly_mean_flow,aes(End_of_Biweek, flow_cfs)) +geom_bar(stat="identity")+
  labs(x = "", y = "Flow, biweekly mean cfs ")+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90,vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))

sp_dry_flow <- subset(sp_biweekly_mean_flow, md > "06-20"&md < "12-20")
sp_wet_flow <- subset(sp_biweekly_mean_flow, md > "12-20"|md < "06-20")

# paired t-test
t.test(sp_wet_flow$flow_cfs,sp_dry_flow$flow_cfs)
boxplot(sp_wet_flow$flow_cfs, at=1,xlim=c(0, 3))
boxplot(sp_dry_flow$flow_cfs, at=2, add=TRUE)

# Combine two data.frame
df_flow <- rbind(sp_dry_flow, sp_wet_flow )

# Create variable Group
df_flow$Group <- rep(c("Dry", "Wet"), c(dim(sp_dry_flow)[1], dim(sp_wet_flow)[1]))
b2<-ggplot(df_flow, aes(x=Group, y=flow_cfs,fill=Group)) + geom_boxplot()+
  labs(x = "Period", y = "Flow, mean cfs ")+
  theme_bw()+theme(legend.position="none")+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))

#-----------------------------------------------------------------------------------------------------------------
#bi_weekly_summary_baseflow
colnames(snyder_period)
# make sure dates are parsed properly
pf_data$Date <- as.Date(pf_data$Date, "%m/%d/%Y")
#subset study_period
snyder_period <- subset(pf_data, Date > as.Date("2015-10-31")&Date < as.Date("2017-08-01") )

#bi_weekly_summary_flow
# Aggregate over bi_week and climate division
sp_biweekly_mean_precipi<-aggregate(precip_inch~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_flow<-aggregate(flow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)
sp_biweekly_mean_bflow<-aggregate(baseflow_cfs~End_of_Biweek, FUN=mean, data=snyder_period, na.rm=TRUE)


# add a 'Monthly' column with month breaks
sp_biweekly_mean_bflow$yyyym <- format(as.Date(sp_biweekly_mean_bflow$End_of_Biweek), "%Y-%m")
sp_biweekly_mean_bflow$Month <- format(as.Date(sp_biweekly_mean_bflow$End_of_Biweek), "%m")
sp_biweekly_mean_bflow$md <- format(as.Date(sp_biweekly_mean_bflow$End_of_Biweek), "%m-%d")

#plot bi_weekly_precipitation
p11<-ggplot(data = sp_biweekly_mean_bflow,aes(End_of_Biweek, baseflow_cfs)) +geom_bar(stat="identity")+
  labs(x = "", y = "Baseflow, biweekly mean cfs ")+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90,vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))

sp_dry_bflow <- subset(sp_biweekly_mean_bflow, md > "06-20"&md < "12-20")
sp_wet_bflow <- subset(sp_biweekly_mean_bflow, md > "12-20"|md < "06-20")

# paired t-test
t.test(sp_dry_bflow$baseflow_cfs,sp_wet_bflow$baseflow_cfs)
boxplot(sp_dry_bflow$baseflow_cfs, at=1,xlim=c(0, 3))
boxplot(sp_wet_bflow$baseflow_cfs, at=2, add=TRUE)

# Combine two data.frame
df_bflow <- rbind(sp_dry_bflow, sp_wet_bflow )

# Create variable Group
df_bflow$Group <- rep(c("Dry", "Wet"), c(dim(sp_dry_bflow)[1], dim(sp_wet_bflow)[1]))
b3<-ggplot(df_bflow, aes(x=Group, y=baseflow_cfs,fill=Group)) + geom_boxplot()+
  labs(x = "Period", y = "Baseflow, mean cfs ")+
  theme_bw()+ theme(legend.position="none")+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))


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

#raw data plots
png(filename="C:/git/snyder_proctorcreek/graphics/Precip_flow_bf.png",width=10, height=12, units="in",res=250)
multiplot(P2, P3, P4, cols=1)
dev.off()

#snyder_periodNovember 2015 - July 2017 plots

png(filename="C:/git/snyder_proctorcreek/graphics/SnyderPeriod_Precip_flow_bf.png",width=10, height=12, units="in",res=250)
multiplot(P5, P6, P7,P8, cols=1)
dev.off()

#bi_weekly Plots
png(filename="C:/git/snyder_proctorcreek/graphics/sp_biweekly_Precip_flow_bf.png",width=10, height=12, units="in",res=250)
multiplot(p9, p10, p11, cols=1)
dev.off()

#bi_weekly dry and wet period box Plots
png(filename="C:/git/snyder_proctorcreek/graphics/sp_biweekly_boxplots.png",width=10, height=6, units="in",res=250)
multiplot(b1, b2, b3, cols=3)
dev.off()