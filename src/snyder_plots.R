##install

library(ggplot2)
require(gridExtra)
library(reshape2)
library(scales)

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



pc_data_file <- paste(pcdir_data_in,"proctor_creek_feb20.csv", sep="")
file.exists(pc_data_file)
pc_data <- read.table(pc_data_file, header = TRUE, sep = ",")
View(pc_data)
dim(pc_data)
summary(pc_data)
colnames(pc_data)
pc_data$Site_f = factor(pc_data$Site, levels=c('Burbank','North Ave','Hortense',"Kerry Circle","James Jackson", "Northwest","Greensferry", "North Ave CSO","Lindsay Street","Grove Park","Spring Street","Lillian Cooper Park"))

#Log E.Coli plot
p1<-ggplot(pc_data,aes(x=Site_f,y=Log10.E..coli.MPN.100.ml))+geom_boxplot(aes(fill=Wet.Dry.Season))+ facet_grid(.~Mainstem_Tributary, scales = 'free')+
  labs(x = "", y = "Log E.Coli (MPN/100 mL)",color="Season")+
  scale_y_continuous(limits = c(0, 5))+
  #scale_fill_brewer(palette="BrBG")+
  theme_bw()+
  theme(strip.background=element_rect(fill="grey"))+
  theme(strip.text=element_text(color="black", face="bold",size=15))+ 
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
  #theme(legend.position =c(0.2, 0.2), legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.position ="bottom", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_blank())+ geom_hline(yintercept=2.1,linetype="dashed", color = "red",size=1)+
  scale_linetype_manual(name="Title of legend", values = c(1,2), guide = guide_legend(override.aes = list(color = c("blue", "red"))))


#Log HF183MGB plot
p2<-ggplot(pc_data,aes(x=Site_f,y=Log.HF183MGB..GC.100.mL.))+geom_boxplot(aes(fill=Wet.Dry.Season))+ facet_grid(.~Mainstem_Tributary, scales = 'free')+
  labs(x = "", y = "Log HF183MGB (GC/100 mL)",color="Season")+
  scale_y_continuous(limits = c(0, 8), breaks =c(0,2, 4, 6,8))+
  #scale_fill_brewer(palette="BrBG")+
  theme_bw()+
  theme(strip.background=element_rect(fill="grey"))+
  theme(strip.text=element_text(color="black", face="bold",size=15))+ 
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
  #theme(legend.position =c(0.2, 0.2), legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.position ="bottom", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_blank())#+ #geom_hline(yintercept=8.3,linetype="dashed", color = "red",size=1)
  #scale_linetype_manual(name="Title of legend", values = c(1,2), guide = guide_legend(override.aes = list(color = c("blue", "red"))))

#Log.DG3 plot
p3<-ggplot(pc_data,aes(x=Site_f,y=Log.DG3..GC.100.mL.))+geom_boxplot(aes(fill=Wet.Dry.Season))+ facet_grid(.~Mainstem_Tributary, scales = 'free')+
  labs(x = "", y = "Log DG3 (GC/100 mL)",color="Season")+
  scale_y_continuous(limits = c(0, 6))+
  #scale_fill_brewer(palette="BrBG")+
  theme_bw()+
  theme(strip.background=element_rect(fill="grey"))+
  theme(strip.text=element_text(color="black", face="bold",size=15))+ 
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
  theme(legend.position =c(0.8, 0.89), legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  ##theme(legend.position ="bottom", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_blank())+ #geom_hline(yintercept=8.3,linetype="dashed", color = "red",size=1)+
  scale_linetype_manual(name="Title of legend", values = c(1,2), guide = guide_legend(override.aes = list(color = c("blue", "red"))))

#Salmonella frequency with calculated frequency

salmon_file <- paste(pcdir_data_in,"salmon.csv", sep="")
file.exists(salmon_file)
salmon <- read.table(salmon_file, header = TRUE, sep = ",")
#salmon<- read.csv("C:/git/snyder_proctorcreek/data_in/salmon.csv", header=TRUE,sep=",")
colnames(salmon)
salmon$Site_f = factor(salmon$Site, levels=c('Burbank','North Ave','Hortense',"Kerry Circle","James Jackson", "Northwest","Greensferry", "North Ave CSO","Lindsay St","Grove Park","Spring St","Lillian Cooper"))
p4<-ggplot(data=salmon, aes(x=Site_f, y=Frequency, fill=factor(Season))) +geom_bar(stat="identity",position="dodge")+
  facet_grid(.~Stream_type, scales = 'free')+geom_point(aes(x =Site_f, y = Lower.95.,fill=factor(Season)))

#Salmonella frequency with raw data  
sal_file <- paste(pcdir_data_in,"proctor_creek_feb20_sal.csv", sep="")
file.exists(sal_file)
sal <- read.table(sal_file, header = TRUE, sep = ",")
#sal<- read.csv("C:/git/snyder_proctorcreek/data_in/proctor_creek_feb20_sal.csv", header=TRUE,sep=",")
colnames(sal)
sal$Site_f = factor(sal$Site, levels=c('Burbank','North Ave','Hortense',"Kerry Circle","James Jackson", "Northwest","Greensferry", "North Ave CSO","Lindsay Street","Grove Park","Spring Street","Lillian Cooper Park"))
p4<-ggplot(data=sal, aes(x=Site_f,fill=factor(Salmonella..Presence.Absence.))) +geom_bar() + 
     facet_grid(Wet.Dry.Season~Mainstem_Tributary, scales = 'free')+
  labs(x = "", y = "Count")+scale_y_continuous(limits = c(0, 30))+
  theme_bw()+
  theme(strip.background=element_rect(fill="grey"))+
  theme(strip.text=element_text(color="black", face="bold",size=15))+ 
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
  theme(legend.position =c(0.2, 0.89), legend.direction = "horizontal")+ 
  #theme(legend.position ="bottom", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_blank())+ #geom_hline(yintercept=8.3,linetype="dashed", color = "red",size=1)+
  scale_linetype_manual(name="Title of legend", values = c(1,2), guide = guide_legend(override.aes = list(color = c("blue", "red"))))

 

p5<-ggplot(data=sal, aes(x=Site_f,fill=factor(Stx2..Presence.Absence.))) +geom_bar() + 
  facet_grid(Wet.Dry.Season~Mainstem_Tributary, scales = 'free')+
  labs(x = "", y = "Count")+scale_y_continuous(limits = c(0, 30))+
  theme_bw()+
  theme(strip.background=element_rect(fill="grey"))+
  theme(strip.text=element_text(color="black", face="bold",size=15))+ 
  theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
  theme(legend.position =c(0.2, 0.89), legend.direction = "horizontal")+ 
  #theme(legend.position ="bottom", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_blank())+ #geom_hline(yintercept=8.3,linetype="dashed", color = "red",size=1)+
  scale_linetype_manual(name="Title of legend", values = c(1,2), guide = guide_legend(override.aes = list(color = c("blue", "red"))))



###with error bar
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

install.packages("ggpubr")
library(ggpubr)
pc_data$Site_f = factor(pc_data$Site, levels=c('Burbank','North Ave','Hortense',"Kerry Circle","James Jackson", "Northwest","Greensferry", "North Ave CSO","Lindsay St","Grove Park","Spring Street","Lillian Cooper"))
p<-ggerrorplot(pc_data, x = "Site_f", y = "Log.HF183MGB..GC.100.mL.", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            color = "Wet.Dry.Season",palette="Set1", 
            add = "mean"                 # Add mean points
)+labs(x = "", y = "Log HF183MGB (GC/100 mL)",color="Season")+
  scale_y_continuous(limits = c(0, 9))
facet(p+theme_bw(), facet.by = "Mainstem_Tributary")

facet(p+theme_bw()+
        theme(strip.background=element_rect(fill="grey"))+
        theme(strip.text=element_text(color="black", face="bold",size=15))+ 
        theme(axis.title.x = element_text(face="bold", colour="black", size=15),axis.text.x  = element_text(angle=90, vjust=0.5, size=15,colour="black"))+ 
        theme(axis.title.y = element_text(face="bold", colour="black", size=15),axis.text.y  = element_text(vjust=0.5, size=15,colour="black"))+
        theme(legend.position ="top", legend.background = element_rect(color = "black",fill = "white", size = 1, linetype = "solid"), legend.direction = "horizontal")+ 
        theme(legend.key.size = unit(0.5, "in"))+
        theme(legend.text=element_text(size=15))+
        theme(legend.title=element_blank())+ geom_hline(yintercept=8.5,linetype="dashed", color = "red",size=1), facet.by = "Mainstem_Tributary", scales = 'free')

