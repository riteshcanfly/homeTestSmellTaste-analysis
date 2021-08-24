# library
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

#Loading dataset

dft<-read.csv('../data/covid19_online_survey_data.csv')

#*********************************Figure 4(b) and (c)**********************************************

dfct<-read.csv('../data/contacts_new.csv')
g3<-ggplot(data = dfct, 
   mapping = aes(
      x = InContact, 
      y = ifelse(test = Diagnosed == "COVID-19", yes = -Percentage, no = Percentage), 
      fill = Diagnosed,
       label=paste(round(Percentage, 2))
   )) +
geom_bar(stat = "identity") +
geom_text(hjust=ifelse(test = dfct$Diagnosed == "COVID-19",  yes = 1.1, no = -0.1), size=6, fontface=2,colour="black") +
scale_y_continuous(labels = abs, limits = max(dfct$Percentage) * c(-1,1) * 1.1) +  
coord_flip() +

  scale_fill_manual(values=c("maroon","skyblue"))+

  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold'),
        #legend.position = "none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(y=expression(bold("Percentage")),x=expression(bold("In-Contact")))


dfct<-read.csv('../data/smoker.csv')
g4<-ggplot(data = dfct, 
   mapping = aes(
      x = Smoker, 
      y = ifelse(test = Diagnosed == "COVID-19", yes = -Percentage, no = Percentage), 
      fill = Diagnosed,
       label=paste(round(Percentage, 2))
   )) +
geom_bar(stat = "identity") +
geom_text(hjust=ifelse(test = dfct$Diagnosed == "COVID-19",  yes = 1.1, no = -0.1), size=6, fontface=2,colour="black") +
scale_y_continuous(labels = abs, limits = max(dfct$Percentage) * c(-1,1) * 1.1) +
  
coord_flip() +

  scale_fill_manual(values=c("maroon","skyblue"))+

  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold'),
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(y=expression(bold("Percentage")),x=expression(bold("Smoker")))

figure<-ggarrange(g3,g4,ncol=1,nrow=2,labels=c("B","C"))
png('../figures/fig4bc.png',width=1200,height=500)
figure
dev.off()


#*********************************Figure 4(a)**********************************************

dfct<-read.csv('../data/gender.csv')
g2<-ggplot(data = dfct, 
   mapping = aes(
      x = Gender, 
      y = ifelse(test = Diagnosed == "COVID-19", yes = -Percentage, no = Percentage), 
      fill = Diagnosed,
       label=paste(round(Percentage, 2))
   )) +
geom_bar(stat = "identity") +
geom_text(hjust=ifelse(test = dfct$Diagnosed == "COVID-19",  yes = 1.1, no = -0.1), size=6, fontface=2,colour="black") +
scale_x_discrete(labels=c('Female','Male','',''))+
scale_y_continuous(labels = abs, limits = max(dfct$Percentage) * c(-1,1) * 1.1) +
  
coord_flip() +

  scale_fill_manual(values=c("maroon","skyblue"))+

  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold'),
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(y=expression(bold("Percentage")),x=expression(bold("Gender")))

figure<-ggarrange(g2,ncol=1,nrow=1,labels=c("A"))
png('../figures/fig4a.png',width=1200,height=300)
figure
dev.off()


#*********************************Figure 4(d)**********************************************

g5<-ggplot()+theme_void()

g1<-ggplot(dft, aes(x=Diagnosed, y=age, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 10)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour="grey40"),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face="bold",colour="grey40"),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Age")))


figure<-ggarrange(g1,g5,g5,ncol=3,nrow=1,labels=c("D"))
png('../figures/fig4d.png',width=1200,height=350)
figure
dev.off()



#*********************************Figure 5**********************************************

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(3.81,8.21))
h1<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,10)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Seasonal Allergies')))
#ggsave("../figures/temp.png", h1, width =4, height = 4)


df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(4.76,3.73))
h4<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,6)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Diabetes')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(2.86,6.72))
h5<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,10)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Obesity')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(3.81,0.75))
h6<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,5)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Chronic Sinus Problems')))
#ggsave("../figures/temp.png", h6, width =4, height = 4)


df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(8.57,7.46))
h7<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,10)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('High BP')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(1.9,0.75))
h8<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,2.5)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Head Trauma')))


df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(4.73,3.73))
h9<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,6)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Dry Mouth')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(0.95,0))
h10<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,1.25)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Heart Disease')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(0,0.75))
h11<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,1.25)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Lung Disease')))

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(0.95,0.75))
h12<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,1.25)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Neurological Disease')))


df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(0,2.24))
h13<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,3)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('Other')))


df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(37.14,67.91))
h14<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,100)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold('Percentage')),title=expression(bold('No_Comorbity')))

figure <- ggarrange(h1,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,
                    labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L"),
                    ncol = 3, nrow = 4)
png('../figures/fig5.png',width=1200,height=880)
figure
dev.off()


#*********************************Figure 6**********************************************

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(73.33,20.9))
s1<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,100)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Fever')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(6.67,0.75))
s2<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,10)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Change in Food Flavor')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(13.33,2.24))
s3<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,20)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Change in Smell')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(13.33,1.49))
s4<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,20)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Change in Taste')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(7.62,15.67))
s5<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,20)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Headache')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(10.48,8.96))
s6<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,15)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Fatigue')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(8.57,8.21))
s7<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,12)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Body Aches')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(2.86,2.24))
s8<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,4)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Nausea')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(25.71,14.93))
s9<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,30)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Runny Nose')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(35.24,6.72))
s10<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,40)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Cough with Mucus')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(21.9,15.67))
s11<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,25)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Dry Cough')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(11.43,7.46))
s12<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,15)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Sore Throat')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(0,3.73))
s13<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,5)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Skin Sensitivity')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(1.9,4.48))
s14<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,6)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Abdominal Pain')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(16.19,2.24))
s15<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,20)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Chest Tightness')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(3.81,3.73))
s16<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,5)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Diarrhea')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("1.9,2.24")),
                Percentage=c(5.56,3.6))
s17<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,8)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Dry Mouth')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(4.76,3.75))
s18<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,8)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Loss of Appetite')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(9.52,0.75))
s19<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,12.5)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('Difficulty in Breathing')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)

df2 <- data.frame(Diagnosed=rep(c("COVID-19", "Non-COVID-19"), each=1),
                Moral=rep(c("")),
                Percentage=c(10.48,39.55))
s20<-ggplot(data=df2, aes(x=Diagnosed, y=Percentage, fill=Diagnosed)) +
  ggtitle('')+
  geom_bar(stat="identity", width=0.8,position=position_dodge(width=0.8))+
     geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25,size=7,fontface=2)+ylim(0,50)+
  scale_fill_manual(values=c("maroon","skyblue"))+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Percentage")),title=expression(bold('No symptoms')))
#ggsave("../figures/temp.png", s1, width =4, height = 4)


figure <- ggarrange(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,
                    labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"),
                    ncol = 3, nrow = 7)
png('../figures/fig6.png',width=1200,height=1540)
figure
dev.off()


#*********************************Figure 7**********************************************

dfct<-read.csv('../data/change_in_smell.csv')
sps<-ggplot(data = dfct, 
   mapping = aes(
      x = Change.in.Smell, 
      y = ifelse(test = Diagnosed == "COVID-19", yes = -Percentage, no = Percentage), 
      fill = Diagnosed,
       label=paste(round(Percentage, 2))
   )) +
geom_bar(stat = "identity") +
geom_text(hjust=ifelse(test = dfct$Diagnosed == "COVID-19",  yes = 1.1, no = -0.1), size=7, fontface=2,colour="black") +
scale_y_continuous(labels = abs, limits = max(dfct$Percentage) * c(-1,1) * 1.1) +  
coord_flip() +

  scale_fill_manual(values=c("maroon","skyblue"))+

  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold'),
        
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(y=expression(bold("Percentage")),x=expression(bold("Change in Smell")))


dfct<-read.csv('../data/change_in_taste.csv')
spt<-ggplot(data = dfct, 
   mapping = aes(
      x = Change.in.Taste, 
      y = ifelse(test = Diagnosed == "COVID-19", yes = -Percentage, no = Percentage), 
      fill = Diagnosed,
       label=paste(round(Percentage, 2))
   )) +
geom_bar(stat = "identity") +
geom_text(hjust=ifelse(test = dfct$Diagnosed == "COVID-19",  yes = 1.1, no = -0.1), size=7, fontface=2,colour="black") +
scale_y_continuous(labels = abs, limits = max(dfct$Percentage) * c(-1,1) * 1.1) +  
coord_flip() +

  scale_fill_manual(values=c("maroon","skyblue"))+

  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold'),
       
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+

labs(y=expression(bold("Percentage")),x=expression(bold("Change in Taste")))

figure<-ggarrange(spt,sps,ncol=1,nrow=2,labels=c("A","B"))
png('../figures/fig7.png',width=1200,height=500)
figure
dev.off()


#*********************************Figure 8**********************************************

t1<-ggplot(dft, aes(x=Diagnosed, y=Scented_detergents, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Scented Detergents")))

t2<-ggplot(dft, aes(x=Diagnosed, y=Spices_Herbs1, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Spices/Herbs-1")))

t3<-ggplot(dft, aes(x=Diagnosed, y=Spices_Herbs2, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Spices/Herbs-2")))

t4<-ggplot(dft, aes(x=Diagnosed, y=Spice_Mixtures, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Spice Mixtures")))

t5<-ggplot(dft, aes(x=Diagnosed, y=Fruits_Vegetables, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Fruits/Vegetables")))

t6<-ggplot(dft, aes(x=Diagnosed, y=Dairy, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Dairy items")))

t7<-ggplot(dft, aes(x=Diagnosed, y=Other, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Other items")))

t8<-ggplot(dft, aes(x=Diagnosed, y=Nasal_irritant, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Nasal irritants")))

figure <- ggarrange(t1,t2,t3,t4,t5,t6,t7,t8,
                    labels = c("A", "B", "C","D","E","F","G","H"),
                    ncol = 3, nrow = 3)
png('../figures/fig8.png',width=1200,height=1050)
figure
dev.off()


#*********************************Figure 9**********************************************

s1<-ggplot(dft, aes(x=Diagnosed, y=Sweet, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Sweet")))

s2<-ggplot(dft, aes(x=Diagnosed, y=Salty, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Salty")))

s3<-ggplot(dft, aes(x=Diagnosed, y=Sour, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Sour")))

s4<-ggplot(dft, aes(x=Diagnosed, y=Bitter, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Bitter")))

s5<-ggplot(dft, aes(x=Diagnosed, y=Taste_irritant, width = ..density.., fill=Diagnosed)) +scale_fill_manual(values = c("maroon","skyblue"))+
  geom_vridgeline(stat="ydensity",  scale = 20)+theme_ridges(grid=FALSE,center_axis_labels = TRUE)+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold',colour='grey40'),
        axis.text.y = element_text(angle=0, hjust=0.8,vjust=0.5,size=20,face='bold',colour='grey40'),
        legend.position="none",
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"), axis.ticks.y = element_line(colour='black',size=0.5))+
labs(x=expression(bold("Diagnosed")),y=expression(bold("Intensity")),title=expression(bold("Taste irritants")))

figure <- ggarrange(s1,s2,s3,s4,s5,
                    labels = c("A", "B", "C","D","E"),
                    ncol = 3, nrow = 2)
png('../figures/fig9.png',width=1200,height=700)
figure
dev.off()

#*********************************Figure 10**********************************************

x1=c(0.5,2,3.5,5,6.5,10.5,12,13.5,15,16.5,20.5,22,23.5,25,26.5,30.5,31.5,33,34.5,36)
x2=x1+0.5
y1=c(68.89,70.25,58.27,68.84,72.41,69.13,69.63,58.33,68.33,72.32,68.91,69.42,58.23,67.71,71.15,68.75,69.27,57.49,67.65,71.33)
y2=c(80.16,84.21,72.66,81.62,82.98,80.23,85.43,72.63,82.84,83.75,79.91,82.85,72.02,80.12,82.37,79.93,83.16,72.05,80.67,82.51)

#library(ggplot2)

d=data.frame(x1=x1, x2=x2,y1=y1,y2=y2, Features=c('GEN','OBSAT','GBSAT','OBSAT+GBSAT','GEN+OBSAT+GBSAT','GEN','OBSAT','GBSAT','OBSAT+GBSAT','GEN+OBSAT+GBSAT','GEN','OBSAT','GBSAT','OBSAT+GBSAT','GEN+OBSAT+GBSAT','GEN','OBSAT','GBSAT','OBSAT+GBSAT','GEN+OBSAT+GBSAT'))
d$Features = factor(d$Features, levels=c('GEN','OBSAT','GBSAT','OBSAT+GBSAT','GEN+OBSAT+GBSAT'))
p<-ggplot() + 
geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Features), color="black", alpha=0.5) +
#geom_text(data=d, aes(x=x1+(x2-x1)/2, label=c(3.5,12,20.5,29)),size=4) +
 theme(plot.title = element_text(hjust = 0.5,vjust=0.5,size=20,face='bold'),text = element_text(size=20,face='bold'),
        axis.title=element_text(size=20,face="bold"),axis.text.x = element_blank(),
        axis.text.y = element_text(angle=0, hjust=0.5,vjust=0.5,size=20,face='bold'),
        legend.text=element_text(size=20,face='bold'),legend.title=element_text(size=20,face='bold'),
       panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"),axis.line = element_line(colour = "black"))+
#coord_flip(clip = "off") +
labs(x=expression(bold("")),y=expression(bold("Evaluation Metric Score (LB to UB)")),title=expression(bold("Accuracy\t\t\t\t\tPrecision\t\t\t\t\tRecall\t\t\t\t\tF1-Score")))
p<-p+ geom_vline(xintercept=8.5,linetype="dotted")
p<-p+ geom_vline(xintercept=18.5,linetype="dotted")
p<-p+ geom_vline(xintercept=28.5,linetype="dotted")
png('../figures/fig10.png',width=1200,height=700)
p
dev.off()

