dx <- read.table("CLIMER TFF R COMPLETE.txt", sep = "\t", header = TRUE)
head(dx)

library(lubridate)
library(scales)
dx$Date <- as.Date(parse_date_time(dx$Date, "%d/%m/%y"))

#Nødvendig for å kunne få dit på x-aksen
library(plyr)
dx$Month <- month(dx$Date, label=TRUE) 
dx$year <- year(dx$Date) 
head(dx)

#Order variables
#ORDERING the factor variables
dx$Position <- factor(dx$Position,
                      levels = c("Inlet", "Hypo", "Outlet"),ordered = TRUE)
dx$Size <- factor(dx$Size,
                  levels = c("LMW", "HMW"),ordered = TRUE)
dx$Date <- factor(dx$Date,
                  levels = c("15.09.2016", "25.01.2017", "01.04.2017", "06.06.2017", "22.09.2017", 
                             "29.05.2018", "20.09.2018","17.10.2018", "21.05.2019", "07.06.2019"),ordered = TRUE)

head(dx)

#1) Barplot: seasonal, relative. Muligens det samme som i punkt 3...
library(dplyr)
library(Rmisc)
library(ggplot2)
dx <- na.omit(dx)
DOC <- select(dx, DOC_frac, Size, Position)
Hg <- select(dx, THg, MeHg, THg_DOC, MeHg_DOC, Size, Position)
d <- summarySE(Hg, measurevar="THg_DOC", groupvar=c("Position", "Size"))
head(d)

ggplot(d, aes(x=Position, y=THg_DOC, fill=factor(Size))) + 
  geom_bar(stat="identity", position=position_dodge())
theme(axis.text.y = element_text(size= 20, colour="black"),
      axis.title.x = element_text(size = 20, margin=margin(20,0,0,0)),
      axis.title.y = element_text(size = 24, margin=margin(0,20,0,0)),
      strip.text.x = element_text(size = 26),
      axis.text.x = element_text(angle=0, vjust=1, size = 24, colour="black"),
      plot.title=element_text(size=30))+
  labs(title = "MeHg/DOC", x = "", y = "MeHg:DOC (ng/mg)")+
  geom_errorbar(aes(ymin=MeHg_DOC-sd, ymax=MeHg_DOC+sd), position=position_dodge(0.9),
                width=.2, col="black")


#2) VELDIG BRA BAR FOR TID PÅ X-AKSE 
#for the facet, the side of the variable you put the .~ determined if vertical or horisontal
dx$Date = as.Date(dx$Date)
HMW<- subset(dx, Size == "HMW")
LMW<- subset(dx, Size == "LMW")
head(dx)
#See all data next to each other either with natural or compressed x-axis
ggplot(data = subset(dx, Position == "Inlet"|Position == "Outlet"||Position == "Hypolimnion"),
       aes(y=DOC_natural, x=Date, fill=(factor(Position))))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(position=position_dodge(), stat="identity", size=1, linetype="dashed")+
  facet_grid(Size~., scale="free")
ggtitle("sUVa")


# 3) For å se om forskjeller i gjennomsnitt mellom inlet, hypo og outlet. Lager et sammendrag først og så plottes det
library(Rmisc)
head(dx)
dx =na.omit(dx)
ds <- summarySE(dx, measurevar="Labile", groupvar=c("Size", "Position"))
head(ds)
length(dx$Position)

head(ds)
# Convert dose to a factor variable
df2$dose=as.factor(df2$dose)

ggplot(ds, aes(x=factor(Position), y=Labile)) + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Labile-sd, ymax=Labile+sd), width=0.2, position=position_dodge(.9))+
  facet_grid(. ~ factor(Size))


#4) #4) Correlation plots 
#Correlation plots, LMW/HMW, and INlet/Outlet
d <- subset(d, Lake == "Inlet"| Lake =="Outlet")
d <- subset(d, Fraction == "LMW"| Fraction =="HMW")
head(dx)
#IMPORTANT!!! Correlation plot for biodeg and parameters
ggplot(dx, aes(DOC_natural, DOC_TFF))+
  geom_point(aes(colour=interaction(factor(Size))), size=6)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  facet_grid(. ~ factor(Position))+
  theme(axis.text.y = element_text(size= 22),
        axis.title.x = element_text(size = 24, margin=margin(20,0,0,0)),
        axis.title.y = element_text(size = 24, margin=margin(0,20,0,0)),
        strip.text.x = element_text(size = 32),
        axis.text.x = element_text(angle=0, hjust=0.5, vjust=0, size = 24),
        plot.title=element_text(size=30, face="bold"),
        legend.text=element_text(size=26),
        legend.title = element_blank())
labs(title = "DOM Size and Aromaticity", x = "sUVa (Abs254:DOC*100)", y = "Rate of DOC decay:DOC (/h)")
ylim(0, -0.007)+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4"))+
  xlim(0, 8)
