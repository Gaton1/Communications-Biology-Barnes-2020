require(tidyr)
require(plyr)
require(dplyr)
require(mvabund)
require(vegan)
require(tidyverse)
require(data.table)
require(ggplot2)
require(grid)
require(car)
require(userfriendlyscience)
require(egg)

CompHeat<-read.csv("./CompHeat.csv")

##Analysis of competition
#Species Richness Fig 3C
Rich.aov<-aov(Rich~Treat,data=CompHeat)
summary(Rich.aov)
plot(Rich.aov,1)
leveneTest(Rich~Treat,data=CompHeat)
plot(Rich.aov,2)

#Rare Species count Fig 3D
Rare.aov<-aov(Rare~Treat,data=CompHeat)
summary(Rare.aov)
plot(Rare.aov,1)
leveneTest(Rare~Treat,data=CompHeat)
plot(Rare.aov,2)

#P of Contest Competition Fig 2C
PCC.aov<-aov(PCC~Treat,data=CompHeat)
summary(PCC.aov)
plot(PCC.aov,1)
leveneTest(PCC~Treat,data=CompHeat)
plot(PCC.aov,2)
TukeyHSD(PCC.aov)
oneway.test(PCC~Treat,data=CompHeat,var.equal=FALSE)
posthocTGH(y=CompHeat$PCC,x=CompHeat$Treat,method=c("games-howell"),digits=2)
Omega2PCC<-(4*(28.326-1))/(4*(28.326-1)+20)

#Density of Contest Competition Fig 2D
DCC.aov<-aov(DCC~Treat,data=CompHeat)
summary(DCC.aov)
plot(DCC.aov,1)
leveneTest(DCC~Treat,data=CompHeat)
plot(DCC.aov,2)
TukeyHSD(DCC.aov)
oneway.test(DCC~Treat,data=CompHeat,var.equal=FALSE)
posthocTGH(y=CompHeat$DCC,x=CompHeat$Treat,method=c("games-howell"),digits=2)
Omega2DCC<-(4*(6.5527-1))/(4*(6.5527-1)+20)

#P of Intraspecific Competition Fig 3E
PIC.aov<-aov(PIC~Treat,data=CompHeat)
summary(PIC.aov)
plot(PIC.aov,1)
leveneTest(PIC~Treat,data=CompHeat)
plot(PIC.aov,2)
TukeyHSD(PIC.aov)
oneway.test(PIC~Treat,data=CompHeat,var.equal=FALSE)
posthocTGH(y=CompHeat$PIC,x=CompHeat$Treat,method=c("games-howell"),digits=2)

#P of Competition w Frug Fig 3F
PCFrug.aov<-aov(PCFrug~Treat,data=CompHeat)
summary(PCFrug.aov)
plot(PCFrug.aov,1)
leveneTest(PCFrug~Treat,data=CompHeat)
plot(PCFrug.aov,2)
oneway.test(PCFrug~Treat,data=CompHeat,var.equal=FALSE)

#Community Complexity Fig 2E
CC.aov<-aov(CC~Treat,data=CompHeat)
summary(CC.aov)
plot(CC.aov,1)
leveneTest(CC~Treat,data=CompHeat)
plot(CC.aov,2)
TukeyHSD(CC.aov)

##Plot Spp Diversity Figs
#Fig3C
RichFig<-ggplot(CompHeat, aes(y=Rich,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(9,16.7))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("Species richness")+
  xlab(NULL)

#Fig3D
RareFig<-ggplot(CompHeat, aes(y=Rare,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(0,4.5))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("Rare spp count")+
  xlab(NULL)

#Fig2C
PCCFig<-ggplot(CompHeat, aes(y=PCC,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(0.46,1.08))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("P(contest competition)")+
  xlab(NULL)

#Fig2D
DCCFig<-ggplot(CompHeat, aes(y=DCC,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(0.5,5.7))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("Density (contest competition)")+
  xlab(NULL)

#Fig3E
PICFig<-ggplot(CompHeat, aes(y=PIC,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(0.731,0.915))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("P(Intraspec competition)")+
  xlab(NULL)

#Fig3F
PCFFig<-ggplot(CompHeat, aes(y=PCFrug,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(0.961,1.0035))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("P(Competition w Frug)")+
  xlab(NULL)

#Fig2E
CCFig<-ggplot(CompHeat, aes(y=CC,x=Treat))+
  geom_point(aes(color=Treat,shape=Treat,size=0.25),position=position_jitter(w=0.05,h=0))+
  theme_bw()+
  scale_color_manual(values=c("orange","red","black","black","blue"))+
  scale_shape_manual(values=c(19,17,15,7,18))+
  scale_y_continuous(limits=c(6,14))+
  theme(axis.text.x = element_blank(),
        legend.position="none")+
  ylab("Competition complexity")+
  xlab(NULL)


##nMDS on Community Composition
Commcomp<-read.csv("./Commcomp.csv")

set.seed=36
NMDS=metaMDS(subset(Commcomp,select=c(F:Pyura)),k=2,trymax=100)
NMDS

stressplot(NMDS)
plot(NMDS)

Commcomp<-cbind(Commcomp,as.data.frame(scores(NMDS)))

#ANOSIM and SIMPER on Community Composition
ano=anosim(as.matrix(subset(Commcomp,select=c(F:Pyura),k=2)),
           Commcomp$Treat,distance="bray",permutations=1000)
ano

summary(simper(subset(Commcomp,select=c(F:Pyura)),Commcomp$Treat,permutations=0))

#Fig3A
CommNMDS = ggplot(Commcomp, aes(x = NMDS1, y = NMDS2)) + 
    geom_point(size = 4, aes( shape = Treat, colour = Treat))+ 
    theme_bw()+
    theme(legend.position = "none",
          legend.key=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) + 
    xlab(NULL)+ 
    scale_colour_manual(values = c("orange","red","black","black","blue"))+
    scale_x_continuous(limits=c(-0.6,0.6))+
    scale_y_continuous(limits=c(-0.6,0.6))+
    scale_shape_manual(values=c(19,17,15,7,18))
CommNMDS
  

##NMDS on Competition Composition
Compcomp<-read.csv("./Compcomp.csv")

set.seed=36
NMDS=metaMDS(subset(Compcomp,select=c(FoF:Cv.o.F)),k=2,trymax=100)
NMDS

stressplot(NMDS)
plot(NMDS)

Compcomp<-cbind(Compcomp,as.data.frame(scores(NMDS)))

#ANOSIM and SIMPER on Competition Composition
ano=anosim(as.matrix(subset(Compcomp,select=c(FoF:Cv.o.F),k=2)),
           Compcomp$Treat,distance="bray",permutations=1000)
ano

summary(simper(subset(Compcomp,select=c(FoF:Cv.o.F)),Compcomp$Treat,permutations=0))

#Fig 3B
CompNMDS = ggplot(Compcomp, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( shape = Treat, colour = Treat))+ 
  theme_bw()+
  theme(legend.position = "none",
        legend.key=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) + 
  xlab(NULL)+
  scale_colour_manual(values = c("orange","red","black","black","blue"))+
  scale_x_continuous(limits=c(-0.6,0.6))+
  scale_y_continuous(limits=c(-0.6,0.6))+
  scale_shape_manual(values=c(19,17,15,7,18))
CompNMDS


##Composite Figs
#Fig2C-E
ggarrange(PCCFig,CCFig,DCCFig,
          ncol=2,nrow=2)
#Fig3A-F
ggarrange(CommNMDS,CompNMDS,RichFig,RareFig,PICFig,PCFFig,
          ncol=2,nrow=3)
