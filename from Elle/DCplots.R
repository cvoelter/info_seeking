rm(list=ls())
setwd("~/Studies/Peeking/Cylinders/Analysis")

#---------------------------------------GRAPH1------------------------------------------------#
scoreplot=read.table(file="from Elle/DCAverages.txt",header=T)
library(lme4)
library(ggplot2)
library(cowplot)
library(forcats)

bp1 <- ggplot(scoreplot, aes(x=Condition, y=score)) + 
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(width = 0.4,fill="goldenrod1")

bp1

plot1<- bp1 + scale_x_discrete(limits = c("Transparent","Opaque","Opaque2")) + #rev(levels(DCdata$Condition))) +
  expand_limits(y=c(0,1)) + 
  #stat_sum(aes(size = factor(..n..)), colour = "purple", geom = "point") +
  #scale_size_discrete(range = c(1, 9)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Choice performance") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_blank()) + 
  ylab("Mean proportion of correct choices") +
  theme(legend.position="none")+
  geom_hline(aes(yintercept=0.33), colour="red", linetype="dashed")

plot1

peekplot=read.table(file="DCplot.txt",header=T)

bp2 <- ggplot(peekplot, aes(x=Condition, y=peek_freq, fill=Peek_Type)) + 
  stat_boxplot(geom ='errorbar',width=0.6)+
  geom_boxplot(width=0.6)

bp2

plot2 <- bp2 + scale_x_discrete(limits = c("Transparent","Opaque","Opaque2"))+ # rev(levels(DCdata$Condition)),drop=TRUE) +
  expand_limits(y=c(0,1)) + 
  #stat_sum(aes(size = factor(..n..)), colour = "purple", geom = "point") +
  #scale_size_discrete(range = c(1, 9)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Peeking")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_blank()) + 
  ylab("Mean proportion of peeking") +
  scale_fill_discrete(name = "Peek Type", label=c("PIP","PPP")) 

plot2


plot_grid(plot1, plot2, labels = "AUTO",nrow = 1,ncol = 2, rel_widths = c(.4,.6))


#------------------------------------- GRAPH 2 ---------------------------------#
graph2=read.table(file="DCgrph2.txt",header=T)

source("C:/Users/ej33/Documents/R/MODELS KIDS/Diagnostic_fcns.r")
#must have run the summarySE function before this will work#
G2Sum<-summarySE(graph2,measurevar="Score",groupvars=c("Condition","Configuration"))

bp3 <- ggplot(G2Sum, aes(x=Condition, y=Score, fill=Configuration))+
  geom_bar(position=position_dodge(), stat="identity",color="black") +
  geom_errorbar(aes(ymin=Score-se, ymax=Score+se),
                width=.2, position=position_dodge(.9))

bp3

plot3 <- bp3 + scale_x_discrete(limits = c("Transparent","Opaque","Opaque2"))+ # rev(levels(DCdata$Condition)),drop=TRUE) +
  expand_limits(y=c(0,1)) + 
    theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Average score for each of the visibility conditions broken down into the different baiting configurations  ")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_blank()) + 
  ylab("Mean proportion of correct choices") +
  scale_fill_discrete(name = "Baiting configuration") 

plot3

