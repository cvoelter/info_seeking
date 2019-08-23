rm(list=ls())
setwd("~/Studies/Peeking/Lids/Analysis")

#---------------------------------------GRAPH1----------------------------#
Graph1data=read.table(file="LidsAverages.txt",header=T)
library(lme4)
library(ggplot2)
library(cowplot)

bp1 <- ggplot(Graph1data, aes(x=Condition, y=score)) + 
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(width = 0.4,fill="goldenrod1")

bp1

plot1<- bp1 + scale_x_discrete(limits = rev(levels(Graph1data$Condition))) +
  expand_limits(y=c(0,1)) + 
  #stat_sum(aes(size = factor(..n..)),colour="maroon4",geom="point") +
  #scale_size_discrete(range = c(1, 8)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Choice performance") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_blank()) + 
  ylab("Mean proportion of correct choices") +
  theme(legend.position="none")+
  geom_hline(aes(yintercept=0.25), colour="red", linetype="dashed")

plot1

bp2 <- ggplot(Graph1data, aes(x=Condition, y=peek)) + 
  stat_boxplot(geom ='errorbar',width=0.2)+
  geom_boxplot(width=0.4,fill="salmon")

bp2

plot2 <- bp2 + scale_x_discrete(limits = rev(levels(Graph1data$Condition))) +
  expand_limits(y=c(0,1)) + 
 # stat_sum(aes(size = factor(..n..)), colour = "salmon", geom = "point") +
 # scale_size_discrete(range = c(1, 8)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Peeking") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_blank()) + 
  ylab("Mean proportion of peeking") +
  theme(legend.position="none")

plot2

plot_grid(plot1, plot2, labels = "AUTO")


#------------------------------------GRAPH2-------------------------------------#
Graph2data=read.table(file="LidsPeekvNo.txt",header=T)

bp3 <- ggplot(Graph2data, aes(x=peek, y=score),na.rm=TRUE) + 
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(width = 0.4,fill="seagreen3") 
  
bp3

plot3 <- bp3 + 
  expand_limits(y=c(0,1)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("  ") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ylab("Average proportion of correct cups chosen") +
  xlab("Presence of peeking") 

plot3

#---------------------------------------GRAPHs not in write up -------------------------#
DCdata=read.table(file="LidsPeekCond.txt",header=T)

bp4 <- ggplot(DCdata, aes(x=Configuration, y=peek)) + 
  geom_boxplot()

bp4

plot4 <- bp4 + scale_x_discrete(limits = rev(levels(DCdata$Condition))) +
  expand_limits(y=c(0,1)) + 
  stat_sum(aes(size = factor(..n..)), colour = "purple", geom = "point") +
  scale_size_discrete(range = c(1, 16)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Any peeking - opaque condition") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ylab("Mean proportion of peeking") +
  xlab("Cup configuration") +
  theme(legend.position="none")

plot4

bp5 <- ggplot(DCdata, aes(x=Configuration, y=peekAbove)) + 
  geom_boxplot()

bp5

plot5 <- bp5 + scale_x_discrete(limits = rev(levels(DCdata$Condition))) +
  expand_limits(y=c(0,1)) + 
  stat_sum(aes(size = factor(..n..)), colour = "purple", geom = "point") +
  scale_size_discrete(range = c(1, 16)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Peeking above - opaque condition") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ylab("Mean proportion of peeking") +
  xlab("Cup Configuration") +
  theme(legend.position="none")

plot5

bp6<- ggplot(DCdata, aes(x=Configuration, y=peekBelow)) + 
  geom_boxplot()

bp6

plot6 <- bp6 + scale_x_discrete(limits = rev(levels(DCdata$Condition))) +
  expand_limits(y=c(0,1)) + 
  stat_sum(aes(size = factor(..n..)), colour = "purple", geom = "point") +
  scale_size_discrete(range = c(1, 16)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Peeking below - opaque condition") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ylab("Mean proportion of peeking") +
  xlab("Cup configuration") +
  theme(legend.position="none")

plot6