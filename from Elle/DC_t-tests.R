
library(lme4)
library(readr)
library(tidyverse)
library(sjPlot)
library(ggthemes)
library(gridExtra)
library(reshape2)
DCdata=read.table(file="from Elle/DCGLM.txt",header=T) # load data

##peeking data
dc_individual_peek <- DCdata %>%
  filter(!is.na(correct)) %>%
  filter(peek==1) %>%
  group_by(Condition, ID) %>% 
  summarize(dc.correct = mean(correct), dc.length = length(correct))


t.test(dc_individual_peek[dc_individual_peek$Condition=="Opaque",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_peek[dc_individual_peek$Condition=="OpaqueProc2",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_peek[dc_individual_peek$Condition=="Visible",]$dc.correct, mu=1/3, alternative = "two.sided")

##no peeking data
dc_individual_nopeek <- DCdata %>%
  filter(!is.na(correct)) %>%
  filter(peek==0) %>%
  group_by(Condition, ID) %>% 
  summarize(dc.correct = mean(correct), dc.length = length(correct))



t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="Opaque",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="OpaqueProc2",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="Visible",]$dc.correct, mu=1/3, alternative = "two.sided")



##peek vs no peek
dc_individual <- DCdata %>%
  filter(!is.na(correct)) %>%
  group_by(Condition, ID, peek) %>% 
  summarize(dc.correct = mean(correct)) %>% 
  spread(peek, dc.correct)%>% 
  rename(nopeek="0", peek="1")


t.test(dc_individual[dc_individual$Condition=="Opaque",]$nopeek, dc_individual[dc_individual$Condition=="Opaque",]$peek, paired = TRUE, alternative = "two.sided")

t.test(dc_individual[dc_individual$Condition=="OpaqueProc2",]$nopeek, dc_individual[dc_individual$Condition=="OpaqueProc2",]$peek, paired = TRUE, alternative = "two.sided")

t.test(dc_individual[dc_individual$Condition=="Visible",]$nopeek, dc_individual[dc_individual$Condition=="Visible",]$peek, paired = TRUE, alternative = "two.sided")

