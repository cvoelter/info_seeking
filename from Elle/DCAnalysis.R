## Double-cylinders analysis ##
rm(list=ls())
setwd("~/Studies/Peeking/Cylinders/Analysis")

#-----------------------------------------------------------------------#
# T-tests #
library(lme4) #load packages
library(multcomp) 
DCaverages=read.table(file="from Elle/DCAverages.txt",header=T) #load data

#1.a. cup choice vs chance in visibility conditions
trans<-subset(DCaverages,Condition=="Transparent") # subset
opaque<-subset(DCaverages,Condition=="Opaque") #subset
opaque2<-subset(DCaverages,Condition=="Opaque2") #subset

t.test(trans$score,mu=0.33,conf.int = TRUE) # one-sample t-test
t.test(opaque$score,mu=0.33,conf.int=TRUE) # one-sample t-test
t.test(opaque2$score,mu=0.33,conf.int=TRUE) # one-sample t-test

t.test(opaque$score,trans$score,paired=T,conf.int=T) # paired t-test
t.test(opaque2$score,opaque$score,paired=T,conf.int=T) # paired t-test

aov1 <- aov(score ~ Condition, data =DCaverages) #anova as there are 3 groups
summary(aov1) # signficant difference between means so do a post hoc test

summary(glht(aov1, linfct = mcp(Condition = "Tukey"))) #Tukey post hoc test for multiple comparisons
Meanz<-tapply(DCaverages$score,DCaverages$Condition,mean)
barplot(Meanz,ylim=c(0,1),col="gold")

#1.b. peeking in visibility conditions
#is PPP differnt to PIP?
t.test(DCaverages$peek,DCaverages$PPP, paired=T,confint=T)

t.test(opaque$peek,trans$peek,paired=T,conf.int=T) #paired t-test for Peeking-interval-peeks (PIPs)
t.test(opaque$PPP,trans$PPP,paired=T,conf.int=T) #paired t-test for pre-peeking peeks (PPPs)
t.test(opaque$All_peek,trans$All_peeks,paired=T,conf.int=T) #paired t-test for any peek

aov2 <- aov(peek ~ Condition, data =DCaverages) #anova as there are 3 groups
summary(aov2) # signficant difference between means so do a post hoc test
summary(glht(aov2, linfct = mcp(Condition = "Tukey"))) #Tukey post hoc test for multiple comparisons

aov3 <- aov(PPP ~ Condition, data =DCaverages) #anova as there are 3 groups
summary(aov3) # if signficant difference between means do a post hoc test

aov4 <- aov(All_peeks ~ Condition, data =DCaverages) #anova as there are 3 groups
summary(aov4) # if signficant difference between means do a post hoc test

#---------------------------------------------------------------------------------#
## GLMs ##
source("C:/Users/ej33/Documents/R/MODELS KIDS/Diagnostic_fcns.r") # tell r packages location
library(car) # load packages
library(lme4) 
library(multcomp) 

library(lme4)
library(readr)
library(tidyverse)
library(sjPlot)
library(ggthemes)
library(gridExtra)
library(reshape2)
DCdata=read.table(file="from Elle/DCGLM.txt",header=T) # load data


dc_individual_peek <- DCdata %>%
  filter(!is.na(correct)) %>%
  filter(peek==1) %>%
  group_by(Condition, ID) %>% 
  summarize(dc.correct = mean(correct), dc.length = length(correct))


t.test(dc_individual_peek[dc_individual_peek$Condition=="Opaque",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_peek[dc_individual_peek$Condition=="OpaqueProc2",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_peek[dc_individual_peek$Condition=="Visible",]$dc.correct, mu=1/3, alternative = "two.sided")


dc_individual_nopeek <- DCdata %>%
  filter(!is.na(correct)) %>%
  filter(peek==0) %>%
  group_by(Condition, ID) %>% 
  summarize(dc.correct = mean(correct), dc.length = length(correct))



t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="Opaque",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="OpaqueProc2",]$dc.correct, mu=1/3, alternative = "two.sided")
t.test(dc_individual_nopeek[dc_individual_nopeek$Condition=="Visible",]$dc.correct, mu=1/3, alternative = "two.sided")




dc_individual <- DCdata %>%
  filter(!is.na(correct)) %>%
  group_by(Condition, ID) %>% 
  summarize(dc.correct = mean(correct), dc.length = length(correct))


t.test(lids_individual[lids_individual$Phase=="transparent",]$lids.correct, lids_individual[lids_individual$Phase=="opaque",]$lids.correct, paired = TRUE, alternative = "two.sided")





dc_individual$Condition2 <- factor(dc_individual$Condition2, levels = c("transparent", "opaque"))








contr=glmerControl(optimizer="bobyqa",	optCtrl=list(maxfun=10000000)) # set contrl







## 1. GLM visibilty effect on peeking ##

# 1.a. all 3 visibility conditions. all peek types
z.Trialnumb1a=as.vector(scale(DCdata$trial_number)) #standardise 

GLM1a=glmer(anyPeek #1/0
           ~ Condition #visible /oopaque / opaque 2
           + z.Trialnumb1a#trial number per condition standardised above
           + (1+Condition + z.Trialnumb1a|ID), family = binomial,control=contr,data=DCdata)

null1a=glmer(anyPeek~ 1 + (1+Condition + z.Trialnumb1a|ID), family = binomial, control=contr,data=DCdata)

anova(null1a,GLM1a, test="Chisq")
#significant so see what is having a significant effect#
drop1(GLM1a, test="Chisq", control=contr)
#condtion is significant so run post hoc #
mc1a=glht(GLM1a, linfct = mcp(Condition = "Tukey"))
summary(mc1a)
#Opaque sig diff to other two so plot to see which way the effect is #
mean1a<-tapply(DCdata$anyPeek,DCdata$Condition,mean)
barplot(mean1a)
#trial number is also significant so plot to se which direction #
mean1a2=tapply(DCdata$anyPeek,DCdata$trial_number,mean)
plot(mean1a2)
#assumption checks#
overdisp.test(GLM1a)
xres1a=lm(anyPeek~ Condition+ z.Trialnumb1a,data=DCdata)
vif(xres1a)
#summary for model output table#
summary(GLM1a)

## 2. GLM baiting effect on peeking [Opaque trials only]
BothOpaques=subset(DCdata,Condition !="Visible")
z.Trialnumb2=as.vector(scale(BothOpaques$trial_number))

## 2.a. Baiting effect on any kind of peek [both opaque trials]
GLM2a=glmer(anyPeek~Configuration+z.Trialnumb2+(1+Configuration+z.Trialnumb2|ID),family=binomial,control=contr,data=BothOpaques)
null2a=glmer(anyPeek~1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)

anova(null2a,GLM2a, test="Chisq")
#not significant#
summary(GLM2a) #for model output table
#assumption checks #
overdisp.test(GLM2a)
xres2a=lm(anyPeek~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xres2a)

## 2.b. Baiting effect on any kind of above peek [both opaque trials]
GLM2b=glmer(anyPeek_above~Configuration+z.Trialnumb2+(1+Configuration+z.Trialnumb2|ID),family=binomial,control=contr,data=BothOpaques)
null2b=glmer(anyPeek_above~1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)

anova(null2b,GLM2b, test="Chisq")#not significant
summary(GLM2b) #for output table
#assumption checks #
overdisp.test(GLM2b)
xres2b=lm(anyPeek_above~Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xres2b)

## 2.c. Baiting effect on any kind of below peek [both opaque trials]
GLM2c=glmer(anyPeek_below~Configuration+z.Trialnumb2+(1+Configuration+z.Trialnumb2|ID),family=binomial,control=contr,data=BothOpaques)
null2c=glmer(anyPeek_below~1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)

anova(null2c,GLM2c, test="Chisq") #not significant
summary(GLM2c) # for output table
#assumption checks
overdisp.test(GLM2c)
xres2c=lm(anyPeek_below~Configuration+ z.Trialnumb2, data=BothOpaques)
vif(xres2c)

### 3. Interaction between condition and configuration [all 3 visibility conditions] ###
z.Trialnumber3=as.vector(scale(DCdata$trial_number))

GLMM3a=glmer(correct~Condition*Configuration + z.Trialnumber3 + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumber3|ID), family = binomial, control=contr,data=DCdata)
null3a=glmer(correct~ 1 + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumber3|ID), family = binomial, control=contr,data=DCdata)

anova(null3a,GLMM3a, test="Chisq")
drop1(GLMM3a, test="Chisq", control=contr)
summary(GLMM3a) #for output table  
#assumption checks#
overdisp.test(GLMM3a)
xres3a=lm(correct~ Condition*Configuration+ z.Trialnumber3,data=DCdata)
vif(xres3a)
#Is it significant?#
means3a<-tapply(DCdata$correct,list(DCdata$Configuration,DCdata$Condition),mean)
barplot(means3a,beside=T,ylim=c(0,1),legend=T,col=c("red","orange","green","blue"))

#Signficant interaction so subset data and run further analyses#
#Transparent#
Transparent=subset(DCdata, Condition =="Visible")
z.Trialnumber3b=as.vector(scale(Transparent$trial_number))

GLMM3b=glmer(correct~Configuration +z.Trialnumber3b +(1+Configuration+z.Trialnumber3b|ID),family=binomial,control=contr,data=Transparent)
null3b=glmer(correct~ 1 + (1+Configuration+z.Trialnumber3b|ID),family=binomial,control=contr,data=Transparent)

anova(null3b,GLMM3b, test="Chisq") #significant so look at which term
drop1(GLMM3b, test="Chisq", control=contr) #significant effect of condition so do post hocs
mc3b=glht(GLMM3b, linfct = mcp(Configuration = "Tukey")) #Tukey HSD post hoc
summary(mc3b)

summary(GLMM3a) #for output table  
#assumption checks#
overdisp.test(GLMM3b)
xres3b=lm(correct~ Configuration+ z.Trialnumber3b,data=Transparent)
vif(xres3b)

# Opaque #
Opaque=subset(DCdata, Condition =="Opaque")
z.Trialnumber3c=as.vector(scale(Opaque$trial_number))

GLMM3c=glmer(correct~Configuration +z.Trialnumber3c+(1+Configuration+z.Trialnumber3c|ID),family=binomial,control=contr,data=Opaque)
null3c=glmer(correct~ 1 + (1+Configuration+z.Trialnumber3c|ID),family=binomial,control=contr,data=Opaque)

anova(null3c,GLMM3c, test="Chisq") #not significant
summary(GLMM3c)   # output table
#assumption checks
overdisp.test(GLMM3c)
xres3c=lm(correct~ Configuration+ z.Trialnumber3c,data=Opaque)
vif(xres3c)

# Opaque 2 #
Opaque2=subset(DCdata, Condition =="OpaqueProc2")
z.Trialnumber3d=as.vector(scale(Opaque2$trial_number))

GLMM3d=glmer(correct~Configuration +z.Trialnumber3d+(1+Configuration+z.Trialnumber3d|ID),family=binomial,control=contr,data=Opaque2)
null3d=glmer(correct~ 1 + (1+Configuration+z.Trialnumber3c|ID),family=binomial,control=contr,data=Opaque2)

anova(null3d,GLMM3d, test="Chisq") #not significant
summary(GLMM3d)   #output table
#assumption checks#
overdisp.test(GLMM3d)
xres3d=lm(correct~ Configuration+ z.Trialnumber3d,data=Opaque2)
vif(xres3d)

#---------------------------------------------------------------------------------#
## more T TESTS ##
peekdata=read.table(file="DCPeekvNo.txt",header=T)
Bothopaque=subset(peekdata,Condition!="Visible")
NO<-subset(Bothopaque,peek=="Anypeek_N")
YES<-subset(Bothopaque,peek=="Anypeek_Y")

t.test(NO$score,mu=0.33,conf.int = TRUE)
t.test(YES$score,mu=0.33,conf.int = TRUE)
t.test(YES$score,NO$score,paired = F,conf.int=T)


#---------------------Appendix DATA --------------------------------------------##
# A.1.a All 3 visibility conditions. PIPs #
z.Trialnumb1a=as.vector(scale(DCdata$trial_number))
GLMA1a=glmer(peek~ Condition + z.Trialnumb1a+ (1+Condition + z.Trialnumb1a| ID), family = binomial,control=contr,data=DCdata)
nullA1a=glmer(peek~ 1 + (1+Condition + z.Trialnumb1a| ID), family = binomial, control=contr,data=DCdata)
anova(nullA1a,GLMA1a,test="Chisq")
drop1(GLMA1a,test="Chisq", control=contr)
summary(GLMA1a)
#significant effect of condition so run post hoc test#
mcA1a=glht(GLMA1a,linfct = mcp(Condition = "Tukey"))
summary(mcA1a)
# visible is diff from both opaques so plot to see how #
meanA1a<-tapply(DCdata$peek,DCdata$Condition,mean)
barplot(meanA1a,ylim=c(0,1),col="pink")
#significant effect of trial number so plot to see effect#
mcA1a2<-tapply(DCdata$peek,DCdata$trial_number,mean)
plot(mcA1a2,ylim=c(0,1),pch=8)
#check assumptions#
overdisp.test(GLMA1a)
xresA1a=lm(peek~ Condition+ z.Trialnumb1a,data=DCdata)
vif(xresA1a)

# A.1.b All 3 visibility condtions. PPPs #
#use same trial numbers as same data
GLMA1b=glmer(prePeek~ Condition + z.Trialnumb1a+ (1+Condition + z.Trialnumb1a| ID), family = binomial,control=contr,data=DCdata)
nullA1b=glmer(prePeek~ 1 + (1+Condition + z.Trialnumb1a| ID), family = binomial, control=contr,data=DCdata)
anova(nullA1b,GLMA1b,test="Chisq")#significant so drop1
drop1(GLMA1b,test="Chisq", control=contr)
summary(GLMA1b)
#significant so run post hoc test#
mcA1b=glht(GLMA1b,linfct = mcp(Condition = "Tukey"))
summary(mcA1b)
#plot to see direction of effect#
meanA1b<-tapply(DCdata$prePeek,DCdata$Condition,mean)
barplot(meanA1b,ylim=c(0,1),col="orange")
#check assumptions#
overdisp.test(GLMA1b)
xresA1b=lm(prePeek~ Condition+ z.Trialnumb)
vif(xresA1b)

# A.2.a Opaque2 excluded. Any Peek #
NoOpaque2<-subset(DCdata,Condition != "OpaqueProc2") #subset to remove Opaque2
z.Trialnumb2a=as.vector(scale(NoOpaque2$trial_number)) #standardise 

GLMA2a=glmer(anyPeek ~ Condition+ z.Trialnumb2a+ (1+Condition + z.Trialnumb2a| ID), family = binomial,control=contr,data=NoOpaque2)
nullA2a=glmer(anyPeek~ 1 + (1+Condition + z.Trialnumb2a| ID), family = binomial, control=contr,data=NoOpaque2)
anova(nullA2a,GLMA2a, test="Chisq")#significant
drop1(GLMA2a, test="Chisq", control=contr)
summary(GLMA2a)
#significant effect of condition so plot to see direction #
meanA2a<-tapply(NoOpaque2$anyPeek,NoOpaque2$Condition,mean)
barplot(meanA2a,ylim=c(0,1),col="red")
#assumption checking#
overdisp.test(GLMA2a)
xresA2a=lm(anypeek~ Condition+ z.Trialnumb2a,data=NoOpaque2)
vif(xresA2a)

# A.2.b Opaque2 excluded. PIP #
GLMA2b=glmer(peek ~ Condition+ z.Trialnumb2a+ (1+Condition + z.Trialnumb2a| ID), family = binomial,control=contr,data=NoOpaque2)
nullA2b=glmer(peek~ 1 + (1+Condition + z.Trialnumb2a| ID), family = binomial, control=contr,data=NoOpaque2)
anova(nullA2b,GLMA2b, test="Chisq")
drop1(GLMA2b, test="Chisq", control=contr)
summary(GLMA2b)
#significant so plot to see direction #
meanA2b<-tapply(NoOpaque2$peek,NoOpaque2$Condition,mean)
barplot(meanA2b,ylim=c(0,1),col="green")
#significant with trial so plot #
meanA2b2<-tapply(NoOpaque2$peek,NoOpaque2$trial_number,mean)
plot(meanA2b2,ylim=c(0,1),col="red")
#assumption checking#
overdisp.test(GLMA2b)
xresA2b=lm(peek~ Condition+ z.Trialnumb2a,data=NoOpaque2)
vif(xresA2b)

# A.2.c Opaque2 excluded. PPP #
GLMA2c=glmer(prePeek ~ Condition+ z.Trialnumb2a+ (1+Condition + z.Trialnumb2a| ID), family = binomial,control=contr,data=NoOpaque2)
nullA2c=glmer(prePeek~ 1 + (1+Condition + z.Trialnumb2a| ID), family = binomial, control=contr,data=NoOpaque2)
anova(nullA2c,GLMA2c, test="Chisq")
drop1(GLMA2c, test="Chisq", control=contr)
summary(GLMA2c)
#significant so plot #
meanA2c<-tapply(NoOpaque2$prePeek,NoOpaque2$Condition,mean)
barplot(meanA2c,ylim=c(0,1),col="skyblue")
#assumption checking#
overdisp.test(GLMA2c)
xresA2c=lm(prePeek~ Condition+ z.Trialnumb2a,data=NoOpaque2)
vif(xresA2c)

# A.3.a All 3 visibility conditions PIPs (All peeks) #
#use BothOpaques from earlier [BothOpaques=subset(DCdata,Condition !="Visible")]
z.Trialnumb2=as.vector(scale(BothOpaques$trial_number))
GLMA3a=glmer(peek ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA3a= glmer(peek ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA3a,GLMA3a, test="Chisq")
drop1(GLMA3a, test="Chisq", control=contr)
summary(GLMA3a)
# Significant so run post hoc test #
mcA3a=glht(GLMA3a, linfct = mcp(Configuration = "Tukey"))
summary(mcA3a)
#and plot to see direction
meansA3a<-tapply(BothOpaques$peek,BothOpaques$Configuration,mean)
barplot(meansA3a,ylim=c(0,1),col="gold")
#assumption chec
overdisp.test(GLMA3a)
xresA3a=lm(peek~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA3a)

# A.3.b All 3 visibility conditions PPPs (all peeks) #
#use BothOpaques & the trial standardisation from earlier
GLMA3b=glmer(prePeek ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA3b= glmer(prePeek ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA3b,GLMA3b, test="Chisq")
drop1(GLMA3b, test="Chisq", control=contr)
summary(GLMA3b)
# Significant so run post hoc test #
mcA3b=glht(GLMA3b, linfct = mcp(Configuration = "Tukey"))
summary(mcA3b)
#and plot to see direction
meansA3b<-tapply(BothOpaques$prePeek,BothOpaques$Configuration,mean)
barplot(meansA3b,ylim=c(0,1),col="blue")
#assumption chec
overdisp.test(GLMA3a)
xresA3a=lm(peek~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA3a)

# A.4.a Just Opaque. Any peek (all peeks) # 
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA4a=glmer(anyPeek ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA4a= glmer(anyPeek ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA4a,GLMA4a, test="Chisq") #not significant
summary(GLMA4a)
#assumption chec
overdisp.test(GLMA4a)
xresA4a=lm(anyPeek~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA4a)

# A.4.b. Just Opaque PIPs (all peeks) #
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA4b=glmer(peek ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA4b= glmer(peek ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA4b,GLMA4b, test="Chisq") 
summary(GLMA4b)
drop1(GLMA4b, test="Chisq", control=contr)
# Significant so run post hoc test #
mcA4b=glht(GLMA4b, linfct = mcp(Configuration = "Tukey"))
summary(mcA4b)
#and plot to see direction
meansA4b<-tapply(Opaque$peek,Opaque$Configuration,mean)
barplot(meansA4b,ylim=c(0,1),col="purple")
#assumption chec
overdisp.test(GLMA4b)
xresA4b=lm(peek~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA4b)

# A.4.c. Just Opaque PPPs (all peeks) #
# use "Opaque" dataset created before, and z.Trialnumber3c
# PPP isnt possible in Opaque None, so make new dataset with it removed#
NoNone<-subset(Opaque,Configuration !="none")
z.Trialnumber4c=as.vector(scale(NoNone$trial_number))

GLMA4c=glmer(prePeek ~ Configuration + z.Trialnumber4c + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
nullA4c= glmer(prePeek ~ 1 + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
anova(nullA4c,GLMA4c, test="Chisq") #not significant
summary(GLMA4c)
#assumption chec
overdisp.test(GLMA4c)
xresA4c=lm(prePeek~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA4c)

# A.5.a All 3 visibility conditions PIPs (Above peeks) #
#use BothOpaques and z.Trialnumb2 from earlier 
GLMA5a=glmer(peek_above ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA5a= glmer(peek_above ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA5a,GLMA5a, test="Chisq")
drop1(GLMA5a, test="Chisq", control=contr)
summary(GLMA5a)
# Significant so run post hoc test #
mcA5a=glht(GLMA5a, linfct = mcp(Configuration = "Tukey"))
summary(mcA5a)
#and plot to see direction
meansA5a<-tapply(BothOpaques$peek_above,BothOpaques$Configuration,mean)
barplot(meansA5a,ylim=c(0,1),col="gold")
#assumption check
overdisp.test(GLMA5a)
xresA5a=lm(peek_above~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA5a)

# A.5.b All 3 visibility conditions PPPs (above peeks) #
GLMA5b=glmer(prePeek_above ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA5b= glmer(prePeek_above ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA5b,GLMA5b, test="Chisq")
drop1(GLMA5b, test="Chisq", control=contr)
summary(GLMA5b)
# Significant so run post hoc test #
mcA5b=glht(GLMA5b, linfct = mcp(Configuration = "Tukey"))
summary(mcA5b)
#and plot to see direction
meansA5b<-tapply(BothOpaques$prePeek_above,BothOpaques$Configuration,mean)
barplot(meansA5b,ylim=c(0,1),col="salmon")
#assumption check
overdisp.test(GLMA5b)
xresA5b=lm(prePeek_above~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA5b)

# A.6.a Just Opaque. Any peek (above peeks) # 
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA6a=glmer(anyPeek_above ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA6a= glmer(anyPeek_above ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA6a,GLMA6a, test="Chisq") #not significant
summary(GLMA6a)
#assumption chec
overdisp.test(GLMA6a)
xresA6a=lm(anyPeek_above~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA6a)

# A.6.b. Just Opaque PIPs (above peeks) #
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA6b=glmer(peek_above ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA6b= glmer(peek_above ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA6b,GLMA6b, test="Chisq") 
summary(GLMA6b)
drop1(GLMA6b, test="Chisq", control=contr)
# Significant so run post hoc test #
mcA6b=glht(GLMA6b, linfct = mcp(Configuration = "Tukey"))
summary(mcA6b)
#and plot to see direction
meansA6b<-tapply(Opaque$peek_above,Opaque$Configuration,mean)
barplot(meansA6b,ylim=c(0,1),col="purple")
#assumption check
overdisp.test(GLMA6b)
xresA6b=lm(peek_above~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA6b)

# A.6.c. Just Opaque PPPs (above peeks) #
# PPP isnt possible in Opaque None, so make new dataset with it removed#
NoNone<-subset(Opaque,Configuration !="none")
z.Trialnumber4c=as.vector(scale(NoNone$trial_number))

GLMA6c=glmer(prePeek_above ~ Configuration + z.Trialnumber4c + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
nullA6c= glmer(prePeek_above ~ 1 + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
anova(nullA6c,GLMA6c, test="Chisq") 
summary(GLMA6c)
#assumption check
overdisp.test(GLMA6c)
xresA6c=lm(prePeek_above~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA6c)

# A.7.a All 3 visibility conditions PIPs (below peeks) #
#use BothOpaques and z.Trialnumb2 from earlier 
GLMA7a=glmer(peek_below ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA7a= glmer(peek_below ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA7a,GLMA7a, test="Chisq")
summary(GLMA7a)
#assumption check
overdisp.test(GLMA7a)
xresA7a=lm(peek_below~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA7a)

# A.7.b All 3 visibility conditions PPPs (below peeks) #
GLMA7b=glmer(prePeek_below ~ Configuration + z.Trialnumb2 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
nullA7b= glmer(prePeek_below ~ 1 + (1+Configuration + z.Trialnumb2 |ID), family =binomial, control=contr,data=BothOpaques)
anova(nullA7b,GLMA7b, test="Chisq")
summary(GLMA7b)
#assumption check
overdisp.test(GLMA7b)
xresA7b=lm(prePeek_below~ Configuration+ z.Trialnumb2,data=BothOpaques)
vif(xresA7b)

# A.8.a Just Opaque. Any peek (below peeks) # 
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA8a=glmer(anyPeek_below ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA8a= glmer(anyPeek_below ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA8a,GLMA8a, test="Chisq") #not significant
summary(GLMA8a)
#assumption check
overdisp.test(GLMA8a)
xresA8a=lm(anyPeek_below~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA8a)

# A.8.b. Just Opaque PIPs (below peeks) #
# use "Opaque" dataset created before, and z.Trialnumber3c
GLMA8b=glmer(peek_below ~ Configuration + z.Trialnumber3c + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
nullA8b= glmer(peek_below ~ 1 + (1+Configuration + z.Trialnumber3c |ID), family =binomial, control=contr,data=Opaque)
anova(nullA8b,GLMA8b, test="Chisq") 
summary(GLMA8b)
#assumption check
overdisp.test(GLMA8b)
xresA8b=lm(peek_below~ Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA8b)

# A.8.c. Just Opaque PPPs (below peeks) #
# PPP isnt possible in Opaque None, so make new dataset with it removed#
NoNone<-subset(Opaque,Configuration !="none")
z.Trialnumber4c=as.vector(scale(NoNone$trial_number))

GLMA8c=glmer(prePeek_below ~ Configuration + z.Trialnumber4c + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
nullA8c= glmer(prePeek_below ~ 1 + (1+Configuration + z.Trialnumber4c |ID), family =binomial, control=contr,data=NoNone)
anova(nullA8c,GLMA8c, test="Chisq") 
summary(GLMA8c)
drop1(GLMA8c, test="Chisq", control=contr)
# Significant so run post hoc test #
mcA8c=glht(GLMA8c, linfct = mcp(Configuration = "Tukey"))
summary(mcA8c)
#and plot to see direction
meansA8c<-tapply(Opaque$prePeek_below,Opaque$Configuration,mean)
barplot(meansA8c,ylim=c(0,1),col="lilac")
#assumption check
overdisp.test(GLMA8c)
xresA8c=lm(prePeek_below~Configuration+ z.Trialnumb3c,data=Opaque)
vif(xresA8c)

# A.9.a Opaque2 removed
#use dataset "NoOpaque2" created before, and z.Trialnumb2a
GLMA9=glmer(correct~Condition*Configuration + z.Trialnumb2a + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumb2a|ID), family = binomial, control=contr,data=NoOpaque2)
nullA9=glmer(correct~ 1 + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumb2a|ID), family = binomial, control=contr,data=NoOpaque2)
anova(nullA9,GLMA9, test="Chisq")
drop1(GLMA9, test="Chisq", control=contr)
summary(GLMA9) #for output table 
#is it significant?
meansA9<-tapply(NoOpaque2$correct,list(NoOpaque2$Configuration,NoOpaque2$Condition),mean)
barplot(meansA9,beside=T,ylim=c(0,1),legend=T,col=c("red","orange","green","blue"))
#assumption checks#
overdisp.test(GLMA9)
xresA9=lm(correct~ Condition*Configuration+ z.Trialnumb2a,data=NoOpaque2)
vif(xresA9)

# A.9.b None only baiting - effect of condition?
None<-subset(DCdata, Configuration == "none")
z.trialnone=as.factor(scale(None$trial_number))

GLMA9b=glmer(correct ~ Condition + z.trialnone + (1+Condition + z.trialnone|ID), family = binomial,control=contr,data=None)
nullA9b=glmer(correct ~ 1+ (1+Condition + z.trialnone|ID), family = binomial,control=contr,data=None)

anova(nullA9b,GLMA9b, test="Chisq")

#------------ More t-tests ------------#
peekdata=read.table(file="from Elle/DCPeekvNo.txt",header=T)
# opaque & opaque 2. PIP, PPP #
Bothopaque=subset(peekdata,Condition!="Visible")

PIPNO<-subset(Bothopaque,peek=="peek_N")
PIPYES<-subset(Bothopaque,peek=="peek_Y")

t.test(PIPNO$score,mu=0.33,conf.int = TRUE)
t.test(PIPYES$score,mu=0.33,conf.int = TRUE)
t.test(PIPYES$score,PIPNO$score,paired = F,conf.int=T)

PPPNO<-subset(Bothopaque,peek=="prepeek_N")
PPPYES<-subset(Bothopaque,peek=="prePeek_Y")

t.test(PPPNO$score,mu=0.33,conf.int = TRUE)
t.test(PPPYES$score,mu=0.33,conf.int = TRUE)
t.test(PPPYES$score,PPPNO$score,paired = F,conf.int=T)

#opaque1 only. any peek, PIP PPP #
Opaque1=subset(peekdata, Condition == "Opaque")

no<-subset(Opaque1,peek=="Anypeek_N")
yes<-subset(Opaque1, peek=="Anypeek_Y")

t.test(no$score,mu=0.33,conf.int = TRUE)
t.test(yes$score,mu=0.33,conf.int = TRUE)
t.test(yes$score,no$score,paired = F,conf.int=T)

PIPno<-subset(Opaque1,peek=="peek_N")
PIPyes<-subset(Opaque1, peek=="peek_Y")

t.test(PIPno$score,mu=0.33,conf.int = TRUE)
t.test(PIPyes$score,mu=0.33,conf.int = TRUE)
t.test(PIPyes$score,PIPno$score,paired = F,conf.int=T)

PPPno<-subset(Opaque1,peek=="prepeek_N")
PPPyes<-subset(Opaque1, peek=="prePeek_Y")

t.test(PPPno$score,mu=0.33,conf.int = TRUE)
t.test(PPPyes$score,mu=0.33,conf.int = TRUE)
t.test(PPPyes$score,PPPno$score,paired = F,conf.int=T)
