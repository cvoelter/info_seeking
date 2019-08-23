## Lids analysis ##
rm(list=ls())
setwd("~/Studies/Peeking/Lids/Analysis")

# 1. t-tests #
library(lme4) #load required packages
LidsAverages=read.table(file="LidsAverages.txt",header=T) #load data

#1.a. cup choice in visible condition vs chance
Vis<-subset(LidsAverages,Condition=="Visible") # first subset data
t.test(Vis$score,mu=0.25,conf.int = TRUE) # one-sample t-test

#1.b. cup choice in occluded condition vs chance
occlude<-subset(LidsAverages,Condition=="Occluded") # fist subset
t.test(occlude$score,mu=0.25,conf.int=TRUE) # one-sample t-test

#1.c. cup choice visible vs occluded
t.test(occlude$score,Vis$score,paired=T,conf.int=T) #paired t-test

#1.d. peeking visible vs occluded
t.test(occlude$peek,Vis$peek,paired=T,conf.int=T) #paired t-test

## 2. GLMs ##
source("C:/Users/ej33/Documents/R/MODELS KIDS/Diagnostic_fcns.r") #load required packages
library(lme4)
library(car)
Lidsdata=read.table(file="LidsGLM.txt",header=T) #load data

contr=glmerControl(optimizer="bobyqa",	optCtrl=list(maxfun=10000000)) #set  contr for GLMs

##2.a.GLM1 Visiblity effect on peeking
attach(Lidsdata)
z.Trialnumb=as.vector(scale(trial_number)) #standardise numerical covariates

GLM1=glmer(any_peek #1/0
           ~ Condition #visible /occluded 
           + z.Trialnumb  #trial number per condition standardised above
           + (1+Condition + z.Trialnumb| ID), #random effects
           family = binomial,control=contr,data=Lidsdata)
null1=glmer(any_peek~ 1 + (1+Condition + z.Trialnumb| ID), family = binomial, control=contr,data=Lidsdata)

anova(null1,GLM1, test="Chisq") # full vs null to see if predictors have a significant effect on model fit
drop1(GLM1, test="Chisq", control=contr) #ONLY if anova is significant to see which predictor is haveign the effect
summary(GLM1)

overdisp.test(GLM1) #check overdispersion assumptions
xres1=lm(any_peek~ Condition+ z.Trialnumb,data=Lidsdata) #check VIF 
vif(xres1)

##2.b.GLM2A baiting effect on peeking [occluded trials only]
detach(Lidsdata)
occlude=subset(Lidsdata,Condition=="Occluded") #subsetdata
attach(occlude)

z.Trialnumb=as.vector(scale(trial_number)) #after subsetting re-standardize

GLM2a=glmer(any_peek ~ Configuration + z.Trialnumb +  (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

null2a= glmer(any_peek ~ 1 +  (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

anova(null2a,GLM2a, test="Chisq")
drop1(GLM2a, test="Chisq", control=contr)
summary(GLM2a) 

overdisp.test(GLM2a)
xres2a=lm(any_peek~ Configuration+ z.Trialnumb, data=occlude)
vif(xres2a)

##2.c.GLM2B baiting effect on peeking above [Occluded trials only]
GLM2b=glmer(peek_above ~ Configuration #(all baited/all open/ mixed)
            + z.Trialnumb + (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

null2b= glmer(peek_above ~ 1 + (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

anova(null2b,GLM2b, test="Chisq")
drop1(GLM2b, test="Chisq", control=contr)
summary(GLM2b)

overdisp.test(GLM2b)
xres2b=lm(peek_above~ Configuration+ z.Trialnumb,data=occlude)
vif(xres2b)

##2.d.GLM2C baiting effect on peekign below [Occluded trials only]
GLM2c=glmer(peek_below ~ Configuration + z.Trialnumb +(1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

null2c= glmer(peek_below ~ 1 + (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr,data=occlude)

anova(null2c,GLM2c, test="Chisq")
drop1(GLM2c, test="Chisq", control=contr)
summary(GLM2c) 

overdisp.test(GLM2c)
xres2c=lm(peek_below~ Configuration+ z.Trialnumb,data=occlude)
vif(xres2c)

##2.e.GLM3 condition and configuration interaction effect on cup choice
detach(occlude)
Lidsdata=read.table(file="LidsGLM.txt",header=T) #load data
attach(Lidsdata)
z.Trialnumb=as.vector(scale(trial_number))

GLMM3=glmer(correct~Condition* Configuration + z.Trialnumb + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=Lidsdata)

null3=glmer(correct~ 1 + (1|ID) + (0+Condition*Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=Lidsdata)

anova(null3,GLMM3, test="Chisq")
drop1(GLMM3, test="Chisq", control=contr)
summary(GLMM3)   

overdisp.test(GLMM3)
xres3=lm(correct~ Condition*Configuration+ z.Trialnumb,data=Lidsdata)
vif(xres3)

##2.f. GLM3a - significant interaction for GLM3 so subset data again and run just occluded
detach(Lidsdata)
occlude=subset(Lidsdata,Condition=="Occluded")
attach(occlude)
z.Trialnumb=as.vector(scale(trial_number))

GLMM3a=glmer(correct~Configuration + z.Trialnumb + (1|ID) + (0+Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=occlude)

null3a=glmer(correct~1 + (1|ID) + (0+Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=occlude)

anova(null3a,GLMM3a, test="Chisq")
drop1(GLMM3a, test="Chisq", control=contr)
summary(GLMM3a) 

overdisp.test(GLMM3a)
xres3a=lm(correct~ Configuration+ z.Trialnumb,data=occlude)
vif(xres3a)

##2.g. GLM3b - significant interaction for GLM3 so subset data again and run just visible
detach(occlude)
visible=subset(Lidsdata,Condition=="Transparent")
attach(visible)
z.Trialnumb=as.vector(scale(trial_number))

GLMM3b=glmer(correct~Configuration + z.Trialnumb + (1|ID) + (0+Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=visible)

null3b=glmer(correct~1 + (1|ID) + (0+Configuration |ID)+ (0+z.Trialnumb|ID), family = binomial, control=contr,data=visible)

anova(null3b,GLMM3b, test="Chisq")
drop1(GLMM3b, test="Chisq", control=contr)
summary(GLMM3b) 

overdisp.test(GLMM3b)
xres3b=lm(correct~ Configuration+ z.Trialnumb, data=visible)
vif(xres3b)

### 3. Pairwise comparisons

###3.a. Significant GLM so perform pairwise comparisons
library(multcomp) #load packages

Pair=glht(GLMM3b, linfct = mcp(Configuration = c("All_Baited-Mixed = 0", "All_Baited-All_Open = 0", "Mixed-All_Open = 0")))
summary(Pair)

#basic barplot to visualise mulitcomp#
g3means<-tapply(correct,Configuration,mean,data=transparent)
barplot(g3means,ylim=c(0,1))

#####4. t-tests

#####5.a. effect of peeking on cup choice
Lidspeek=read.table(file="LidsPeekvNo.txt",header=T)

No<-subset(Lidspeek,peek=="No")
t.test(No$score,mu=0.25,conf.int = TRUE)

Yes<-subset(Lidspeek,peek=="Yes")
t.test(Yes$score,mu=0.25,conf.int = TRUE)
t.test(Yes$score,No$Score,paired = F,conf.int=T)
