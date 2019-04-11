#rm(list=ls())
#setwd("~/NEW STUDY/Peeking/Lids/Analysis")
Lidsdata=read.table(file="data/LidsGLM_capuchins.txt",header=T)
View(Lidsdata)
attach(Lidsdata)
library(lme4)
summary(Lidsdata)
names(Lidsdata)

library(summarytools)
view(dfSummary(Lidsdata))
contr=glmerControl(optimizer="bobyqa",	optCtrl=list(maxfun=10000000))

z.Trialnumb=as.vector(scale(trial_number))

#GLMM1: To analyse the effects of occlusion (visible vs occluded) on participants looking#
  
GLM1=glmer(any_peek #1/0
           ~ Condition #visible /occluded 
           + z.Trialnumb  #trial number per condition standardised above
           + (1+Condition + z.Trialnumb| ID), family = binomial,control=contr)

null1=glmer(any_peek~ z.Trialnumb + (1+Condition + z.Trialnumb| ID), family = binomial, control=contr)

anova(null1,GLM1, test="Chisq")
drop1(GLM1, test="Chisq", control=contr)
summary(GLM1)

#GLM1b (NOT IN PRE REG) effect of occluding on just below peeks#

GLM1b=glmer(peek_below~ Condition + z.Trialnumb  + (1+Condition + z.Trialnumb| ID), family = binomial,control=contr)

null1b=glmer(peek_below~ z.Trialnumb + (1+Condition + z.Trialnumb| ID), family = binomial, control=contr)

anova(null1b,GLM1b, test="Chisq")
drop1(GLM1b, test="Chisq", control=contr)
summary(GLM1b)

#GLM1c (NOT IN PRE REG) effect of occluding on just above peeks#

GLM1c=glmer(peek_above~ Condition + z.Trialnumb  + (1+Condition + z.Trialnumb| ID), family = binomial,control=contr)

null1c=glmer(peek_above~ z.Trialnumb + (1+Condition + z.Trialnumb| ID), family = binomial, control=contr)

anova(null1c,GLM1c, test="Chisq")
drop1(GLM1c, test="Chisq", control=contr)
summary(GLM1c)

#-----------------------------------------#

#GLMM2a: To analyse the effects of baiting condition (within the occluded condition only) on participants looking above the occluder#
    
occlude=subset(Lidsdata,Condition=="Occluded")
detach(Lidsdata)
attach(occlude)

z.Trialnumb=as.vector(scale(trial_number))##CV:after subsetting the data you need to do the scaling again.

GLM2a=glmer(peek_above ~ Configuration #(all baited/all open/ mixed)
            + z.Trialnumb + (1|ID), family =binomial, control=contr)

null2a= glmer(peek_above ~ z.Trialnumb + (1|ID), family =binomial, control=contr)

anova(null2a,GLM2a, test="Chisq")
drop1(GLM2a, test="Chisq", control=contr)
summary(GLM2a)

#GLMM2b: To analyse the effects of condition (within the test phase only) on participants looking below the occluder / double-cylinders:
 
GLM2b=glmer(peek_below ~ Configuration + z.Trialnumb + (1|ID), family =binomial, control=contr)

null2b= glmer(peek_below ~ z.Trialnumb + (1+Configuration + z.Trialnumb |ID), family =binomial, control=contr)

anova(null2b,GLM2b, test="Chisq")
drop1(GLM2b, test="Chisq", control=contr)
summary(GLM2b)    
  
#GLMM2c: To analyse the effects of configuration (within the occluded condition only) on participants looking either above or below the occluder#
 
GLM2c=glmer(any_peek ~ Configuration + z.Trialnumb + (1|ID), family =binomial, control=contr)

null2c= glmer(any_peek ~ z.Trialnumb + (1 |ID), family =binomial, control=contr)

anova(null2c,GLM2c, test="Chisq")
drop1(GLM2c, test="Chisq", control=contr)
summary(GLM2c)   

#--------------------------------------------#

#GLMM3: To analyse the effects of condition and configuratiob on participants' cup choice.

detach(occlude)

attach(Lidsdata)

GLMM3=glmer(correct~Condition* Configuration + z.Trialnumb + (1|ID) + (0+Condition*Configuration |ID) + (0 + z.Trialnumb| ID), family = binomial, control=contr)

null3=glmer(correct~z.Trialnumb + (1|ID) + (0+Condition*Configuration |ID) + (0 + z.Trialnumb| ID), family = binomial, control=contr)

anova(null3,GLMM3, test="Chisq")
drop1(GLMM3, test="Chisq", control=contr)
summary(GLM3)   
