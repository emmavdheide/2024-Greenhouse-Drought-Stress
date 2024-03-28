#Example data analysis, greenhouse drought stress, cress edition

#set working directory

#load data
dat<-read.csv("March 25 cress.csv")

#Make rep and treatment number into factors
dat$Rep<-as.factor(dat$Rep)
dat$TrtNo<-as.factor(dat$TrtNo)

#Simple data visualization
plot(dat$LLL)
hist(dat$LLL)
plot(dat$NoLeaves)
hist(dat$NoLeaves)

#We're interested in how plant size relates to drought stress treatment
#So we want to fit a model with, for example, longest leaf length (LLL) as response variable and drought stress treatment as predictor
  #We could fit a very similar model for plant height (Ht) or # of leaves (NoLeaves)
  #Because this data is from March 8th, when we haven't yet had T2 (late stress), I created a variable called TrtNameT1 to increase sample size

#The assumptions of linear regression (which we will evaluate after fitting the model):
  #1. Data is normally distributed
  #2. Data are linear
  #3. Data have constant variance around the mean
  #4. Independence of data points/observations
  #5. Random/representative sample

#Fitting the model - we do not need any additional packages for this step
#We're using LLL as a response variable, and treatment and germination date as predictors
fitLLL<-lm(LLL~TrtNameT1+GermDate, data = dat)

#Let's view a summary of the fitted model
summary(fitLLL)
#The p values here indicate that there are some significant differences

#Now, we need to evaluate whether the assumptions of linear regression are satisfied
#We can do this visually
plot(fitLLL)
#These look pretty good

#We can also use the package DHARMa, which will conduct some tests
library(DHARMa)
sim.fitLLL<-simulateResiduals(fitLLL)
plot(sim.fitLLL)
#Typically, we just look at the plot on the left - nothing is in red and the points adhere to the 1:1 line, so I'm happy with this

#So, which treatments are different?

#We can use pairwise comparisons to figure this out

#I like the package emmeans
#install.packages("emmeans")
library(emmeans)

emmLLL<-emmeans(fitLLL, specs = pairwise~TrtNameT1, infer=T, level=.95)
#this tells R to calculate the estimated marginal means (a fancy type of average) for the model we fitted
#and to calculate pairwise comparisons using TrtNameT1
#infer=T has R add confidence intervals to the output
#level=0.95 tells R what level of significant to use
emmLLL

#Now, let's visualize these results

#ggplot2 is a good graphics package
#install.packages("ggplot2")
library(ggplot2)

#First, we need to save the emmeans output
LLLforChart<-as.data.frame(emmLLL[1])

#I like to create an object like this to make R plot in the order I want it to
plotorder<-c("Control", "SE", "LE", "SL", "LL")

LLLplot<-ggplot(LLLforChart, aes(y=emmeans.emmean, x=emmeans.TrtNameT1, plotorder))+
  geom_bar(stat="identity")+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Longest Leaf Length (cm)")+
  ylim(0, 20)+
  ggtitle("Longest Leaf Length (cm) on March 25th")+
  geom_errorbar(aes(x=emmeans.TrtNameT1, ymin=emmeans.emmean-emmeans.SE, ymax=emmeans.emmean+emmeans.SE), width = 0.4, colour="gray10", alpha = 0.9, linewidth = 1.3)
LLLplot  
  



#For Number of Leaves
#Fitting the model - we do not need any additional packages for this step
fitNL<-lm(NoLeaves~TrtNameT1+GermDate, data = dat)

#Let's view a summary of the fitted model
summary(fitNL)
#The p values here indicate that there are some significant differences

#Now, we need to evaluate whether the assumptions of linear regression are satisfied
#We can do this visually
plot(fitNL)
#These look pretty good

#Using the package DHARMa:
sim.fitNL<-simulateResiduals(fitNL)
plot(sim.fitNL)
#There's a bit of red text and a small deviation from the 1:1 line
  #We could trouble-shoot this by changing the regression family, but I'm happy enough with this for now

#So, which treatments are different?

#We can use pairwise comparisons to figure this out

#I like the package emmeans
#install.packages("emmeans")
library(emmeans)

emmNL<-emmeans(fitNL, specs = pairwise~TrtNameT1, infer=T, level=.95)
#this tells R to calculate the estimated marginal means (a fancy type of average) for the model we fitted
#and to calculate pairwise comparisons using TrtNameT1
#infer=T has R add confidence intervals to the output
#level=0.95 tells R what level of significant to use
emmNL

#Now, let's visualize these results

#ggplot2 is a good graphics package
#install.packages("ggplot2")
library(ggplot2)

#First, we need to save the emmeans output
NLforChart<-as.data.frame(emmNL[1])

#I like to create an object like this to make R plot in the order I want it to
plotorder<-c("Control", "SE", "LE", "SL", "LL")

NLplot<-ggplot(NLforChart, aes(y=emmeans.emmean, x=emmeans.TrtNameT1, plotorder))+
  geom_bar(stat="identity")+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Number of True Leaves")+
  ylim(0, 20)+
  ggtitle("Number of True Leaves on March 25th")+
  geom_errorbar(aes(x=emmeans.TrtNameT1, ymin=emmeans.emmean-emmeans.SE, ymax=emmeans.emmean+emmeans.SE), width = 0.4, colour="gray10", alpha = 0.9, linewidth = 1.3)
NLplot  

