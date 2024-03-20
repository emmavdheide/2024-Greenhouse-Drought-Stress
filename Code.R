#Example data analysis, greenhouse drought stress, cress edition

#set working directory

#load data
dat<-read.csv("March 8 cress data example.csv")

#Make rep and treatment number into factors
dat$Rep<-as.factor(dat$Rep)
dat$TrtNo<-as.factor(dat$TrtNo)

#Simple data visualization
plot(dat$LLL)
hist(dat$LLL)

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
fitLLL<-lm(LLL~TrtNameT1, data = dat)

#Let's view a summary of the fitted model
summary(fitLLL)
#The p values here indicate that there are some significant differences

#Now, we need to evaluate whether the assumptions of linear regression are satisfied
#We can do this visually
plot(fitLLL)
#These look pretty good

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
  ylim(0, 12)+
  ggtitle("Longest Leaf Length (cm) on March 8th")+
  geom_errorbar(aes(x=emmeans.TrtNameT1, ymin=emmeans.emmean-emmeans.SE, ymax=emmeans.emmean+emmeans.SE), width = 0.4, colour="gray10", alpha = 0.9, linewidth = 1.3)
LLLplot  
  
