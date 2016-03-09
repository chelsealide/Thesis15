## Chelsea Lide
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016


# ! This code is ONLY descriptives/stats for the first response in Experiment 2 !

require(ggplot2)
require(plyr)
require(lme4)
require(nlme)

# Load in data set & kick out excluded participants ----

raw.data <- read.csv("Experiment2Data.csv")

mydata <- raw.data[raw.data$Include == 1,]
mydata <- raw.data[!is.na(raw.data$Response..First.Action.),]

# Subset the predictors (condition, language, hands) and response variable (first action)

subset.null <- NULL
subset.null$Participant = mydata$Participant..
subset.null$Condition = as.factor(mydata$Condition)
subset.null$Language = as.factor(mydata$Lang.Condition)
subset.null$Hands = as.factor(mydata$Action.Condition)
subset.null$Response = as.factor(mydata$Response..First.Action.)


# The last line recodes the responses as 0s and 1s -- basically: 
# "If the response is the same as the first line (which was a hand touch), give it a 0, else give it a 1"

# Convert to dataframe 

Exp.2 <- as.data.frame(subset.null)

### Graph ----


Exp.2$Response <- factor(Exp.2$Response, levels = rev(levels(Exp.2$Response)))
p <- ggplot(data=Exp.2, aes(x=Condition, fill=Response)) + geom_bar() + theme_light() + 
  ylab("Count") + theme(axis.title.y=element_text(size = 13, face = "bold")) + scale_y_continuous(breaks=seq(0, 20, 2)) + labs(fill="Response Type") + ggtitle("First Response Action") + 
  theme(plot.title=element_text(vjust=1.5, face="bold")) + xlab("\n Condition") +theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) 
bp <- p + scale_x_discrete(limits=c("Dax to toy, Hands Exposed","Dax toy, Hands Exposed","Dax to toy, Hands Occupied", "Dax toy, Hands Occupied"), 
                           labels = c("Hands Exposed \n Manner Language","Hands Exposed \n Outcome Language","Hands Occupied \n Manner Language", "Hands Occupied \n Outcome Language"))
bp + scale_fill_manual(values=c("deepskyblue3", "chartreuse3"), 
                       breaks=c("Head", "Hand"),
                       labels=c("Head Touch", "Hand Touch")) 
  
  

### Fitting Models & Checking Effects by Removal ----
  
  
response.model <- lmer(Response ~ Hands*Language + (1|Participant), data=Exp.2, family="binomial")
  
  # No interaction 
  
    model2 <- lmer(Response ~ Hands + Language + (1|Participant), data=Exp.2, family="binomial")
    anova(response.model, model2)
  
  ### chisq = 0.09, df = 5, p = .76
  
  
  # No Hands 
  
    model3 <- lmer(Response ~ Language + (1|Participant), data=Exp.2, family="binomial")
    anova(model2, model3)
  
  ### chisq = 1.92, df = 4, p = 0.17
  
  
  # No Langauge 
  
    model4 <- lmer(Response ~ Hands + (1|Participant), data=Exp.2, family="binomial")
    anova(model2, model4)
  
  ### chisq = 2.62, df = 4, p = 0.11
 
  
