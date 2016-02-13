


### Load data for Experiment 1 ----


raw.data <- read.csv("Experiment1Data.csv")

# Kick out excluded participants 

mydata <- raw.data[raw.data$Include == 1,]

# Data visualization 

subset.null <- NULL
subset.null$Condition = as.factor(mydata$Condition)
subset.null$Language = as.factor(mydata$Lang.Condition)
subset.null$Hands = as.factor(mydata$Action.Condition)
subset.null$Response = as.factor(ifelse(mydata$Response..First.Action. == mydata$Response..First.Action.[1],0,1))

# The last line recodes the responses as 0s and 1s -- basically: 
# "If the response is the same as the first line (which was a hand touch), give it a 0, else give it a 1"

# Convert to dataframe 

Exp.1 <- as.data.frame(subset.null)


### Graph ----

require(ggplot2)

Exp.1$Response <- factor(Exp.1$Response, levels = rev(levels(Exp.1$Response)))
p <- ggplot(data=Exp.1, aes(x=Condition, fill=Response)) + geom_bar(position="fill") + ylab("Proportion of Responses") + 
  labs(fill="Response") + ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) 
p + scale_fill_manual(values=c("deepskyblue3","chartreuse3"), 
                      breaks=c("1", "0"),
                      labels=c("Head Touch", "Hand Touch"))
