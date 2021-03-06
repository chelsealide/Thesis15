


require(ggplot2)



### Load data for Experiment 1 ----


raw.data <- read.csv("Experiment1Data.csv")

# Kick out excluded participants 

mydata <- raw.data[raw.data$Include == 1,]

# Data visualization 

subset.null <- NULL
subset.null$Condition = as.factor(mydata$Condition)
subset.null$Language = as.factor(mydata$Lang.Condition)
subset.null$Hands = as.factor(mydata$Action.Condition)
subset.null$Response = as.factor(ifelse(mydata$Response == mydata$Response[1],0,1))




# The last line recodes the responses as 0s and 1s -- basically: 
# "If the response is the same as the first line (which was a hand touch), give it a 0, else give it a 1"

# Convert to dataframe 

Exp.1 <- as.data.frame(subset.null)


### Graph ----



Exp.1$Response <- factor(Exp.1$Response, levels = rev(levels(Exp.1$Response)))

p <- ggplot(data=Exp.1, aes(x=Condition, fill=Response)) + geom_bar() + theme_light() + 
  ylab("Count") + theme(axis.title.y=element_text(size = 13, face = "bold")) + scale_y_continuous(breaks=seq(0, 10, 2)) + labs(fill="Response Type") + ggtitle("First Response Action") + 
  theme(plot.title=element_text(vjust=1.5, face="bold")) + xlab("\n Condition") +theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) 
bp <- p + scale_x_discrete(limits=c("Dax to toy, Hands Exposed","Dax toy, Hands Exposed","Dax to toy, Hands Occupied", "Dax toy, Hands Occupied"), 
                           labels = c("Hands Exposed \n Manner Language","Hands Exposed \n Outcome Language","Hands Occupied \n Manner Language", "Hands Occupied \n Outcome Language"))
bp + scale_fill_manual(values=c("deepskyblue3", "chartreuse3"), 
                       breaks=c("1", "0"),
                       labels=c("Head Touch", "Hand Touch")) 


p <- ggplot(data=Exp.1, aes(x=Condition, fill=Response)) + geom_bar(position="fill") + theme_light() + 
  ylab("Proportion of Responses \n") + theme(axis.title.y=element_text(size = 13, face = "bold")) + labs(fill="Response Type") + ggtitle("First Response Action") + 
  theme(plot.title=element_text(vjust=1.5, face="bold")) + xlab("\n Condition") +theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) 
bp <- p + scale_x_discrete(limits=c("Dax to toy, Hands Exposed","Dax toy, Hands Exposed","Dax to toy, Hands Occupied", "Dax toy, Hands Occupied"), 
                           labels = c("Hands Exposed \n Manner Language","Hands Exposed \n Outcome Language","Hands Occupied \n Manner Language", "Hands Occupied \n Outcome Language"))
bp + scale_fill_manual(values=c("deepskyblue3", "chartreuse3"), 
                       breaks=c("1", "0"),
                       labels=c("Head Touch", "Hand Touch")) 

### Location of First Response (Globe, Button, Neither)

p <- ggplot(data=mydata, aes(x=Response.Location)) + geom_bar(stat="bin") + theme_light() +
  ylab("Count \n") + theme(axis.title.y=element_text(size = 13, face = "bold")) + scale_y_continuous(breaks=seq(0, 14, 2)) +
  ggtitle("Location of First Response Imitation") + theme(plot.title=element_text(vjust=1.5, face="bold")) + 
  xlab("\n Location") + theme(axis.title.x=element_text(size = 13, face = "bold"))
p + scale_x_discrete(limits=c("Globe", "Button", "Box")) 


 