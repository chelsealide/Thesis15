## Chelsea Lide
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016


# ! This code is ONLY descriptives/stats for the exploration period in Experiment 2 !


# Workflow ------
# (1) For each participant, sum the total number of responses and divide it by each desired response (head or hand)
    # Attach this column to data frame
# (2) Subset these proportions by condition 
# (3) Take each condition's average (sum of proportions / # of proportions)
# (4) Graph! 


# Load in the relevant data sets (MUST SOURCE THROUGH DESCRIPTIVES FIRST) ----


# Exploration Data 

# Add in response counts 

a <- subset(alldata, TrackName == "Head Touch")  
unique.head.counts <- subset(a, !duplicated(a[1]))
Head.Touches <- ddply(a,.(Participant),nrow)

Head.Touches
unique.head.counts

# Add column with head touch counts (V1)

for (i in 1:n) {
  unique.head.counts$Head.Touches <- Head.Touches
}


## Now for hand touches

b <- subset(alldata, TrackName == "Hand Touch")
unique.hand.counts <- subset(b, !duplicated(b[1]))
Hand.Touches <- ddply(b,.(Participant), nrow)

Hand.Touches
unique.hand.counts

for (i in 1:n) {
  unique.hand.counts$Hand.Touches <- Hand.Touches
}

# Unique Records of Participants, and what condition they're in 

raw.unique.participants <- subset(alldata, !duplicated(alldata[1]))
raw.unique.participants2 <- subset(raw.unique.participants[1:4])
head(raw.unique.participants2)

# Merge HEAD touch counts based on row number & code "NA" as 0 

x <- join(raw.unique.participants2, unique.head.counts, 
          by = c("Participant", "Condition", "Hands", "Language"), type = "left")
x$Head.Touches$V1[is.na(x$Head.Touches$V1)] <- 0
merged.head.touches <- cbind(x[1:4], x[9])
merged.head.touches

# Merge HAND touch counts based on row number & code "NA" as 0 

y <- join(raw.unique.participants2, unique.hand.counts, 
          by = c("Participant", "Condition", "Hands", "Language"), type = "left")
y$Hand.Touches$V1[is.na(y$Hand.Touches$V1)] <- 0
merged.hand.touches <- cbind(y[1:4], y[9])
merged.hand.touches

# Merge ALL 

clean.exploration <- cbind(raw.unique.participants2, merged.head.touches$Head.Touches$V1, merged.hand.touches$Hand.Touches$V1) 
colnames(clean.exploration) = c("Participant", "Condition", "Hands", "Language", "Head Touches", "Hand Touches")
clean.exploration 

# kick out any participants who did not explore ----

clean.exploration <- clean.exploration[clean.exploration$Total.Responses > 0,]

### HEAD TOUCHES ---------------

# (1) 

# create column of total # of responses, add it to 'clean.exploration'

Total.Responses <- as.matrix(rowSums(clean.exploration[, c(5,6)]))

clean.exploration <- cbind(clean.exploration, Total.Responses)


# create vector of proportions, turn them into percentages, name the column, add it to 'clean.exploration'
 
Prop.Head <- (clean.exploration[5]/clean.exploration[7])
    Prop.Head <- 100*(round(Prop.Head, digits = 4))
    colnames(Prop.Head) <- "Percent Head Touch"
clean.exploration <- cbind(clean.exploration, Prop.Head)  
attach(clean.exploration) 

# (2) 

# subset percent of Head Touches by condition

prop.head.1 <- subset(clean.exploration, Condition == "Exposed, Dax To", c("Participant", "Condition", "Percent Head Touch"))
prop.head.2 <- subset(clean.exploration, Condition == "Exposed, Dax", c("Participant", "Condition", "Percent Head Touch"))
prop.head.3 <- subset(clean.exploration, Condition == "Occupied, Dax To", c("Participant", "Condition", "Percent Head Touch"))
prop.head.4 <- subset(clean.exploration, Condition == "Occupied, Dax", c("Participant", "Condition", "Percent Head Touch"))

# (3)

# average per condition 

mean.head.1 <- colSums(prop.head.1[3])/(dim(prop.head.1)[1])
mean.head.2 <- colSums(prop.head.2[3])/(dim(prop.head.2)[1])
mean.head.3 <- colSums(prop.head.3[3])/(dim(prop.head.3)[1])
mean.head.4 <- colSums(prop.head.4[3])/(dim(prop.head.4)[1])

# (4)

# graph! 

require(ggplot2)

# make a data frame 

head.dat <- data.frame(
  condition = factor(c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")),
  percent = c(mean.head.1, mean.head.2, mean.head.3, mean.head.4))

ggplot(data = head.dat, aes(x = condition, y = percent)) +
  geom_bar(stat="identity") +
  xlab("Condition") + ylab("Percent") + 
  ggtitle("Percentage of Head Touch Responses Per Condition")

detach(clean.exploration)

### HAND TOUCHES -------------

# (1) 

# total # of responses column already created, so...

# create vector of proportions, turn them into percentages, name the column, add it to 'clean.exploration'

Prop.Hand <- (clean.exploration[6]/clean.exploration[7])
  Prop.Hand <- 100*(round(Prop.Hand, digits = 4))
  colnames(Prop.Hand) <- "Percent Hand Touch"
clean.exploration <- cbind(clean.exploration, Prop.Hand)  
attach(clean.exploration) 

# (2) 

# subset percent of Head Touches by condition

prop.hand.1 <- subset(clean.exploration, Condition == "Exposed, Dax To", c("Participant", "Condition", "Percent Hand Touch"))
prop.hand.2 <- subset(clean.exploration, Condition == "Exposed, Dax", c("Participant", "Condition", "Percent Hand Touch"))
prop.hand.3 <- subset(clean.exploration, Condition == "Occupied, Dax To", c("Participant", "Condition", "Percent Hand Touch"))
prop.hand.4 <- subset(clean.exploration, Condition == "Occupied, Dax", c("Participant", "Condition", "Percent Hand Touch"))

# (3)

# average per condition 

mean.hand.1 <- colSums(prop.hand.1[3])/(dim(prop.hand.1)[1])
mean.hand.2 <- colSums(prop.hand.2[3])/(dim(prop.hand.2)[1])
mean.hand.3 <- colSums(prop.hand.3[3])/(dim(prop.hand.3)[1])
mean.hand.4 <- colSums(prop.hand.4[3])/(dim(prop.hand.4)[1])

# (4)

# graph! 


# make a data frame 

hand.dat <- data.frame(
  condition = factor(c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")),
  percent = c(mean.hand.1, mean.hand.2, mean.hand.3, mean.hand.4))

ggplot(data = hand.dat, aes(x = condition, y = percent)) +
  geom_bar(stat="identity") +
  xlab("Condition") + ylab("Percent") + 
  ggtitle("Percentage of Hand Touch Responses Per Condition")

### GRAPHING -----------------

require(reshape2)

# make a data frame with condition & head/hand touch percentages & order levels

responsedat <- cbind(head.dat, hand.dat[2])
      colnames(responsedat) <- c("Condition", "Head Touches", "Hand Touches") 
  responsedat$Condition <- factor(responsedat$Condition, 
                                levels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"), 
                                labels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

# melt the data frame for plotting
  
data.m <- melt(responsedat, id.vars='Condition')

# plot everything

    # Stacked -----

plot <- ggplot(data.m, aes(Condition, value)) +   
  geom_bar(aes(fill = variable), position = "stack", stat="identity") +
  xlab("Condition") + ylab("Percent") + ggtitle("Percent of Response Type Per Condition") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("deepskyblue3", "chartreuse3")) 
plot + guides(fill=guide_legend(title=NULL))

    # Grouped -----

plot <- ggplot(data.m, aes(Condition, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  xlab("Condition") + ylab("Percent") + ggtitle("Percent of Response Type Per Condition") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("deepskyblue3", "chartreuse3")) 
plot + guides(fill=guide_legend(title=NULL))





