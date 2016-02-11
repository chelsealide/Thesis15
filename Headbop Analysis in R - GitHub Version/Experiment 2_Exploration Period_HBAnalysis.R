## Chelsea Lide
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016

require(plyr)
require(ggplot2)
require(reshape2)
require(reshape)
require(gridExtra)

# ! This code is ONLY descriptives/stats for the exploration period in Experiment 2 !


# Workflow ------
# (1) For each participant, sum the total number of responses and divide it by each desired response (head or hand)
    # Attach this column to data frame
# (2) Subset these proportions by condition 
# (3) Take each condition's average (sum of proportions / # of proportions)
# (4) Graph! 



#Read in and merge individual data sets ----

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

alldata <- load_data("Headbop Coded CSVs")

head(alldata)

# Heavy-duty data cleaning ----

# Add in response counts 

a <- subset(alldata, TrackName == "Head Touch")  
unique.head.counts <- subset(a, !duplicated(a[1]))
Head.Touches <- ddply(a,.(Participant),nrow)

Head.Touches
unique.head.counts

# Add column with head touch counts (V1)

n <- dim(unique.head.counts)[1]

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

# create column of total # of responses, add it to 'clean.exploration'

Total.Responses <- as.matrix(rowSums(clean.exploration[, c(5,6)]))

clean.exploration <- cbind(clean.exploration, Total.Responses)

# kick out any participants who did not explore 

clean.exploration <- clean.exploration[clean.exploration$Total.Responses > 0,]

attach(clean.exploration)

### Distribution of Response Types ----

# Head Touches per condition 

ggplot(clean.exploration, aes(x=`Head Touches`, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Experimental\nCondition",
                        breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                        labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

# Hand Touches per condition 

ggplot(clean.exploration, aes(x=`Hand Touches`, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))


                      
### Model Fitting (Head Touches) ----

temp.data <- clean.exploration[clean.exploration$`Head Touches` > 0,]   # To get rid of non-positive values (which cause error term for gamma dist)

null.fit <- glm(`Head Touches` ~ Hands, family = Gamma, data = temp.data)

fit.1 <- glm(`Head Touches` ~ Hands + Language, family = Gamma, data = temp.data)

fit.2 <- glm(`Head Touches`~ Hands*Language, family = Gamma, data = temp.data)

head.comparison <- anova(null.fit,fit.1, fit.2)
head.comparison 

### Model Fitting (Hand Touches) ----

fit.null <- glm(`Hand Touches` ~ Hands, family = Gamma, data = temp.data)

fit.a <- glm(`Hand Touches` ~ Hands + Language, family = Gamma, data = temp.data)

fit.a <- glm(`Hand Touches`~ Hands*Language, family = Gamma, data = temp.data)

hand.comparison <- anova(fit.null,fit.a, fit.b)
hand.comparison 

### Fit 1 ( Hands + Lang ) predicts BOTH head (p = .05) and hand (p = .01) touch responses significanly better than the null fit ----


### Exploration Language Effects ---- 


op <- par(mfrow = c(1,2))
boxplot(`Head Touches`~Language, data = clean.exploration)
boxplot(`Hand Touches`~Language, data = clean.exploration)
par(op)

# T Tests...

q <- subset(clean.exploration, `Head Touches` > 0, c("Language", "Head Touches"))
z <- subset(clean.exploration, `Hand Touches` > 0, c("Language", "Hand Touches"))
t.test(`Head Touches`~Language, data = q)
t.test(`Hand Touches`~Language, data = z)

pairs(clean.exploration)




### Below = proportions --------------------------------------------------------------------------------------------









### HEAD TOUCHES ---------------


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
  head.dat$condition <- factor(head.dat$condition, 
                                levels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"), 
                                labels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))


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
  hand.dat$condition <- factor(hand.dat$condition, 
                             levels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"), 
                             labels = c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))


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

    # Stacked

plot <- ggplot(data.m, aes(Condition, value)) +   
  geom_bar(aes(fill = variable), position = "stack", stat="identity") +
  xlab("Condition") + ylab("Percent") + ggtitle("Percent of Response Type Per Condition") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("deepskyblue3", "chartreuse3")) 
plot + guides(fill=guide_legend(title=NULL))

    # Grouped 

plot <- ggplot(data.m, aes(Condition, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  xlab("Condition") + ylab("Percent") + ggtitle("Percent of Response Type Per Condition") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("deepskyblue3", "chartreuse3")) 
plot + guides(fill=guide_legend(title=NULL))

### CHI SQUARED TEST -----

# re-arranging data to make contingency table

clean.exploration

# create condition labels vector 

Conditions <- c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax") 

# subset head & hand responses by condition 

C1 <- subset(clean.exploration, Condition == "Exposed, Dax To", select = c("Participant", "Condition", "Head Touches", "Hand Touches"))
C2 <- subset(clean.exploration, Condition == "Exposed, Dax", select = c("Participant", "Condition", "Head Touches", "Hand Touches"))
C3 <- subset(clean.exploration, Condition == "Occupied, Dax To", select = c("Participant", "Condition", "Head Touches", "Hand Touches"))
C4 <- subset(clean.exploration, Condition == "Occupied, Dax", select = c("Participant", "Condition", "Head Touches", "Hand Touches"))

# sum head responses per condition 

C1.head.sum <- as.vector(colSums(C1[3]))
C2.head.sum <- as.vector(colSums(C2[3]))
C3.head.sum <- as.vector(colSums(C3[3]))
C4.head.sum <- as.vector(colSums(C4[3]))

head.sums <- as.integer(c(C1.head.sum, C2.head.sum, C3.head.sum, C4.head.sum))
head.temp <- as.data.frame(head.sums, Conditions)

# sum hand responses per condition 

C1.hand.sum <- as.vector(colSums(C1[4]))
C2.hand.sum <- as.vector(colSums(C2[4]))
C3.hand.sum <- as.vector(colSums(C3[4]))
C4.hand.sum <- as.vector(colSums(C4[4]))

hand.sums <- as.integer(c(C1.hand.sum, C2.hand.sum, C3.hand.sum, C4.hand.sum))
hand.temp <- as.data.frame(hand.sums, Conditions)



# merge hand sums, head sums and conditions

raw.contingency <- cbind(head.temp, hand.temp$hand.sums)
colnames(raw.contingency) <- c("Head Touches", "Hand Touches")
contingency.table <- raw.contingency

contingency.table


### chi-squared = 1.7065, df = 3, p-value = 0.64 
# cannot reject the null that response type is independent of condition ----

chisq.test(contingency.table)




#### CONTINGENCY TABLE (ever head touched vs. never)


clean.exploration$Ever.Head <- as.integer(ifelse(clean.exploration$"Head Touches" > 0, 1,0))
clean.exploration

raw.cont <- NULL
raw.cont$Condition <- as.factor(clean.exploration$Condition)
raw.cont$Ever <- as.integer(clean.exploration$Ever.Head)
raw.cont$Never <- as.integer(ifelse(clean.exploration$Ever.Head == clean.exploration$Ever.Head[1], 0,1))
raw.cont <- as.data.frame(raw.cont)

C1 <- subset(raw.cont, Condition == "Exposed, Dax To", select = c("Ever", "Never"))
C2 <- subset(raw.cont, Condition == "Exposed, Dax", select = c("Ever", "Never"))
C3 <- subset(raw.cont, Condition == "Occupied, Dax To", select = c("Ever", "Never"))
C4 <- subset(raw.cont, Condition == "Occupied, Dax", select = c("Ever", "Never"))

C1.ever.sum <- as.vector(colSums(C1[1]))
C2.ever.sum <- as.vector(colSums(C2[1]))
C3.ever.sum <- as.vector(colSums(C3[1]))
C4.ever.sum <- as.vector(colSums(C4[1]))

C1.never.sum <- as.vector(colSums(C1[2]))
C2.never.sum <- as.vector(colSums(C2[2]))
C3.never.sum <- as.vector(colSums(C3[2]))
C4.never.sum <- as.vector(colSums(C4[2]))

cont.table <- NULL 
cont.table$Ever <- as.integer(c(C1.ever.sum, C2.ever.sum, C3.ever.sum, C4.ever.sum))
cont.table$Never <- as.integer(c(C1.never.sum, C2.never.sum, C3.never.sum, C4.never.sum))
cont.table <- as.data.frame(cont.table)
rownames(cont.table) <- c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")
cont.table 

## Statistic ----

exploration.chi <- chisq.test(cont.table)

### chi-squared = .170, df = 3, p-value = 0.98 
# cannot reject the null that response type is independent of condition ----



### Percentage of Head Touches (ever) per condition (like Schwier et al., 2006) ---- 

# start with clean.exploration & binary column of Ever.Head

# subset condition (as factor) and Ever.Head

Ever.Head1 <- subset(clean.exploration, Condition == "Exposed, Dax To", c("Participant", "Condition", "Ever.Head"))
Ever.Head2 <- subset(clean.exploration, Condition == "Exposed, Dax", c("Participant", "Condition", "Ever.Head"))
Ever.Head3 <- subset(clean.exploration, Condition == "Occupied, Dax To", c("Participant", "Condition", "Ever.Head"))
Ever.Head4 <- subset(clean.exploration, Condition == "Occupied, Dax", c("Participant", "Condition", "Ever.Head"))

C1mean <- colMeans(Ever.Head1[3])
C2mean <- colMeans(Ever.Head2[3])
C3mean <- colMeans(Ever.Head3[3])
C4mean <- colMeans(Ever.Head4[3])

ever.head.plot <- NULL 
ever.head.plot$Condition <- as.factor(c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
ever.head.plot$'Mean Head Touch' <- c(C1mean, C2mean, C3mean, C4mean)  
ever.head.plot <- as.data.frame(ever.head.plot)
ever.head.plot
  
  
  
  





