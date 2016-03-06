### Exploratory Descriptives for both Experiment 1 and Experiment 2 


require(plyr)
require(ggplot2)
require(reshape2)
require(reshape)
require(corrplot)



### Load data for Experiment 1 ----


raw.data <- read.csv("Experiment1Data.csv")

# Kick out excluded participants 

mydata <- raw.data[raw.data$Inclusion == 1,]
mydata$Months.Old <- I(mydata$Days.Old/30)
attach(mydata)


# Data visualization ----

  # Age & Gender 

  p <- ggplot(mydata, aes(factor(Gender), Months.Old, xlab = "Gender", ylab = "Age (days)")) 
  p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
        labs(x = "Gender", 
        y = "Age (months)",
        title = "Age Differences by Gender")
  
  # Vocab & Gender 

  p <- ggplot(mydata, aes(factor(Gender), Raw.MCDI..Harvard.Only.), xlab = "Gender", ylab = "Score")  
  p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
        labs(x = "Gender", 
         y = "Vocabulary Score",
         title = "Vocabulary Differences by Gender")
  
 # Vocab & Age
  
  ggplot(mydata, aes(x=Days.Old, y=Raw.MCDI..Harvard.Only., color=Condition)) + geom_point(size=5) + geom_smooth(method=lm, se=FALSE) + xlab("Age (days)") + ylab("MCDI Score") + 
    ggtitle("Age and Vocabulary Score by Condition") + theme(plot.title = element_text(lineheight=1.5, face="bold"))
  
  # Gender & Condition
  
  ggplot(data=mydata, aes(x=Condition, fill=Gender)) +
    geom_bar(stat="bin") + scale_fill_manual(values=c("lightcoral", "darkslategray3")) + ggtitle("Gender Distribution per Condition") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) 
    
  
  # Age & Condition
  
  #(density) 
  ggplot(mydata, aes(x=Days.Old, fill=Condition)) + geom_density(alpha = .5) 
  
  #(boxplot)
  p <- ggplot(mydata, aes(factor(Condition), Days.Old)) 
  p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs(x = "Condition", 
         y = "Age (days)",
         title = "Age Distribution by Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  # Vocab & Condition
  
  #(density)
  ggplot(mydata, aes(x=Raw.MCDI..Harvard.Only., fill=Condition)) + geom_density(alpha = .5)
  
  #(boxplot)
  p <- ggplot(mydata, aes(factor(Condition), Raw.MCDI..Harvard.Only.)) 
  p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs(x = "Condition", 
         y = "Score",
         title = "Vocabulary Distribution by Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1))
  


# Stats ----
  
  # Per age ...
  
        # vocab (r^2)++++
        cor.test(Days.Old, Raw.MCDI..Harvard.Only.) # R^2= .44, df = 17, p = .05 (trending) 
  
  # Per Gender ...
  
        # age (t)
        t.test(Days.Old~Gender) # t = 0.10, df = 17.96, p = 0.91
  
        # vocab (t)
        t.test(Raw.MCDI..Harvard.Only.~Gender) # t = 0.56, df = 16.73, p = 0.58
  
  # Per Cond ...
  
        # gender (t) 
        
        # age (t)
        t.test(Days.Old~Action.Condition)  # t = -1.25, df = 11.93, p = 0.24
        t.test(Days.Old~Lang.Condition)    # t = 0.50, df = 13.91, p = 0.62
         
        # vocab (t)
        t.test(Raw.MCDI..Harvard.Only.~Action.Condition)  # t = -0.13, df = 10.88, p = 0.89
        t.test(Raw.MCDI..Harvard.Only.~Lang.Condition)    # t = -0.15, df = 14.90, p = 0.88
  
  
detach(mydata)

        
        
        
        
### Exp 1 & 2 Fuss&Fail Comparison ----
        
  # Exp 1 fuss outs = 0/20
  # Exp 1 fail to intract w toy = 5/20
  # Exp 2 fuss outs = 5/45
  # Exp 2 fail to intract w toy = 3/45
        
  ff <- read.csv("FussFail.csv")
  ff$Experiment <- as.factor(ff$Experiment)
  mylogit <- glm(Fuss.or.Fail~Experiment, data = ff, family = "binomial")
  summary(mylogit) 
  
  
  
  
### Load data for Experiment 2 ----

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

alldata <- load_data("Headbop Coded CSVs")

head(alldata)

# Heavy-duty data cleaning 

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


### Load in Descriptives CSV (created by hand in excel)

raw.des <- read.csv("Exp2Descriptives.csv")

### Combine descriptives and head/hand touch info based on Participant # 

merged.des <- merge(raw.des, clean.exploration, by = "Participant")

### Clean up round 1

merged.des$Subject <- NULL
merged.des$Condition.y <- NULL
merged.des$Hands.y <- NULL
merged.des$Language.y <- NULL
descriptives <- merged.des
  colnames(descriptives) <- c("Participant", "Condition", "Language", "Hands", "Gender", "Days.Old", "MCDI", "First Response", "Head.Touches", "Hand.Touches", "Total.Responses")

### Add in Duration info 
  
  total <- subset(alldata, TrackName == "Total Trial Length", c("Participant", "Duration"))
  total$Duration <- I(total$Duration/1000)
    colnames(total) <- c("Participant", "Total.Dur")
  warmup <- subset(alldata, TrackName == "Warm Up Duration", c("Participant", "Duration"))
  warmup$Duration <- I(warmup$Duration/1000)
    colnames(warmup) <- c("Participant", "Warmup.Dur")
  demo <- subset(alldata, TrackName == "Demonstration Duration", c("Participant", "Duration"))
  demo$Duration <- I(demo$Duration/1000)
    colnames(demo) <- c("Participant", "Demo.Dur")
  first <- subset(alldata, TrackName == "First Response Window", c("Participant", "Duration"))
  first$Duration <- I(first$Duration/1000)
    colnames(first) <- c("Participant", "FirstResponse.Dur")
  exploration <- subset(alldata, TrackName == "Exploration Duration", c("Participant", "Duration"))
  exploration$Duration <- I(exploration$Duration/1000)
    colnames(exploration) <- c("Participant", "Exploration.Dur")
  
descriptives <- merge(descriptives, total, by = "Participant")
descriptives <- merge(descriptives, warmup, by = "Participant")
descriptives <- merge(descriptives, demo, by = "Participant")
descriptives <- merge(descriptives, first, by = "Participant")
descriptives <- merge(descriptives, exploration, by = "Participant")


### Add Engagement & IBA info 

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

rawdata <- load_data("EIBA CSVs")
head(rawdata)


### Subsetting "Engagement" 

e <- subset(rawdata, TrackName == "Engagement", c("Participant", "Duration"))
e$Duration <- I(e$Duration/1000)
e

### Summing durations based on participant numbers

Engagement.Duration <- aggregate(. ~ Participant, data=e, FUN=sum)
colnames(Engagement.Duration) <- c("Participant", "Engagement.Duration")

### Subsetting "IBAs"

i <- subset(rawdata, TrackName == "IBA", c("Participant", "Duration"))
i$Duration <- I(i$Duration/1000)
i

IBA.Duration <- aggregate(. ~ Participant, data=i, FUN=sum)
colnames(IBA.Duration) <- c("Participant", "IBA.Duration")


descriptives <- merge(descriptives, Engagement.Duration, by = "Participant")
descriptives <- merge(descriptives, IBA.Duration, by = "Participant")

attach(descriptives)
  


### Experiment 2 Correlations ----

pairs(descriptives)

# Subsetting numeric variables for cor()

numeric.dat <- descriptives[6:17]

pairs(numeric.dat)

cor(numeric.dat)
  
### No apparent correlations, except in duration lengths ###
  

  


### Experiment 2 Distributions ----


# Durations by Condition

  # Total

p <- ggplot(descriptives, aes(factor(Condition), Total.Dur)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "Total Trial Length (s)",
       title = "Trial Duration by Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1))

  
  # Warmup 

p <- ggplot(descriptives, aes(factor(Condition), Warmup.Dur)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "Warm-Up Length (s)",
       title = "Warm-Up Duration by Condition") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(size=15, face="bold"))


  # Demonstration

p <- ggplot(descriptives, aes(factor(Condition), Demo.Dur)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "Demonstration Length (s)",
       title = "Demonstration Duration by Condition") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=15, face="bold"))


  # FirstResponse

p <- ggplot(descriptives, aes(factor(Condition), FirstResponse.Dur)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "First Response Window Length (s)",
       title = "First Response Window Duration by Condition") + 
       theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=15, face="bold"))

  # Exploration

p <- ggplot(descriptives, aes(factor(Condition), Exploration.Dur)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "Exploration Length (s)",
       title = "Exploration Duration by Condition") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=15, face="bold"))


  # Engagement

p <- ggplot(descriptives, aes(factor(Condition), Engagement.Duration)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "Engagement Length (s)",
       title = "Engagement Duration by Condition") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=15, face="bold"))

  # IBA

p <- ggplot(descriptives, aes(factor(Condition), IBA.Duration)) 
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Condition", 
       y = "IBA Length (s)",
       title = "IBA Duration by Condition") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=15, face="bold"))


### No real trends, other than that first response window tending to be shortest in Exposed, Dax To cond ###


### Head touches 

hist(descriptives$Head.Touches, breaks = 3)

  # Head touches by Gender 
  
  ggplot(descriptives, aes(x=Head.Touches)) + geom_histogram() + facet_grid(Gender ~ .)
    scale_fill_discrete(name="Participant Gender",
                        breaks=c("F", "M"),
                        labels=c("Girls", "Boys"))
    
  # Head touches by Hands 
  
  ggplot(descriptives, aes(x=Head.Touches, fill=Hands)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Hand Condition", labels=c("Exposed", "Occupied"), values = c("dodgerblue","limegreen")) 
  
  # Head touches by Language
  
  ggplot(descriptives, aes(x=Head.Touches, fill=Language)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Language Condition",
                        breaks=c("Dax To", "Dax"),
                        labels=c("Manner", "Outcome"), values = c("limegreen","dodgerblue"))

  
### Hand touches 
  
  hist(descriptives$Hand.Touches, breaks = 50)
  
  # Hand touches by Gender
  
  ggplot(descriptives, aes(x=Hand.Touches, fill=Gender)) + geom_density(alpha=.3) +
    scale_fill_discrete(name="Participant Gender", breaks = c("F", "M"), labels=c("Girls", "Boys"))
  
  # Hand touches by Hands 
  
  ggplot(descriptives, aes(x=Hand.Touches, fill=Hands)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Hand Condition", labels=c("Exposed", "Occupied"), values = c("dodgerblue","limegreen"))
  
  # Hand touches by Language
  
  ggplot(descriptives, aes(x=Hand.Touches, fill=Language)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Language Condition",
                        breaks=c("Dax To", "Dax"),
                        labels=c("Manner", "Outcome"), values = c("limegreen","dodgerblue"))
  
  
  
  # Total Responses by Condition 
  
  ggplot(descriptives, aes(x=Total.Responses, fill=Condition)) + geom_density(alpha=.3) +
    scale_fill_discrete(name="Condition",
                        breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                        labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
  
  # Total Responses by Hands 
  
  ggplot(descriptives, aes(x=Total.Responses, fill=Hands)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Hand Condition", labels=c("Exposed", "Occupied"), values = c("dodgerblue","limegreen"))
  
  # Total Responses by Language
  
  ggplot(descriptives, aes(x=Total.Responses, fill=Language)) + geom_density(alpha=.3) +
    scale_fill_manual(name="Language Condition",
                        breaks=c("Dax To", "Dax"),
                        labels=c("Manner", "Outcome"), values = c("limegreen","dodgerblue"))
  
  # Total Responses by Gender
  
  ggplot(descriptives, aes(x=Total.Responses, fill=Gender)) + geom_density(alpha=.3) +
    scale_fill_discrete(name="Participant Gender", breaks = c("F", "M"), labels=c("Girls", "Boys"))
  



