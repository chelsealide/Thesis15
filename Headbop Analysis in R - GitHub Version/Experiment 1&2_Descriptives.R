### Exploratory Descriptives for both Experiment 1 and Experiment 2 


require(plyr)
require(ggplot2)
require(reshape2)
require(reshape)
require(corrplot)



### Load data for Experiment 1 ----






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
  colnames(descriptives) <- c("Participant", "Condition", "Language", "Hands", "Gender", "Days.Old", "MCDI", "Head.Touches", "Hand.Touches", "Total.Responses")

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

ggplot(descriptives, aes(x=Total.Dur, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
  
  # Warmup 

ggplot(descriptives, aes(x=Warmup.Dur, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

  # Demonstration

ggplot(descriptives, aes(x=Demo.Dur, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

  # FirstResponse

ggplot(descriptives, aes(x=FirstResponse.Dur, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

  # Exploration

ggplot(descriptives, aes(x=Exploration.Dur, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

  # Engagement

ggplot(descriptives, aes(x=Engagement.Duration, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))

  # IBA

ggplot(descriptives, aes(x=IBA.Duration, fill=Condition)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Condition",
                      breaks=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"),
                      labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
  

### No real trends, other than that first response window tending to be shortest in Exposed, Dax To cond ###




  # Head touches by Gender 
  
  ggplot(descriptives, aes(x=Head.Touches, fill=Gender)) + geom_density(alpha=.3) +
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
  



