
require(reshape)
require(reshape2)
require(plyr)
require(ggplot2)
require(lme4)


### Load in data ----

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

rawdata <- load_data("EIBA CSVs")
head(rawdata)


### Subsetting "Engagement" ----

e <- subset(rawdata, TrackName == "Engagement", c("Participant", "Condition", "Hands", "Language", "Duration"))
e$Duration <- I(e$Duration/1000)
e

### Summing durations based on participant numbers

DurationSums <- aggregate(. ~ Participant, data=e, FUN=sum)

### Organizing the dataframe

Engagement <- subset(e, !duplicated(e[1]))
Engagement[5] <- as.vector(DurationSums[5])
colnames(Engagement) <- c("Participant", "Condition", "Hands", "Language", "Summed Duration")
attach(Engagement)

x11()
boxplot(`Summed Duration`~ Condition, data = Engagement, main = "Duration of Engagement During Exploration", 
        xlab="Condition", ylab="Duration (s)")


### Engagement Stats ----

# By Hands

t.test(Engagement$`Summed Duration`~Engagement$Hands)

# By Language

t.test(Engagement$`Summed Duration`~Engagement$Language)

# Model fitting


null.fit <- aov(`Summed Duration` ~ Hands, data=Engagement)

fit.1 <- aov(`Summed Duration` ~ Hands + Language, data=Engagement)

fit.2 <- aov(`Summed Duration`~ Hands*Language, data=Engagement)

eng.comparison <- anova(null.fit,fit.1, fit.2)
eng.comparison 
 
## Variance in Engagement during exploration period is not explained by Hands/Language, or any interaction between the two

detach(Engagement)


### Subsetting "IBAs" ----

i <- subset(rawdata, TrackName == "IBA", c("Participant", "Condition", "Hands", "Language", "Duration"))
i$Duration <- I(i$Duration/1000)
i

DurationSums <- aggregate(. ~ Participant, data=i, FUN=sum)

### Organizing the dataframe

IBAs <- subset(i, !duplicated(i[1]))
IBAs[5] <- as.vector(DurationSums[5])
colnames(IBAs) <- c("Participant", "Condition", "Hands", "Language", "Summed Duration")
attach(IBAs)

x11()
boxplot(`Summed Duration`~ Condition, data = IBAs, main = "Duration of Engagement During Exploration", 
        xlab="Condition", ylab="Duration (s)")


### IBAs Stats ----

# By Hands

t.test(IBAs$`Summed Duration`~Hands)

# By Language

t.test(`Summed Duration`~Language)

# Model fitting

null.fit <- aov(`Summed Duration` ~ Hands, data=IBAs)

fit.1 <- aov(`Summed Duration` ~ Hands + Language, data=IBAs)

fit.2 <- aov(`Summed Duration`~ Hands*Language, data=IBAs)

iba.comparison <- anova(null.fit,fit.1, fit.2)
iba.comparison 

## Variance in IBAs during exploration period is not explained by Hands/Language, or any interaction between the two

detach(IBAs)





