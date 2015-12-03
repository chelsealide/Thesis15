## Chelsea Lide
## Headbop Data Analysis 
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016
## ---------------------------------------------------------------------------------------------------

setwd("/Users/cello72294/Desktop/Headbop Coded CSVs")

#Read in and merge individual data sets

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

alldata <- load_data("/Users/cello72294/Desktop/Headbop Coded CSVs")

head(alldata)
attach(alldata)


## Exploratory Descriptive Analyses -----------------------------------------------------------------------------

## Load Packages Here
if (!require(graphics)) {install.packages("graphics"); require(graphics)}


## Participant Descriptives -----------------------

raw.data <- read.csv("HBThesis_Data.csv")

# Kick out excluded participants

mydata <- raw.data[raw.data$Include == 1,]
mydata <- raw.data[!is.na(raw.data$Has.Exploration.Period),]

# Number of participants

n <- dim(mydata)[1]
n 

# Gender distribution

girls <- sum(mydata$Gender == "F")
boys <- sum(mydata$Gender == "M")

girls
boys

# Age 

summary(mydata$Days.Old)

daysold <- range(mydata$Days.Old)
monthsold <- daysold/30
  
daysold
monthsold


# MCDI 

# All participants
summary(mydata$Raw.MCDI..Harvard.Only.)
plot(mydata$Raw.MCDI..Harvard.Only., main = "Raw MCDI Scores", xlab = "Participants", ylab = "Vocab Score")

# Gender Differences 
FMCDI <- subset(mydata, Gender == "F", select = Raw.MCDI..Harvard.Only.)
MMCDI <- subset(mydata, Gender == "M", select = Raw.MCDI..Harvard.Only.)

x11()
op <- par(mfrow = c(1,2))
boxplot(FMCDI, main = "Girls' MCDI Score", xlab = "Participants", ylab = "Vocab Score")
boxplot(MMCDI, main = "Boys' MCDI Score", xlab = "Participants", ylab = "Vocab Score")
par(op)


## Trial Length Descriptives ----------------------

# Initializing the data frame 
trial.data <- subset(alldata, TrackName == "Total Trial Length", select = c(Participant, Condition, Hands, Language, Duration))
summary(trial.data)

# Converting duration units (from miliseconds to minutes)
duration.minutes <- I((trial.data$Duration/1000)/60)

# Initializing the data frame with converted units
participant.trial <- cbind(trial.data[1:4], duration.minutes)

# Between Hand Conditions

# Subset paricipants in the Hands Exposed Condtion
trial.ex <- subset(participant.trial, Hands == "Exposed", select = c(Participant, duration.minutes))

# Subset participants in the Hands Occupied Condition
trial.oc <- subset(participant.trial, Hands == "Occupied", select = c(Participant, duration.minutes))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(trial.ex, type = "p", main = "Total Trial Duration - Hands Exposed", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 20))
     mdex <- mean(duration.minutes)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(trial.oc, type = "p", main = "Total Trial Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 20))     
     mdoc <- mean(duration.minutes) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)

# Between Language Conditions

# Subset paricipants in the Hands Exposed Condtion
trial.man <- subset(participant.trial, Language == "Dax To", select = c(Participant, duration.minutes))

# Subset participants in the Hands Occupied Condition
trial.out <- subset(participant.trial, Language == "Dax", select = c(Participant, duration.minutes))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(trial.man, type = "p", main = "Total Trial Duration - Manner", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 20))
     mdex <- mean(duration.minutes)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(trial.out, type = "p", main = "Total Trial Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 20))     
     mdoc <- mean(duration.minutes) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)

# Between All Conditions

# Subsetting 

trial.1 <- subset(participant.trial, Condition == "Exposed, Dax To", select = c(Participant, duration.minutes))
trial.2 <- subset(participant.trial, Condition == "Exposed, Dax", select = c(Participant, duration.minutes))
trial.3 <- subset(participant.trial, Condition == "Occupied, Dax To", select = c(Participant, duration.minutes))
trial.4 <- subset(participant.trial, Condition == "Occupied, Dax", select = c(Participant, duration.minutes))

x11()
op <- par(mfrow = c(1,4))
     boxplot(trial.1[2], main = "Exposed, Dax To", ylab = "Demonstration Duration (s)")
     boxplot(trial.2[2], main = "Exposed, Dax")
     boxplot(trial.3[2], main = "Occupied, Dax To")
     boxplot(trial.4[2], main = "Occupied, Dax")
par(op)




## Warm Up Descriptives ---------------------------

# Initializing the data frame 
warmup.data <- subset(alldata, TrackName == "Warm Up Duration", select = c(Participant, Condition, Hands, Language, Duration))
summary(warmup.data)

# Converting duration units (from miliseconds to minutes)
duration.minutes <- I((warmup.data$Duration/1000)/60)

# Initializing the data frame with converted units
participant.warmup <- cbind(demonstration.data[1:4], duration.minutes)

# Between Hand Conditions

# Subset paricipants in the Hands Exposed Condtion
warmup.ex <- subset(participant.warmup, Hands == "Exposed", select = c(Participant, duration.minutes))

# Subset participants in the Hands Occupied Condition
warmup.oc <- subset(participant.warmup, Hands == "Occupied", select = c(Participant, duration.minutes))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(warmup.ex, type = "p", main = "Warm-Up Duration - Hands Exposed", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 20))
     mdex <- mean(duration.minutes)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(warmup.oc, type = "p", main = "Warm-Up Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 20))     
     mdoc <- mean(duration.minutes) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)

# Between Language Conditions

# Subset paricipants in the Hands Exposed Condtion
warmup.man <- subset(participant.warmup, Language == "Dax To", select = c(Participant, duration.minutes))

# Subset participants in the Hands Occupied Condition
warmup.out <- subset(participant.warmup, Language == "Dax", select = c(Participant, duration.minutes))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(warmup.man, type = "p", main = "Warm-Up Duration - Manner", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 20))
     mdex <- mean(duration.minutes)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(warmup.out, type = "p", main = "Warm-Up Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 20))     
     mdoc <- mean(duration.minutes) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)

# Between All Conditions

# Subsetting 

warmup.1 <- subset(participant.warmup, Condition == "Exposed, Dax To", select = c(Participant, duration.minutes))
warmup.2 <- subset(participant.warmup, Condition == "Exposed, Dax", select = c(Participant, duration.minutes))
warmup.3 <- subset(participant.warmup, Condition == "Occupied, Dax To", select = c(Participant, duration.minutes))
warmup.4 <- subset(participant.warmup, Condition == "Occupied, Dax", select = c(Participant, duration.minutes))

x11()
op <- par(mfrow = c(1,4))
boxplot(warmup.1[2], main = "Exposed, Dax To", ylab = "Demonstration Duration (s)")
boxplot(warmup.2[2], main = "Exposed, Dax")
boxplot(warmup.3[2], main = "Occupied, Dax To")
boxplot(warmup.4[2], main = "Occupied, Dax")
par(op)



## Demonstration Descriptives ----------------------

# Initializing the data frame 
demonstration.data <- subset(alldata, TrackName == "Demonstration Duration", select = c(Participant, Condition, Hands, Language, Duration))
summary(demonstration.data)

# Converting duration units (from miliseconds to seconds)
duration.seconds <- I(demonstration.data$Duration/1000)

# Initializing the data frame with converted units
participant.demonstration <- cbind(demonstration.data[1:4], duration.seconds)

# Plot of demonstration duration by participant 
x11()
plot(participant.demonstration [c (1,5)], type = "p", 
     main = "Demonstration Duration", xlab = "Participant", ylab = "Duration (s)")
     md <- mean(duration.seconds)
     h <- abline(h = md, col = "red")

# Between Hand Conditions

# Subset paricipants in the Hands Exposed Condtion
demo.ex <- subset(participant.demonstration, Hands == "Exposed", select = c(Participant, duration.seconds))
     
# Subset participants in the Hands Occupied Condition
demo.oc <- subset(participant.demonstration, Hands == "Occupied", select = c(Participant, duration.seconds))
     
# Plot the two side-by-side
     
x11()
op <- par(mfrow = c(1,2))
plot(demo.ex, type = "p", main = "Demonstration Duration - Hands Exposed", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(30 , 60, by = 5))
     mdex <- mean(duration.seconds)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(demo.oc, type = "p", main = "Demonstration Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(30 , 60, by = 5))     
     mdoc <- mean(duration.seconds) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
     par(op)
     
     
# Between Language Conditions

# Subset paricipants in the Manner Condtion
demo.man <- subset(participant.demonstration, Language == "Dax To", select = c(Participant, duration.seconds))
     
# Subset participants in the Outcome Condition
demo.out <- subset(participant.demonstration, Language == "Dax", select = c(Participant, duration.seconds))
     
x11()
op <- par(mfrow = c(1,2))
plot(demo.man, type = "p", main = "Demonstration Duration - Manner", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(30 , 60, by = 5))
     mdex <- mean(duration.seconds)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(demo.out, type = "p", main = "Demonstration Duration - Outcome", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(30 , 60, by = 5))     
     mdoc <- mean(duration.seconds) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)
     
     
# Between All Conditions

# Subsetting 

demo.1 <- subset(participant.demonstration, Condition == "Exposed, Dax To", select = c(Participant, duration.seconds))
demo.2 <- subset(participant.demonstration, Condition == "Exposed, Dax", select = c(Participant, duration.seconds))
demo.3 <- subset(participant.demonstration, Condition == "Occupied, Dax To", select = c(Participant, duration.seconds))
demo.4 <- subset(participant.demonstration, Condition == "Occupied, Dax", select = c(Participant, duration.seconds))

x11()
op <- par(mfrow = c(1,4))
boxplot(demo.1[2], main = "Exposed, Dax To", ylab = "Demonstration Duration (s)")
boxplot(demo.2[2], main = "Exposed, Dax")
boxplot(demo.3[2], main = "Occupied, Dax To")
boxplot(demo.4[2], main = "Occupied, Dax")
par(op)


## (Basic) First Response Window Descriptives ----------------------

# Initializing the data frame 
frw.data <- subset(alldata, TrackName == "First Response Window", select = c(Participant, Condition, Hands, Language, Duration))
summary(frw.data)

# Converting duration units (from miliseconds to seconds)
duration.seconds <- I(frw.data$Duration/1000)

# Initializing the data frame with converted units
participant.frw <- cbind(frw.data[1:4], duration.seconds)

# Plot of demonstration duration by participant 
x11()
plot(participant.frw [c (1,5)], type = "p", 
     main = "First Response Window Duration", xlab = "Participant", ylab = "Duration (s)")
     md <- mean(duration.seconds)
     abline(h = md, col = "red")

# Between Hand Conditions

# Subset paricipants in the Hands Exposed Condtion
frw.ex <- subset(participant.frw, Hands == "Exposed", select = c(Participant, duration.seconds))

# Subset participants in the Hands Occupied Condition
frw.oc <- subset(participant.frw, Hands == "Occupied", select = c(Participant, duration.seconds))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(frw.ex, type = "p", main = "First Response Window Duration - Hands Exposed", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 40, by = 5))
     mdex <- mean(duration.seconds)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .4, col = "red") 
plot(frw.oc, type = "p", main = "First Response Window Duration - Hands Occupied", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0 , 40, by = 5))     
     mdoc <- mean(duration.seconds) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .4, col = "red") 
par(op)


# Between Language Conditions

# Subset paricipants in the Manner Condtion
frw.man <- subset(participant.frw, Language == "Dax To", select = c(Participant, duration.seconds))

# Subset participants in the Outcome Condition
frw.out <- subset(participant.frw, Language == "Dax", select = c(Participant, duration.seconds))

x11()
op <- par(mfrow = c(1,2))
plot(frw.man, type = "p", main = "First Response Window Duration - Manner", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 40, by = 5))
     mdex <- mean(duration.seconds)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(frw.out, type = "p", main = "First Response Window Duration - Outcome", cex.main =.8, xlab = "Participants", ylab = " ", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(0, 40, by = 5))     
     mdoc <- mean(duration.seconds) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)


# Between All Conditions

# Subsetting 

frw.1 <- subset(participant.frw, Condition == "Exposed, Dax To", select = c(Participant, duration.seconds))
frw.2 <- subset(participant.frw, Condition == "Exposed, Dax", select = c(Participant, duration.seconds))
frw.3 <- subset(participant.frw, Condition == "Occupied, Dax To", select = c(Participant, duration.seconds))
frw.4 <- subset(participant.frw, Condition == "Occupied, Dax", select = c(Participant, duration.seconds))

x11()
op <- par(mfrow = c(1,4))
boxplot(frw.1[2], main = "Exposed, Dax To", ylab = "Demonstration Duration (s)")
boxplot(frw.2[2], main = "Exposed, Dax")
boxplot(frw.3[2], main = "Occupied, Dax To")
boxplot(frw.4[2], main = "Occupied, Dax")
par(op)



## (Basic) Exploration Descriptives ------------------------

# Initializing the data frame
exploration.data <- subset(alldata, TrackName == "Exploration Duration", select = c(Participant, Condition, Hands, Language, Duration))
summary(exploration.data)

# Converting duration units (from miliseconds to seconds)
duration.seconds <- I(exploration.data$Duration/1000)

# Initializing the data frame with converted units
participant.exploration <- cbind(exploration.data[1:4], duration.seconds)

# Plot of exploration duration by participant 
x11()
plot(participant.exploration [c (1,5)], type = "p", 
     main = "Exploration Duration", xlab = "Participant", ylab = "Duration (s)")
     md <- mean(duration.seconds)
     h <- abline(h = md, col = "red")
     text(3.5,76, "Average Exploration = 74 seconds", cex = .6, font = 2, col = "red")

# Between Hand Conditions

# Subset paricipants in the Hands Exposed Condtion
exploration.ex <- subset(participant.exploration, Hands == "Exposed", select = c(Participant, duration.seconds))

# Subset participants in the Hands Occupied Condition
exploration.oc <- subset(participant.exploration, Hands == "Occupied", select = c(Participant, duration.seconds))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(exploration.ex, type = "p", 
     main = "Exploration Duration - Hands Exposed", cex.main = .8, xlab = "Participants", ylab = "Duration (s)", axes = FALSE)
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(40 , 100, by = 10))
     mdex <- mean(duration.seconds)
     abline(h = mdex, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
plot(exploration.oc, type = "p", 
     main = "Exploration Duration - Hands Occupied", cex.main =.8, xlab = "Participant", axes = FALSE) 
     axis(1, lab = FALSE)
     axis(2, las = 1, at = seq(40 , 100, by = 10))     
     mdoc <- mean(duration.seconds) 
     abline(h = mdoc, col = "red") 
     text(2,80, "mean = X seconds", cex = .6, col = "red") 
par(op)


# Between Language Conditions

# Subset paricipants in the Manner Condtion
exploration.man <- subset(participant.exploration, Language == "Dax To", select = c(Participant, duration.seconds))

# Subset participants in the Outcome Condition
exploration.out <- subset(participant.exploration, Language == "Dax", select = c(Participant, duration.seconds))

# Plot the two side-by-side

x11()
op <- par(mfrow = c(1,2))
plot(exploration.man, type = "b", 
     main = "Exploration Duration - Manner", cex.main = .8, xlab = "Participant", ylab = "Duration (s)")
     mdman <- mean(duration.seconds)
     abline(h = mdman, col = "red")
     text(2,80, "mean = X seconds", cex = .6, col = "red")
plot(exploration.out, type = "b", 
     main = "Exploration Duration - Outcome", cex.main =.8, xlab = "Participant", ylab = "Duration (s)")
     mdout <- mean(duration.seconds)
     abline(h = mdout, col = "red")
     text(2,80, "mean = X seconds", cex = .6, col = "red")
par(op)


# Between All Conditions

# Subsetting 

exp.1 <- subset(participant.exploration, Condition == "Exposed, Dax To", select = c(Participant, duration.seconds))
exp.2 <- subset(participant.exploration, Condition == "Exposed, Dax", select = c(Participant, duration.seconds))
exp.3 <- subset(participant.exploration, Condition == "Occupied, Dax To", select = c(Participant, duration.seconds))
exp.4 <- subset(participant.exploration, Condition == "Occupied, Dax", select = c(Participant, duration.seconds))

x11()
op <- par(mfrow = c(1,4))
boxplot(exp.1[2], main = "Exposed, Dax To", ylab = "Exploration Duration (s)")
boxplot(exp.2[2], main = "Exposed, Dax")
boxplot(exp.3[2], main = "Occupied, Dax To")
boxplot(exp.4[2], main = "Occupied, Dax")
par(op)




## Things to add: 
## 1) First Response Descriptives
## 2) Interactions
## 3) ? 

detach(alldata)



