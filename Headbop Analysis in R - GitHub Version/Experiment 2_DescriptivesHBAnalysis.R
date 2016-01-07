## Chelsea Lide
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016

# This code is ONLY Descriptives of Experiment 2: 
  # Participants
  # Durations
  # First Contact (no stats)

  

## Exploratory Descriptive Analyses -----------------------------------------------------------------------------


if (!require(graphics)) {install.packages("graphics"); require(graphics)}
if (!require(lattice)) {install.packages("lattice"); require(lattice)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}

## Participant Descriptives -----------------------

raw.data <- read.csv("HBThesis_Data.csv")

# Kick out excluded participants

mydata <- raw.data[raw.data$Include == 1,]
mydata <- raw.data[!is.na(raw.data$Has.Exploration.Period),]

attach(mydata)

# Number of participants

  n <- dim(mydata)[1]
  n 

# Gender distribution

  girls <- sum(mydata$Gender == "F")
  boys <- sum(mydata$Gender == "M")

  girls
  boys

# Age 

  summary(mydata$Year.and.Months)
  summary(mydata$Days.Old)
  hist(Days.Old, breaks = 5, main = "Age Distribution", xlab = "Days Old")


  
# MCDI 

  # All participants
  summary(mydata$Raw.MCDI..Harvard.Only.)
  plot(mydata$Raw.MCDI..Harvard.Only., main = "Raw MCDI Scores", xlab = "Participants", ylab = "Vocab Score")

  # Gender Differences 
 
  x11()
  p <- ggplot(mydata, aes(factor(Gender), Raw.MCDI..Harvard.Only.), xlab = "Gender", ylab = "Vocabulary Score")  
  p + geom_boxplot() +
  labs(x = "Gender", 
        y = "Vocabulary Score",
        title = "Vocabulary Differences by Gender")
  

# Per Condition 
  
  # gender, age, MCDI
  sub.ex.man <- subset(mydata, Lang.Condition == "Dax to toy" & Action.Condition == "Hands Free", select = c(Gender, Days.Old, Raw.MCDI..Harvard.Only.))
  sub.ex.out <- subset(mydata, Lang.Condition == "Dax toy" & Action.Condition == "Hands Free", select = c(Gender, Days.Old, Raw.MCDI..Harvard.Only.))
  sub.oc.man <- subset(mydata, Lang.Condition == "Dax to toy" & Action.Condition == "Hands Occupied", select = c(Gender, Days.Old, Raw.MCDI..Harvard.Only.))
  sub.oc.out <- subset(mydata, Lang.Condition == "Dax toy" & Action.Condition == "Hands Occupied", select = c(Gender, Days.Old, Raw.MCDI..Harvard.Only.))
  
  # Exposed, Manner 
  sem.girls <- sum(sub.ex.man$Gender == "F")
  sem.boys <- sum(sub.ex.man$Gender == "M")
  sem.age <- mean(sub.ex.man$Days.Old)
  sem.MCDI <- mean(sub.ex.man$Raw.MCDI..Harvard.Only.)
  ex.man <- c(sem.girls, sem.boys, I(sem.age/30), sem.MCDI)
  ex.man
 
  # Exposed, Outcome 
  seo.girls <- sum(sub.ex.out$Gender == "F")
  seo.boys <- sum(sub.ex.out$Gender == "M")
  seo.age <- mean(sub.ex.out$Days.Old)
  seo.MCDI <- mean(sub.ex.out$Raw.MCDI..Harvard.Only.)
  ex.out <- c(seo.girls, seo.boys, I(seo.age/30), seo.MCDI)
  ex.out
  
  # Occupied, Manner 
  som.girls <- sum(sub.oc.man$Gender == "F")
  som.boys <- sum(sub.oc.man$Gender == "M")
  som.age <- mean(sub.oc.man$Days.Old)
  som.MCDI <- mean(sub.oc.man$Raw.MCDI..Harvard.Only.)
  oc.man <- c(som.girls, som.boys, I(som.age/30), som.MCDI)
  oc.man
  
  # Occupied, Outcome 
  soo.girls <- sum(sub.oc.out$Gender == "F")
  soo.boys <- sum(sub.oc.out$Gender == "M")
  soo.age <- mean(sub.oc.out$Days.Old)
  soo.MCDI <- mean(sub.oc.out$Raw.MCDI..Harvard.Only.)
  oc.out <- c(soo.girls, soo.boys, I(soo.age/30), soo.MCDI)
  oc.out

  x11()
  op <- par(mfrow = c(2,2))
  barplot(ex.man, names.arg = c("Girls", "Boys", "Mean Age (mos)", "Mean Vocab"),
          cex.names = .8, col = c("lightpink", "lightblue", "whitesmoke", "whitesmoke"), main = "Exposed, Manner", axes = FALSE)
          axis(2, las = 1, at = seq(0 , 30, by = 5))
  barplot(ex.out, names.arg = c("Girls", "Boys", "Mean Age (mos)", "Mean Vocab"), 
          cex.names = .8, col = c("lightpink", "lightblue", "whitesmoke", "whitesmoke"), main = "Exposed, Outcome", axes = FALSE)
          axis(2, las = 1, at = seq(0 , 30, by = 5))
  barplot(oc.man, names.arg = c("Girls", "Boys", "Mean Age (mos)", "Mean Vocab"), 
          cex.names = .8, col = c("lightpink", "lightblue", "whitesmoke", "whitesmoke"), main = "Occupied, Manner", axes = FALSE)
          axis(2, las = 1, at = seq(0 , 30, by = 5))
  barplot(oc.out, names.arg = c("Girls", "Boys", "Mean Age (mos)", "Mean Vocab"), 
          cex.names = .8, col = c("lightpink", "lightblue", "whitesmoke", "whitesmoke"), main = "Occupied, Outcome", axes = FALSE)
          axis(2, las = 1, at = seq(0 , 30, by = 5))
  par(op)
  
detach(mydata)
  
# -----------------------------------------------------------------------
  
#Read in and merge individual data sets
  
  load_data <- function(path) { 
    files <- dir(path, pattern = '\\.csv', full.names = TRUE)
    tables <- lapply(files, read.csv)
    do.call(rbind, tables)
  }
  
alldata <- load_data("Headbop Coded CSVs")
  
head(alldata)
attach(alldata)
  
## Trial Length Descriptives ----------------------


# Initializing the data frame 
trial.data <- subset(alldata, TrackName == "Total Trial Length", select = c(Participant, Condition, Hands, Language, Duration))
summary(trial.data)

# Converting duration units (from miliseconds to minutes)
duration.minutes <- I((trial.data$Duration/1000)/60)

# Initializing the data frame with converted units
participant.trial <- cbind(trial.data[1:4], duration.minutes)

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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
participant.warmup <- cbind(warmup.data[1:4], duration.minutes)

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------
     
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

## ----------------------------------------------------------------------------------------------     
     
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
     
## ---------------------------------------------------------------------------------------------- 
    
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

## ----------------------------------------------------------------------------------------------     
     
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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------
     
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

## ----------------------------------------------------------------------------------------------

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

## ----------------------------------------------------------------------------------------------

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



## (Basic) First Response Descriptives ----------------------------------------

# All conditions 

head.touches <- as.data.frame(subset(mydata, Response..First.Action. == "Head", select = c(Participant.., Lang.Condition, Action.Condition)))
hand.touches <- as.data.frame(subset(mydata, Response..First.Action. == "Hand", select = c(Participant.., Lang.Condition, Action.Condition)))

summary(head.touches)  ## seems to be trending in the opposite direction as predicted
summary(hand.touches)  ## also seems to be trending in the opposite direction as predicted

mydata$ggresponse<-rep("No Response",26)
mydata$ggresponse[mydata$Response..First.Action.=="Hand"]<-"Hand"
mydata$ggresponse[mydata$Response..First.Action.=="Head"]<-"Head"

## ----------------------------------------------------------------------------------------------

# All Conditions, Counts
x11()
ggplot(data=mydata, aes(x=Condition)) + geom_histogram(aes(fill=ggresponse)) + labs(fill="Response") + 
  ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))


# All Conditions, Proportions
x11()
ggplot(data=mydata, aes(x=Condition, fill=ggresponse)) + geom_bar(position="fill") + ylab("Proportion of Responses") + 
  labs(fill="Response") + ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))

## ----------------------------------------------------------------------------------------------

# By Hands, Counts
x11()
ggplot(data=mydata, aes(x=Action.Condition)) + geom_histogram(aes(fill=ggresponse)) + labs(fill="Response") + 
  ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))


# By Hands, Proportions
x11()
ggplot(data=mydata, aes(x=Action.Condition, fill=ggresponse)) + geom_bar(position="fill") + ylab("Proportion of Responses") + 
  labs(fill="Response") + ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))

## ----------------------------------------------------------------------------------------------

# By Language, Counts
x11()
ggplot(data=mydata, aes(x=Lang.Condition)) + geom_histogram(aes(fill=ggresponse)) + labs(fill="Response") + 
  ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))

# By Language, Proportions
x11()
ggplot(data=mydata, aes(x=Lang.Condition, fill=ggresponse)) + geom_bar(position="fill") + ylab("Proportion of Responses") + 
  labs(fill="Response") + ggtitle("First Response Action") + xlab("Condition") + theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("chartreuse3", "deepskyblue3", "grey94"))


detach(alldata)

# Correlations & Other Random Things... 

# Reshape to wide format 

x <- reshape(alldata, timevar = "TrackName", idvar = "Participant", direction = "wide")
head(x)

# Subset duration data by participant

duration.data <- as.data.frame(c(x$`Duration.Total Trial Length`, x$`Duration.Warm Up Duration`, 
                                 x$`Duration.Demonstration Duration`, x$`Duration.First Response Window`,
                                 x$`Duration.Exploration Duration`))

# Pull out Participant # and Condition info 

duration.data <- x[1:4]

# Initialize new cols & run through loop  

total.dur = NULL
warmup.dur  = NULL
demo.dur = NULL
firstresp.dur = NULL
expl.dur = NULL 

n <- dim(x)[1]

for (i in 1:n) {
  duration.data$total.dur <- x$`Duration.Total Trial Length`
  duration.data$warmup.dur <- x$`Duration.Warm Up Duration`
  duration.data$demo.dur <- x$`Duration.Demonstration Duration`
  duration.data$firstresp.dur <- x$`Duration.First Response Window`
  duration.data$expl.dur <- x$`Duration.Exploration Duration`
}
colnames(duration.data) = c("Participant", "Condition", "Hands", "Language", "Total Trial Duration",
                            "Warm Up Duration", "Demonstration Duration", "First Response Window Duration",
                            "Exploration Duration")
attach(duration.data)
pairs(~Condition+`Warm Up Duration`+`Demonstration Duration`+`First Response Window Duration`+`Exploration Duration`,data=duration.data, 
      main="Simple Scatterplot Matrix")
detach(duration.data)


# Exploration Data ----

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




