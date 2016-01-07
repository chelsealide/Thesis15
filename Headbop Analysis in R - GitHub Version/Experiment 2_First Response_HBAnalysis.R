## Chelsea Lide
## Honors Thesis in Psychology: Mind, Brain, Behavior
## Harvard University, Class of 2016


# ! This code is ONLY descriptives/stats for the first response in Experiment 2 !



# Load in & clean up data set ----

raw.data <- read.csv("HBThesis_Data.csv")

# Kick out excluded participants

mydata <- raw.data[raw.data$Include == 1,]
mydata <- raw.data[!is.na(raw.data$Has.Exploration.Period),]
attach(mydata)

### LOGISTIC REGRESSION ----








