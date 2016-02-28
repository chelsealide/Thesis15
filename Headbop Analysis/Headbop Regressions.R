### Experiment 1 and 2 Stats (using pre-cleaned csv)
### Chelsea Lide 
### Harvard University, 2016


require(glmr)
require(stats)


# Experiment 1 /// First Response \\\ Logistic Regression ---- 

### Load in cleaned data 

raw.data <- read.csv("Experiment1Data.csv")

### Kick out people who didn't perform first response or are excluded

data <- raw.data[!is.na(raw.data$First.Response),]
data <- raw.data[raw.data$Include == 1,]

### Recoding values for model: 

# Make relevant variables numeric 

data$Action.Condition<-as.numeric(data$Action.Condition)
data$Lang.Condition<-as.numeric(data$Lang.Condition)
data$Response<-as.numeric(data$Response)

# Lang - Dax = 0, Dax To = 1

data$Lang.Condition[data$Lang.Condition==2]<-0
data$Lang.Condition[data$Lang.Condition==1]<-1

# Hand - Occupied = 0, Exposed = 1

data$Action.Condition[data$Action.Condition==2]<-0
data$Action.Condition[data$Action.Condition==1]<-1

# Resp - Hand = 0, Head = 1 

data$Response[data$Response==1]<-0
data$Response[data$Response==3]<-1

attach(data)

### Logistic regression 

# Without Interaction

test13 <- glm(Response~Action.Condition+Lang.Condition, family = "binomial")
coef(test13)
summary(test13)

# With Interaction 

test14 <- glm(Response~Action.Condition*Lang.Condition, family = "binomial")
coef(test14)
summary(test14)

detach(data)

# Experiment 2 /// First Response \\\ Logistic Regression ----


### Load in cleaned data 

  raw.data <- read.csv("CleanData.csv")

### Kick out people who didn't perform first response 

  data <- raw.data[!is.na(raw.data$First.Response),]
  
### Recoding values for model: 
  
  # Make relevant variables numeric 
  
    data$Hands<-as.numeric(data$Hands)
    data$Language<-as.numeric(data$Language)
    data$First.Response<-as.numeric(data$First.Response)
  
  # Lang - Dax = 0, Dax To = 1
  
    data$Language[data$Language==1]<-0
    data$Language[data$Language==2]<-1
  
  # Hand - Occupied = 0, Exposed = 1
  
    data$Hands[data$Hands==2]<-0
    data$Hands[data$Hands==1]<-1
    
  
  # Resp - Hand = 0, Head = 1 
  
    data$First.Response[data$First.Response==1]<-0
    data$First.Response[data$First.Response==2]<-1
    
attach(data)
  
### Logistic regression 
    
    # Without Interaction
    
    test1 <- glm(First.Response~Hands+Language, family = "binomial")
    coef(test1)
    summary(test1)
    
    # With Interaction 
    
    test2 <- glm(First.Response~Hands*Language, family = "binomial")
    coef(test2)
    summary(test2)
    
detach(data)
    
   
    
    
# Experiment 2 /// Exploration Patterns (Head & Hand Touches) \\\ Regression ----
    
    
### Load in cleaned data 
    
  raw.data <- read.csv("CleanData.csv")
    
### Kick out people who didn't explore 
    
  data <- raw.data[!is.na(raw.data$Total.Responses),]
    
### Recoding values for model: 
    
  # Make relevant variables numeric 
    
    data$Hands<-as.numeric(data$Hands)
    data$Language<-as.numeric(data$Language)
    
  # Lang - Dax = 0, Dax To = 1
    
    data$Language[data$Language==1]<-0
    data$Language[data$Language==2]<-1
    
  # Hand - Occupied = 0, Exposed = 1
    
    data$Hands[data$Hands==2]<-0
    data$Hands[data$Hands==1]<-1
    
attach(data)
  

### HEAD Touch Regression 
    
  # Without Interaction
    
    test3 <- glm(Head.Touches~Hands+Language)
    coef(test3)
    summary(test3)
    
  # With Interaction 
    
    test4 <- glm(Head.Touches~Hands*Language)
    coef(test4)
    summary(test4)
    
    
### HAND Touch Regression 
    
  # Without Interaction
    
    test5 <- glm(Hand.Touches~Hands+Language)
    coef(test5)
    summary(test5)
    
  # With Interaction 
    
    test6 <- glm(Hand.Touches~Hands*Language)
    coef(test6)
    summary(test6)
    
### ALL Regression 
    
  # Without Interaction
    
    test7 <- glm(Total.Responses~Hands+Language)
    coef(test7)
    summary(test7)
    
  # With Interaction 
    
    test8 <- glm(Total.Responses~Hands*Language)
    coef(test8)
    summary(test8)

  
  
    
    
# Experiment 2 /// Engagement (duration) \\\ Regression ----
    
    # Without Interaction
    
    test9 <- glm(Engagement.Duration~Hands+Language)
    coef(test9)
    summary(test9)
    
    # With Interaction 
    
    test10 <- glm(Engagement.Duration~Hands*Language)
    coef(test10)
    summary(test10)

# Experiment 2 /// IBA (duration) \\\ Regression ----
    
  # Without Interaction
    
    test11 <- glm(IBA.Duration~Hands+Language)
    coef(test11)
    summary(test11)
    
  # With Interaction 
    
    test12 <- glm(IBA.Duration~Hands*Language)
    coef(test12)
    summary(test12)
    
detach(data)   
    

    
    