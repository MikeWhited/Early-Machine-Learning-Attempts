# All of the comments for the analysis are in a PDF that was downloaded from CoCalc.com
# That is a cloud website I do a lot of my work on because it is a R Markdown file with a nice UI
# Not taking credit for all of this, this was scripted during a tutrial I was following. 


Training <- read.csv("Downloads/train.csv")
Testing <- read.csv("Downloads/test.csv")
head(Training)
head(Testing)
test.survived <- data.frame(Survived = rep("None",nrow(Testing)),Testing[,])
head(test.survived)
data.combined <- rbind(Training, test.survived)
head(data.combined)
summary(data.combined)
str(data.combined)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)
table(data.combined$Survived)
table(data.combined$Pclass)
library(stringr)
library(ggplot2)
Training$Survived <- as.factor(Training$Survived)
Training$Pclass <- as.factor(Training$Pclass)
ggplot(Training, aes(x = Pclass,fill = Survived)) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
head(as.character(Training$Name))
length(unique(as.character(data.combined$Name)))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
dup.names
data.combined[which(data.combined$Name %in% dup.names),]
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]
males <- data.combined[which(Training$Sex == "male"),]
males[1:5,]
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)
head(data.combined)
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
table(data.combined$Sex)
ggplot(data.combined[1:891,], aes( x = Sex, fill = Survived)) +
  geom_bar(width=0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
summary(data.combined$Sex)
library(dplyr)
data.combined %>%
  select(Survived, Pclass, PassengerId, Sex) %>%
  filter(Pclass == "2" & Sex =="male")
Male2 <- subset(data.combined[1:891,], Pclass == "2" & Sex == 'male',select=c(Sex, Survived, Pclass, title))
summary(Male2)
Male3 <- subset(data.combined[1:891,], Pclass == "3" & Sex == 'male',select=c(Sex, Survived, Pclass, title))
summary(Male3)
Male2surviverate <- (17/(91+17))
Male2surviverate
Male3surviverate <- (47/(300+47))
Male3surviverate
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])
ggplot(data.combined[1:891,],aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width=10) +
  xlab("Age") +
  ylab("Total Count")
boys <- data.combined[which(data.combined$title == "Master."),]
count(boys)
summary(boys$Age)
misses <- data.combined[which(data.combined$title == "Miss."),]
count(misses)
summary(misses$Age)
head(misses)
ggplot(misses[misses$Survived != "None",], aes( x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar(width = 5) +
ggtitle("Age for Miss. by Pclass") +
  xlab("Age")
ylab("Total Count")
misses.alone <- misses[which(as.numeric(misses$SibSp == 0) & as.numeric(misses$Parch == 0)),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
str(misses.alone)
misses.alone2 <- subset(misses, SibSp == 0 & Parch == 0, select = c("Age", "Sex", "title", "SibSp", "Parch"))
head(misses.alone2)
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)
ggplot(data.combined[1:891,], aes( x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Plass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes( x = Parch, fill = Survived)) +
  geom_bar(width=1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
temp.sibsp <- c(Training$SibSp, Testing$SibSp)
temp.parch <- c(Training$Parch, Testing$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)
length(temp.sibsp)
length(temp.parch)
data.combined[1300:1309,]
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)
data.combined$ticket.first.char <- as.factor(ticket.first.char)
ggplot(data.combined[1:891,], aes( x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability of Ticket's 1st character") +
  xlab("1st Character") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("1st Character") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")
char.test <- data.combined[which(data.combined$ticket.first.char == '2' & data.combined$Pclass == '3'),]
head(char.test)
origin.test <- Training[which(Training$Pclass == 3),]
summary(origin.test$Ticket)
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("1st Character on ticket") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
summary(data.combined$Fare)
length(unique(data.combined$Fare))
ggplot(data.combined,aes(x=Fare)) +
  geom_bar(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)
ggplot(data.combined[1:891,],aes(x= Fare, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass + title) +
ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
labs(fill = "Survived")
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)
data.combined$cabin.first.char <- cabin.first.char
ggplot(data.combined[1:891,], aes( x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival by Cabin") +
  xlab("Cabin Level") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")
ggplot(data.combined[1:891,], aes( x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivabiltiy by Class") +
  xlab("Cabin 1st Letter") +
  ylab("Total Count") +
  labs(fill = "Survived")
ggplot(data.combined[1:891,], aes( x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin's 1st Letter") +
  ylab("Total Count") +
  labs(fill = "Survived")
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
head(data.combined)
ggplot(data.combined[1:891,], aes( x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Multiple cabins?") +
  ylab("Total Count") +
  labs( fill = "Survived")
str(data.combined$Embarked)
levels(data.combined$Embarked)
ggplot(data.combined[1:891,], aes( x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Where they Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")
library(randomForest)
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(Training$Survived)
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]
set.seed(1234)
rf.3 <- randomForest( x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]
set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]
set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
write.csv(submit.df, file = "RF_SUB_2018_7_12_1.csv", row.names = FALSE)
library(caret)
library(doSNOW)
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
table(rf.label)
table(rf.label[cv.10.folds[[33]]])
342/549
308/494
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tunelength = 3, ntree = 1000, trControl = ctrl.1)
stopCluster(cl)
rf.5.cv.1
rf.5
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)
stopCluster(cl)
rf.5.cv.2
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 64, trControl = ctrl.3)
stopCluster(cl)
rf.5.cv.3
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Per video #5, let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("Pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Both rpart and rf confirm that title is important, let's investigate further
table(data.combined$title)

# Parse out last name and title
data.combined[1:25, "name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'?
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) + 
  ggtitle("Surival Rates for new.title by pclass")

# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." | 
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")


# Grab features
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Dive in on 1st class Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$Sex == "female",]

# Update new.title feature
indexes <- which(data.combined$new.title == "Mr." & 
                   data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

# Any other gender slip ups?
length(which(data.combined$Sex == "female" & 
               (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

# Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")

# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# One missing value, take a look
data.combined[is.na(data.combined$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & family.size == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again

# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_Final_Sub.csv", row.names = FALSE)

features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_Final_Sub.csv", row.names = FALSE)

library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than 'Mr."
#install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")


# NOTE - Bug fix for original version. Rtsne needs a seed to ensure consistent
# output between runs.
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")


# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]
