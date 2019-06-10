#Function to normalize values using min-max
normal= function(x)
{
  nor= (x-min(x))/(max(x)-min(x))
}


library(caret)
white= read.csv("D:/SAS Shaas/ASDM Project/wine/winequality-white.csv", sep=";")

#Checking for missng values:
sum(is.na(white))

#checking how many quality levels there are in the dataset:
factor(white$quality)

#checking how many instances there are of each quality
table(white$quality)

#Changing the Wine Quality parameter to characters so that they are treated as nominal values 
white$quality= factor(as.character(white2$quality))

#Reassigning to keep original dataset intact for multiple explorations
white2= white

#Exploring the dataset
summary(white2)
boxplot(white2)

#Distributing the dataset into training and testing subsets
set.seed(998)
inTraining <- createDataPartition(white$quality, p = .75, list = FALSE)

training <- white2[ inTraining,]
testing  <- white2[-inTraining,]

#ctree implementation

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)
ctree = train(quality ~ ., 
                  data=training, 
                  method="ctree", 
                  trControl = control, metric="Accuracy")

test= predict(ctree, newdata=testing)

resultsctree=confusionMatrix(test, testing$quality )

#rpart implementation

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)
rpart = train(quality ~ ., 
              data=training, 
              method="rpart", 
              trControl = control, metric="Accuracy")

test= predict(rpart, newdata=testing)

resultsrpart=confusionMatrix(test, testing$quality )


#Implementing KNN
white3= white2

white3$fixed.acidity= normal(white3$fixed.acidity)
white3$volatile.acidity= normal(white3$volatile.acidity)
white3$citric.acid= normal(white3$citric.acid)
white3$residual.sugar= normal(white3$residual.sugar)
white3$chlorides= normal(white3$chlorides)
white3$free.sulfur.dioxide=normal(white3$free.sulfur.dioxide)
white3$total.sulfur.dioxide= normal(white3$total.sulfur.dioxide)
white3$density= normal(white3$density)
white3$pH= normal(white3$pH)
white3$sulphates= normal(white3$sulphates)
white3$alcohol= normal(white3$alcohol)

set.seed(998)
inTraining <- createDataPartition(white3$quality, p = .75, list = FALSE)

training <- white3[ inTraining,]
testing  <- white3[-inTraining,]


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

knn = train(quality ~ ., data = training, 
                 method = "knn", 
                 trControl = control, preProcess = c("center", "scale"), tuneLength=50)

test= predict(knn, newdata=testing)

resultsknn=confusionMatrix(test, testing$quality )


#Implementing Random Forest

set.seed(998)
inTraining <- createDataPartition(white$quality, p = .75, list = FALSE)

training <- white2[ inTraining,]
testing  <- white2[-inTraining,]

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

rf = train(quality ~ ., data = training, 
            method = "rf", 
            trControl = control, tuneLength=5, verbose=FALSE)

test= predict(rf, newdata=testing)

resultsrf=confusionMatrix(test, testing$quality)

resultsrf$byClass[,"Recall"]

#Implementing Gradient Boosting

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

gbm = train(quality ~ ., data = training, 
           method = "gbm", 
           trControl = control, verbose=FALSE)

test= predict(gbm, newdata=testing)

resultsgbm=confusionMatrix(test, testing$quality)

#Implementing a three-class prediction

white4= read.csv("D:/SAS Shaas/ASDM Project/wine/winequality-white.csv", sep=";")

white4$quality[white4$quality>6]="Above Average"
white4$quality[white4$quality==6]="Average"
white4$quality[white4$quality<6]="Below Average"

white4$quality=factor(as.character(white4$quality))

set.seed(998)
inTraining <- createDataPartition(white4$quality, p = .75, list = FALSE)

training <- white4[ inTraining,]
testing  <- white4[-inTraining,]

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

rf_three_levels = train(quality ~ ., data = training, 
           method = "rf", 
           trControl = control, verbose=FALSE)

test= predict(rf_three_levels, newdata=testing)

resultsrf_three_levels=confusionMatrix(test, testing$quality)

#Implementing a two-class prediction

white5= read.csv("D:/SAS Shaas/ASDM Project/wine/winequality-white.csv", sep=";")

white5$quality[white5$quality>5]="Above Average"
white5$quality[white5$quality<6]="Below Average"

white5$quality=factor(as.character(white5$quality))



set.seed(998)
inTraining <- createDataPartition(white4$quality, p = .75, list = FALSE)

training <- white5[ inTraining,]
testing  <- white5[-inTraining,]

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

rf_two_levels = train(quality ~ ., data = training, 
                        method = "rf", 
                        trControl = control, verbose=FALSE)

test= predict(rf_two_levels, newdata=testing)

resultsrf_two_levels=confusionMatrix(test, testing$quality)

