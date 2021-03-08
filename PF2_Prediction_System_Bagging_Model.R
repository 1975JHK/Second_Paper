# CTQ Prediction System For the PF 2nd Plant
# Coded on December 1, 2020
# Coded By Robin Kim
  
# Loading the required Package
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(caret)
  library(see)
  library(correlation)
  library(ggraph)
  library(e1071)
  library(DT)
})

options(dplyr.summarise.inform = FALSE)

# Section 1 : Thermal Conductivity ----------------------------------------
#--------------------------------------------------------------------------
# Importing the dataset
setwd('D:/RTM_PF2')
raw <- read_excel('Line12.xlsx')
str(raw)

# Preprocessing Ⅰ
## Extracting 2nd Plat Data from Raw Dataset
raw <- raw %>% filter(공장코드 == 'P2')

## Feature Selection
df <- raw[ , -c(1:6, 10, 14:15, 39:45, 51:61, 63:64)]
colnames(df) <- c('Lot_ID', 'Date', 'Viscosity', 'React.Time', 'React.Temp',
                  'Thickness', 'Resin', 'Hardener', 'Blower', 'Plasticizer', 
                  'Coal', 'Resin.Temp', 'Hanrdener.Temp', 'Blower.Temp',
                  'Speed', 'U.Temp', 'L.Temp',
                  'Pressure1', 'Pressure2', 'Pressure3',
                  'Pressure4', 'Pressure5', 'Pressure6',
                  'Pressure7', 'Pressure8', 'Pressure9',
                  'Pressure10', 'Pressure11', 'Conductivity','ShrinkageL', 'ShrinkageW',
                  'Peel.Strength1', 'Peel.Strength2', 'Peel.Strength3', 'Tissue.Type', 
                  'Length')
colnames(df)

## Handling the Not Availables
apply(df, 2, function(x) sum(is.na(x)))
df <- na.omit(df)

## Classifying Tissu.Type
df$Tissue.Type <- ifelse(df$Tissue.Type == 'J' & df$Length < 3000, 'S', df$Tissue.Type)
df <- df[ , -36]

## Removing features with zero variance 
nearZeroVar(df, saveMetrics = T)
df.nvz <- nearZeroVar(df, saveMetrics = F)
df <- df[ , -df.nvz]

df <- df[ , -16]

## Separating the dates
df <- df %>% separate(Date, into = c('Year', 'Month', 'Day'), sep = '/')
df <- df %>% filter(Year == '20', Month != '01')
addmargins(table(df$Month))


## Encoding on Tissu.Type
df$Tissue.Type <- factor(df$Tissue.Type)

## CTQ 정리
### Peel.Strength
df <- df %>% filter(apply(df[ , 31:33], 1, sd) < 50)
nrow(df)
df$Peel.Strength <- apply(df[ , 31:33], 1, mean)
df <- df[ , -c(31:33)]

### Peel.Strength
df$Shrinkage <- apply(df[ , 29:30], 1, mean)
df <- df[ , -c(29:30)]


## Removing outliers
### CTQs
df <- df %>% filter(Peel.Strength > 100)
df <- df %>% filter(Shrinkage > -3.0, Shrinkage < -0.1)
df <- df %>% filter(Conductivity > 0.0160, Conductivity < 0.210)

### React.Time
df <- df %>% filter(React.Time > 2, React.Time < 50)

### React.Temp
df <- df %>% filter(React.Temp > 77, React.Temp < 150)

### Line Speed
df <- df %>% filter(Speed > 3.0)

### Upper and Lower Dryer Temp
df <- df %>% filter(U.Temp > mean(U.Temp)-3*sd(U.Temp),
                    U.Temp < mean(U.Temp)+3*sd(U.Temp))

### Pressure6
df <- df %>% filter(Pressure6 > mean(Pressure6) - 3*sd(Pressure6))

### Pressure8
df <- df %>% filter(Pressure8 > mean(Pressure8) - 3*sd(Pressure8))

### Pressure11
df <- df %>% filter(Pressure11 > mean(Pressure11) - 3*sd(Pressure11),
                    Pressure11 < mean(Pressure11) + 3*sd(Pressure11))

# Feature Engineering
## Pressure slope on Dryers
df <- df %>% mutate(P1 = Pressure6 - Pressure4,
                    P2 = Pressure8 - Pressure6,
                    P3 = Pressure8 - Pressure11,
                    Var.P = P1-P2)

## Resin's reactivity
df <- df %>% mutate(Reactivity1 = Viscosity*Speed*U.Temp,
                    Reactivity2 = Speed*U.Temp*P1)
## Arranging dataset
colnames(df)
df <- df[ , c(1:27, 29, 32:37, 28, 31, 30)]
colnames(df)


# Preprocessing Ⅱ
## Encoding : CTQs
### Peel.Strength
df <- df %>% mutate(
  Peel.Strength = case_when(
    Peel.Strength < 180 ~ 'Bad',
    Peel.Strength < 250 ~ 'Warning',
    Peel.Strength >= 250 ~ 'Good',
  )
)


df$Peel.Strength <- factor(df$Peel.Strength,
                           levels = c('Warning', 'Good', 'Bad'),
                           labels = c('Warning', 'Good', 'Bad'))


### Conductivity
df <- df %>% mutate(
  Conductivity = case_when(
    Conductivity < 0.0196 ~ 'Good',
    Conductivity >= 0.0196 ~ 'Warning',
    Conductivity > 0.0204 ~ 'Bad',
    TRUE ~ 'NA'))

df$Conductivity <- factor(df$Conductivity,
                          levels = c('Warning', 'Good', 'Bad'),
                          labels = c('Warning', 'Good', 'Bad'))

### Shrinkage
df$Shrinkage <- case_when(df$Shrinkage < -1.00 ~ 'Bad',
                          df$Shrinkage < -0.80 ~ 'Warning',
                          df$Shrinkage < 0.00 ~ 'Good',
                          TRUE ~ 'NA')

df$Shrinkage <- factor(df$Shrinkage,
                       levels = c('Warning', 'Good', 'Bad'),
                       labels = c('Warning', 'Good', 'Bad'))


### Encoding : Tissue.Type
addmargins(table(df$Tissue.Type))
df$Tissue.Type <- as.numeric(df$Tissue.Type)
addmargins(table(df$Tissue.Type))
table(is.na(df))
df <- na.omit(df)            



## Splitting the dataset into train and test set
set.seed(1975)
index <- sample(1:nrow(df), nrow(df)*0.75, replace = F)
train <- df[index, ]
test <- df[-index, ]
train.origin <- train
test.origin <- test

## Feature Selection
train <- train[ , -c(1:4, 13:14, 32:34)]
test <- test[ , -c(1:4, 13:14, 32:34)]
colnames(train)


## Scaling Features
center = apply(train[ , 1:25], 2, mean)
scale = apply(train[ , 1:25], 2, sd)
train[ , -c(26:28)] <- scale(train[ , -c(26:28)], center = center, scale = scale)
test[ , -c(26:28)] <- scale(test[ , -c(26:28)], center = center, scale = scale)
head(train, 3)
head(test, 3)
dim(train)
dim(test)



# Hyper-Parameters Optimization
## 2. Shrinkage
type = 'C-classification'
kernel = c('radial', 'polynomial')
cost = seq(1, 10, by = 1)
gamma = seq(0.02, 0.2, by = 0.02)
epsilon = seq(0.02, 0.2, by = 0.02)
doe = expand.grid(kernel, cost, gamma, epsilon) %>% rename('kernel' = Var1, 
                                                           'cost' = Var2,
                                                           'gamma' = Var3,
                                                           'epsilon' = Var4)
kappa <- rep(NA, times = 2000)
accuracy <- rep(NA, times = 2000)
table2 <- cbind(doe, kappa, accuracy)

for(i in 1:2000){
  model2 = svm(Shrinkage ~ ., data = train[ , c(1:25, 27)],
               type = type,
               kernel = table2[i, 1],
               cost = table2[i, 2],
               gamma = table2[i, 3],
               epsilon = table2[i, 4])
  
  y_pred2 = predict(model2, newdata = test[ , c(1:25)])
  cm2 = confusionMatrix(test$Shrinkage, y_pred2)
  table2[i, 5] <- cm2$overall[2]
  table2[i, 6] <- cm2$overall[1]
}

table2 <- table2 %>% mutate(score = kappa * accuracy)
table2[ ,c(5:7)] <- format(round(table2[ , c(5:7)], 2) , nsmall = 2)
best2 <- table2[which.max(table2$score), ]
cat('Results on Hyper-Parameter Optimization Simulation')
datatable(table2)
cat('\n Best Hyper-Parameters \n')
best2


# Modeling with SVM Algorithm
## 2. Shrinkage
svm2 <- svm(Shrinkage ~ ., data = train[ , c(1:25, 27)],
            type = type,
            kernel = best2[1, 1],
            cost = best2[1, 2],
            gamma = best2[1, 3],
            epsilon = best2[1, 4])
print(svm2)



## Performance of classifiers
y_pred2 = predict(svm2, newdata = test[ , c(1:25)])
cm2 = confusionMatrix(test$Shrinkage, y_pred2)
print(cm2)


# SVM with Bagging
new.result <- data.frame(v1 = rep(NA, times = 140),
                         v2 = rep(NA, times = 140),
                         v3 = rep(NA, times = 140),
                         v4 = rep(NA, times = 140),
                         v5 = rep(NA, times = 140),
                         v6 = rep(NA, times = 140),
                         v7 = rep(NA, times = 140),
                         v8 = rep(NA, times = 140),
                         v9 = rep(NA, times = 140),
                         v10 = rep(NA, times = 140),
                         v11 = rep(NA, times = 140),
                         v12 = rep(NA, times = 140),
                         v13 = rep(NA, times = 140),
                         v14 = rep(NA, times = 140),
                         v15 = rep(NA, times = 140),
                         v16 = rep(NA, times = 140),
                         v17 = rep(NA, times = 140),
                         v18 = rep(NA, times = 140),
                         v19 = rep(NA, times = 140),
                         v20 = rep(NA, times = 140),
                         v21 = rep(NA, times = 140),
                         v22 = rep(NA, times = 140),
                         v23 = rep(NA, times = 140),
                         v24 = rep(NA, times = 140),
                         v25 = rep(NA, times = 140),
                         v26 = rep(NA, times = 140),
                         v27 = rep(NA, times = 140),
                         v28 = rep(NA, times = 140),
                         v29 = rep(NA, times = 140),
                         v30 = rep(NA, times = 140),
                         final = rep(NA, times = 140))


for(i in 1:30){
  index <- sample(1:nrow(train), nrow(train)*1.0, replace = T)
  new.train <- train[index, ]
  new.svm <- svm(Shrinkage ~ ., data = new.train[ , c(1:25, 27)],
                 type = type,
                 kernel = best2[1, 1],
                 cost = best2[1, 2],
                 gamma = best2[1, 3],
                 epsilon = best2[1, 4])
  new.pred = predict(new.svm, newdata = test[ , c(1:25)])
  new.result[ , i] <- new.pred
}

final <- apply(new.result, 1, function(x) names(which.max(table(x))))
new.result$final <- final

new.cm = confusionMatrix(as.factor(new.result$final), test$Shrinkage)
print(new.cm)
