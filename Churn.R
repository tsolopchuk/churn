#load and explore datat 
churn_data <- read.csv('churn_data.csv', stringsAsFactors = FALSE)

library(ggplot2)
#3 continious vars+ 4 categorical vars. 
cols <- c("#00AFBB",  "#FC4E07")  
churn <- as.factor(churn_data$Churn)
pairs(churn_data[,c(2,7,8)], pch =19, cex =0.5, col = cols[churn],
      lower.panel = NULL, labels = c("Tenure", "Monthly Charges", "Total Charges"),
      main = "Continious Variables")


require(gridExtra)
bar_Contract <- ggplot(churn_data, aes(x=Contract, fill = Churn))+ 
  geom_bar(position = "fill")
bar_PhoneService <- ggplot(churn_data, aes(x=PhoneService, fill = Churn))+ 
  geom_bar(position = "fill")
bar_PaymentMethod <- ggplot(churn_data, aes(x=PaymentMethod, fill = Churn))+
  geom_bar(position = "fill")
bar_PaperlessBilling <- ggplot(churn_data, aes(x=PaperlessBilling, fill=Churn))+
  geom_bar(position = "fill")
grid.arrange(bar_Contract, bar_PhoneService, bar_PaymentMethod,bar_PaperlessBilling,
             ncol=2 ,nrow=2, top="Categorical Variables")


#########################################################################

#K-Nearest Neighbour 
dataKNN <- churn_data

#convert to boolean variables
dataKNN$Churn <- ifelse(dataKNN$Churn == "Yes",1,0)
dataKNN$PhoneService <- ifelse(dataKNN$PhoneService == "Yes",1,0) 
dataKNN$PaperlessBilling <- ifelse(dataKNN$PaperlessBilling=='Yes',1,0)
dataKNN$Contract <- ifelse (dataKNN$Contract == "One year",1,0)
dataKNN$PaymentMethod <- ifelse(dataKNN$PaymentMethod == "Mailed check",1,0)


summary(dataKNN$tenure)
summary(dataKNN$MonthlyCharges)
summary(dataKNN$TotalCharges)

#normalize data for non-binary variables
normalize <- function(x){
  return((x-min(x)) / (max(x) - min(x)))
}
dataKNN$tenure <- normalize(dataKNN$tenure)
dataKNN$MonthlyCharges <- normalize(dataKNN$MonthlyCharges)

summary(dataKNN$TotalCharges)
library(imputeTS)

dataKNN$TotalCharges <- na_mean(dataKNN$TotalCharges, option="mean")
dataKNN$TotalCharges <- normalize(dataKNN$TotalCharges)

dataKNN <- dataKNN[,-1] #remove ids 

#split into train & test datasets
RNGversion("3.5.2") ; set.seed(123)
train_sample <- sample(7043, 4403)
train_KNN <- dataKNN[train_sample, ]
test_KNN <- dataKNN[-train_sample, ]

table(testKNN$Churn)

train_KNN_labels <- train_KNN[,8]
test_KNN_lables <- test_KNN[,8]

library(class)
knn_pred <- knn(train=train_KNN[1:7], test=test_KNN[1:7], cl=train_KNN_labels, k=66) #sq root of number of observations in train

library(gmodels)
CrossTable(x=test_KNN_lables, y=knn_pred, prop.chisq=FALSE, prop.c = FALSE, prop.r=FALSE)

confm_knn <- (table(knn_pred, test_KNN$Churn))
confm_knn

#(1): k=66, FP = 0.065, FN = 0.137, Err=0.202
#(2): k=50, FP = 0.066, FN = 0.136, Err=0.202
#(3): k=70, FP = 0.066, FN = 0.139, Err=0.205
#(4): k=100, FP = 0.055, FN = 0.151, Err=0.206
#(5): k=150, FP = 0.055, FN = 0.154, Err=0.209
#(6): k=30, FP = 0.081, FN = 0.130, Err=0.211

library(caret)
confusionMatrix(knn_pred, as.factor(test_KNN_lables), positive="1")

improve_knn <- train(Churn ~., data=train_KNN, method ="knn")
improve_knn

#######################################################################################

#naiveB 
library(e1071)
dataNB <- read.csv('C:\\Users\\mattn\\Desktop\\DAP\\Classification\\churn_data.csv', 
                   stringsAsFactors = TRUE)
dataNB <- dataNB[,-1] #remove IDs

par(mfrow=c(1,3))
plot_tenure <- boxplot(dataNB$tenure, col ="gold", notch=TRUE, whisklty = 1, boxlty = 0, medcol="dark green",
        staplelwd = 4, outpch = 8, outcex = 3, main="Tenure")
summary(dataNB$tenure)
dataNB$tenure <- (ifelse(dataNB$tenure < 32.37, "Newbie", "Tenured"))

plot_monthly <- boxplot(dataNB$MonthlyCharges, col ="gold", notch=TRUE, whisklty = 1, boxlty = 0, 
        staplelwd = 4, outpch = 8, outcex = 3, medcol="dark green", main="Monthly Charges")
summary(dataNB$MonthlyCharges)
dataNB$MonthlyCharges <- (ifelse(dataNB$MonthlyCharges < 64.76, "Low", "High"))

plot_total <- boxplot(dataNB$TotalCharges, col ="gold", notch=TRUE, whisklty = 1, boxlty = 0, 
        staplelwd = 4, outpch = 8, outcex = 3, medcol="dark green", main="Total Charges")
summary(dataNB$TotalCharges)
dataNB$TotalCharges<- (ifelse(dataNB$TotalCharges < 1397.5, "Low", "High"))



RNGversion("3.5.2") ; set.seed(123)
train_sample <- sample(7043, 4403)
trainNB <- dataNB[train_sample, ]
testNB <- dataNB[-train_sample, ]

trainNB_labels <- trainNB[,8]
testNB_labels <- testNB[,8]
naiveB <- naiveBayes(trainNB[1:7], trainNB_labels) 
naiveB_pred <- predict(naiveB, testNB)
#naiveB_pred <- as.numeric(ifelse(naiveB_pred==2,1,0))
confm_naiveB <- table(naiveB_pred, testNB_labels)
confm_naiveB 

CrossTable(x=testNB_labels, y=naiveB_pred, prop.chisq=FALSE, prop.c = FALSE, prop.r=FALSE)

confusionMatrix(naiveB_pred, testNB_labels, positive="Yes")

improve_nb <- train(Churn ~., data=trainNB, method ="nb")
#######################################################################################

#Tree
data_tree <- read.csv('C:\\Users\\mattn\\Desktop\\DAP\\Classification\\churn_data.csv', 
                       stringsAsFactors = TRUE)
data_tree <- data_tree[-1]
RNGversion("3.5.2") ; set.seed(123)
train_sample <- sample(7043, 4403)
train_tree <- data_tree[train_sample, ]
test_tree <- data_tree[-train_sample, ]

prop.table(table(train_tree$Churn))
prop.table(table(test_tree$Churn))

library(C50)
tree_model <- C5.0 (train_tree[c(1,3,6,7)], train_tree$Churn, trails=10)
summary(tree_model)
plot(tree_model, subtree=1)


tree_predict <- predict(tree_model, test_tree)
CrossTable(test_tree$Churn, tree_predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

confusionMatrix(test_tree$Churn, tree_predict, positive="Yes")

improve_tree <- train(Churn ~., data=train_tree[-1], method ="C5.0")

#model improvement = trails 
###############################################################################

#Compare 3 models performance 
library(pROC)
par(mfrow=c(1,1))

naiveB_prob <- as.data.frame(predict(naiveB, testNB, type="raw"))$Yes
KNN_prob <- as.numeric(knn(train_KNN[1:7],test_KNN[1:7], train_KNN_labels,
                k=70, prob=TRUE))
tree_prob <- as.data.frame(predict(tree_model, test_tree, type="prob"))$Yes


naiveB_roc <- roc(test_KNN_lables,naiveB_prob)
plot(naiveB_roc, plot=TRUE, legacy.axes=TRUE,col="red",lwd=4)
knn_roc <- roc(test_KNN_lables, KNN_prob)
plot(knn_roc, plot= TRUE, col="dark green", add=TRUE, lwd=4)
tree_roc <- roc(test_KNN_lables, tree_prob)
plot(tree_roc, col="blue", lwd=4, add=TRUE)

auc(naiveB_roc)
auc(knn_roc)
auc(tree_roc)

