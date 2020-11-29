
# Importing libraries
library(randomForest)
library(caret)

# Importing the data set
rfdata <- read.csv("Data_Cortex_Nuclear.csv")
rfdata = rfdata[,c('SOD1_N','APP_N','pPKCG_N','pERK_N','pCAMKII_N','CaNA_N','Tau_N','pP70S6_N','pNUMB_N', 'BRAF_N', 'Ubiquitin_N', 'AKT_N','S6_N',
                 'AcetylH3K9_N','class')]

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(rfdata$class, p=0.5, list = FALSE)
TrainingSet <- rfdata[TrainingIndex,] # Training Set
TestingSet <- rfdata[-TrainingIndex,] # Test Set


data.imputed.train <- rfImpute(class ~.,data = TrainingSet);

data.imputed.test <- rfImpute(class ~ .,data = TestingSet);


#model.rf <- randomForest(class~.,data = data.imputed.train, ntree = 127, importance = TRUE)


write.csv(data.imputed.train, "training.csv")
write.csv(data.imputed.test, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Building Random forest model

model <- randomForest(class ~ ., data = TrainSet, ntree = 127, importance = TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")
