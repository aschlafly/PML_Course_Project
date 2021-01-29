library(caret)
library(tidyverse)

# Run the following for running on the Mac
library(doMC)
registerDoMC(cores = 4)

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
# run it
stopCluster(cluster)

# Read in the data
#pml_training = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
pml_training = read.csv("pml-training.csv")

# Choose only those variables corresponding to Euler metrics and raw data.
pml_grep_pattern = "^roll_|^pitch_|^yaw|^total|^accel|^gyros|^magnet|classe|problem_id"
(pml_pred_vars = grep(pml_grep_pattern, names(pml_training), value = TRUE))
(pml_pv_count = length(pml_pred_vars))

pml_small_training = pml_training[,grep(pml_grep_pattern, names(pml_training))]
pml_small_training$classe = as.factor(pml_small_training$classe)
sort(names(pml_small_training))


# Data exploration
nearZeroVar(pml_small_training, saveMetrics = TRUE)

tot_accels = pml_small_training[,c(grep("^total_accel_|classe", names(pml_small_training)))]
tot_accels %>% gather("location","total_accel",1:4) %>%
    ggplot(aes(x = total_accel, col = classe)) +
    geom_freqpoly() + facet_wrap(.~location, ncol = 2)

ggplot(data = tot_accels, aes(x = total_accel_forearm, y= total_accel_dumbbell, col = classe)) +
    geom_point(alpha=0.2, size = .5, position = "jitter")

accels = pml_small_training[,c(grep("^accel_|classe", names(pml_small_training)))]
accels %>% gather("location","accel",1:12) %>%
    ggplot(aes(x = accel, col = classe)) +
    geom_freqpoly() + facet_wrap(.~location, ncol = 3)

gyros = pml_small_training[,c(grep("^gyros_|classe", names(pml_small_training)))]
gyros %>% gather("location","gyros",1:12) %>%
    ggplot(aes(x = gyros, col = classe)) +
    geom_freqpoly() + facet_wrap(.~location, ncol = 3)

magnet = pml_small_training[,c(grep("^magnet_|classe", names(pml_small_training)))]
magnet %>% gather("location","magnet",1:12) %>%
    ggplot(aes(x = magnet, col = classe)) +
    geom_freqpoly() + facet_wrap(.~location, ncol = 3)

featurePlot(x = pml_small_training[,c("total_accel_belt","total_accel_arm","total_accel_dumbbell","total_accel_forearm")],
            y = pml_small_training$classe, plot = "pairs", alpha = 0.1, size = 0.2)


# Split training data into training and testing data.

set.seed(32343)
inTrain = createDataPartition(y = pml_small_training$classe, p = 0.7, list = FALSE)
pml_train = pml_small_training[inTrain,]
pml_validation = pml_small_training[-inTrain,]


# Train models
pml_pc = prcomp(pml_small_training[,-pml_pv_count])
plot(pml_pc$x[,1], pml_pc$x[,2], col = pml_small_training$classe, cex = 0.2)

pml_pc_fit = train(pml_train$classe~ ., method = "glm", preProcess="pca", data = pml_train)
confusionMatrix(pml_train$classe, predict(pml_pc_fit, pml_train))


system.time({pml_tree_fit = train(classe ~ ., data = pml_train, method = "rpart")})
print(pml_tree_fit$finalModel)
fancyRpartPlot(pml_tree_fit$finalModel)
confusionMatrix(pml_train$classe, predict(pml_tree_fit, pml_train))
confusionMatrix(pml_validation$classe, predict(pml_tree_fit, pml_validation))


library(doMC)
registerDoMC(cores = 4)

system.time({pml_rf_fit = train(classe ~ ., method = "rf", data = pml_train)})
confusionMatrix(pml_train$classe,predict(pml_rf_fit, pml_train))
confusionMatrix(pml_validation$classe, predict(pml_rf_fit, pml_validation))


system.time({pml_gbm_fit = train(classe~., method = "gbm", data = pml_train, verbose = FALSE)})
print(pml_gbm_fit)
confusionMatrix(pml_train$classe,predict(pml_gbm_fit, pml_train))
confusionMatrix(pml_validation$classe,predict(pml_gbm_fit, pml_validation))


#Don't read in pml_testing until later
#pml_testing  = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))
pml_testing  = read.csv("pml-testing.csv")

predict(pml_tree_fit, pml_testing)
predict(pml_rf_fit, pml_testing)
predict(pml_gbm_fit, pml_testing)

#pml_small_testing = pml_testing[,grep(pml_grep_pattern, names(pml_testing))]
#sort(names(pml_small_testing))
