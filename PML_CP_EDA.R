library(caret)
library(tidyverse)
library(e1071)
library(rattle)

# Read in the data
#pml_training = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
#pml_testing  = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))
#pml_classes = c("character","character","integer","integer","character","character","integer",
#                rep("numeric",152), "factor")
pml_training = read.csv("pml-training.csv", colClasses = pml_classes, na.strings = c("\"\"","NA"))
pml_testing  = read.csv("pml-testing.csv", colClasses = pml_classes, na.strings = c("\"\"","NA"))
pml_training = read.csv("pml-training.csv")
pml_testing  = read.csv("pml-testing.csv")
pml_training$X = as.integer(pml_training$X)
pml_training$user_name = as.character(pml_training$user_name)
pml_training$raw_timestamp_part_1 = as.integer(pml_training$raw_timestamp_part_1)
pml_training$raw_timestamp_part_2 = as.integer(pml_training$raw_timestamp_part_2)
pml_training$cvtd_timestamp = as.character(pml_training$cvtd_timestamp)
pml_training$new_window = as.character(pml_training$new_window)
pml_training$num_window = as.integer(pml_training$num_window)

# Examine the names of the data and replace the mis-labeled variable name.
names(pml_training)
sort(names(pml_training))
names(pml_testing)
sort(names(pml_testing))
names(pml_training)[grep("skewness_roll_belt.1",names(pml_training))] = "skewness_pitch_belt"
names(pml_testing)[grep("skewness_roll_belt.1",names(pml_testing))] = "skewness_pitch_belt"


new_pml_training = pml_training %>% dplyr::filter(new_window == "yes")
new_pml_training = pml_training[pml_training$new_window == "yes",]
nearZeroVar(new_pml_training, names = TRUE)
not_new_pml_training = pml_training %>% dplyr::filter(new_window == "no")
nzv_1 = nearZeroVar(not_new_pml_training, names = FALSE)
sort(names(pml_training)[-nzv_1])

sort(nearZeroVar(not_new_pml_training, names = TRUE))

sum(complete.cases(pml_training))

grep("_[xyz]$",names(pml_training), value = TRUE)
grep("^accel",names(pml_training), value = TRUE)
grep("^gyros",names(pml_training), value = TRUE)
grep("^magnet",names(pml_training), value = TRUE)
grep("^skewness",names(pml_training), value = TRUE)
grep("^stdev",names(pml_training), value = TRUE)

grep("_total_",names(pml_training), value = TRUE)

sort(grep("_pi[tc]+h_",names(pml_training), value = TRUE))
sort(grep("_roll_",names(pml_training), value = TRUE))
sort(grep("_yaw_",names(pml_training), value = TRUE))
cbind(sort(grep("_pi[tc]+h_",names(pml_training), value = TRUE)),
      sort(grep("_roll_",names(pml_training), value = TRUE)),
      sort(grep("_yaw_",names(pml_training), value = TRUE)))

grep("forearm",names(pml_training), value = TRUE) 
grep("_arm",names(pml_training), value = TRUE) 
grep("belt",names(pml_training), value = TRUE) 
grep("dumbbell",names(pml_training), value = TRUE) 
cbind(sort(grep("belt",names(pml_training), value = TRUE)),
      sort(grep("_arm",names(pml_training), value = TRUE)),
      sort(grep("dumbbell",names(pml_training), value = TRUE)),
      sort(grep("forearm",names(pml_training), value = TRUE)))


summary(pml_training[,grep("kurtosis",names(pml_training))]) #None has meaningful values
summary(pml_training[,grep("yaw",names(pml_training))]) #Only yaw_xxxx has meaningful values

grep("kurtosis",names(pml_training), value = TRUE)

table(pml_training$classe)

#Consistent names?
setdiff(names(pml_testing), names(pml_training))
setdiff(names(pml_training), names(pml_testing))

#make sure to cite
# find no variance variables

nzvs = nearZeroVar(pml_training, names = FALSE)
sort(names(pml_training)[nzvs])
sort(names(pml_training)[-nzvs])

# make sure it is not idiosyncratic
table(pml_training$user_name, pml_training$classe)
prop.table(table(pml_training$user_name, pml_training$classe), margin = 1)

ggplot(data = pml_training) +
    geom_boxplot(mapping = aes(x = classe, y = roll_forearm))


summary(lm(I(total_accel_belt^2) ~ I(accel_belt_x^2) + I(accel_belt_y^2) + I(accel_belt_z^2)-1, data = pml_training))
plot(I(total_accel_belt^2) ~ I(accel_belt_x^2) + I(accel_belt_y^2) + I(accel_belt_z^2)-1, data = pml_training)

atan(pml_testing$accel_belt_x/sqrt(pml_testing$accel_belt_y*2 + pml_testing$accel_belt_z))
pml_testing$roll_belt


# Verify acceleration is as expected
c_accel_belt = sqrt(pml_training$accel_belt_x^2 + pml_training$accel_belt_y^2 + pml_training$accel_belt_z^2)
plot(c_accel_belt, pml_training$total_accel_belt)
c_accel_arm = sqrt(pml_training$accel_arm_x^2 + pml_training$accel_arm_y^2 + pml_training$accel_arm_z^2)
plot(c_accel_arm, pml_training$total_accel_arm)
c_accel_dumbbell = sqrt(pml_training$accel_dumbbell_x^2 + pml_training$accel_dumbbell_y^2 + pml_training$accel_dumbbell_z^2)
plot(c_accel_dumbbell, pml_training$total_accel_dumbbell)
c_accel_forearm = sqrt(pml_training$accel_forearm_x^2 + pml_training$accel_forearm_y^2 + pml_training$accel_forearm_z^2)
plot(c_accel_forearm, pml_training$total_accel_forearm)
#Note Belt has a bimodal distribution
ggplot(data = pml_training, aes(x = total_accel_belt, color = user_name)) + 
  geom_freqpoly()

c_pitch_belt = atan(pml_training$accel_belt_x/sqrt(pml_training$accel_arm_y^2+pml_training$accel_arm_z^2))
plot(c_pitch_belt, pml_training$pitch_belt)
c_roll_belt = atan(pml_training$accel_belt_y/sqrt(pml_training$accel_arm_x^2+pml_training$accel_arm_z^2))
plot(c_roll_belt, pml_training$roll_belt)


ftable(xtabs( ~  new_window + classe + user_name, data = pml_training),
       row.vars = 3, col.vars = c(1,2))

table(pml_training$num_window, pml_training$user_name)
table(pml_training$num_window, pml_training$new_window)
range(pml_training$num_window)



# Data Exploration
qplot(avg_roll_dumbbell,colour=classe,data=new_pml_training,geom="density")
qplot(avg_pitch_dumbbell,colour=classe,data=new_pml_training,geom="density")
qplot(avg_yaw_dumbbell,colour=classe,data=new_pml_training,geom="density")


# Try some training
new_pml_training$classe = as.factor(new_pml_training$classe)
nearZeroVar(new_pml_training,saveMetrics=TRUE)
pml_tree_fit = train(classe ~ ., method = "rpart", 
                     data = new_pml_training[-nearZeroVar(new_pml_training, names = FALSE),])
fancyRpartPlot(pml_tree_fit$finalModel)

small_pml_training = new_pml_training %>% select(!starts_with("kurtosis") & !starts_with("skewness")) %>% 
  select(8:classe)
nearZeroVar(small_pml_training, saveMetrics = TRUE)

pml_tree_fit = train(classe ~ ., method = "rpart", data = small_pml_training)
fancyRpartPlot(pml_tree_fit$finalModel)
confusionMatrix(small_pml_training$classe,predict(pml_tree_fit, small_pml_training))

pml_rf_fit = train(classe ~ ., method = "rf", data = small_pml_training)
confusionMatrix(small_pml_training$classe,predict(pml_rf_fit, small_pml_training))

