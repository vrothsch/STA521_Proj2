# STA521 PROJECT 2
# R Script
# Andrew Kenealy & Viola Rothschild

#### LOAD PACKAGES 
library(dplyr)
library(readr)
library(ggplot2)
library(visdat)
library(tidyr)
library(patchwork)
library(factoextra)
library(stargazer)
library(cluster)
library(caret)
library(MASS)
library(class)
library(pROC)
library(ROCR)
library(GGally)
library(gridExtra)
library(ggpubr)
library(class)
library(rpart)

#### SET WORKING DIRECTORY
# setwd("~/Documents/Duke/F22 STA521/Project 2/image_data")
setwd("~/Dropbox/STA 521/Project 2/AK and VR Project 2 work/Data and Analysis")

#### ORGANIZE DATA 

# Load data
image1 <- read.table("imagem1.txt")
image2 <- read.table("imagem2.txt")
image3 <- read.table("imagem3.txt")

# Rename features
cols = c("y", "x", "exp_label", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN")
colnames(image1) <- cols
colnames(image2) <- cols
colnames(image3) <- cols

# Create image id variable
image1$image <- 1
image2$image <- 2
image3$image <- 3

#### EXPLORATORY DATA ANALYSIS

# Calculate percent of Cloud (1) vs. No Cloud (-1) vs. No label (0) in Expert Label for each image (TABLE 1)
# Note: table of values created in Overleaf  

#image 1
table(image1$exp_label)
#no cloud/total = 37.25%
42882/115110
# no label/total = 28.63%
32962/115110 
# cloud/total = 34.11 %
39266/115110

#image 2
table(image2$exp_label)
# no cloud/total = 43.78%
50446/115229
# no label/total = 38.46%
44312/115229
# cloud/total = 17.77%
20471/115229

#image 3
table(image3$exp_label)
#no cloud/total = 29.29%
33752/115217
#no label/total = 52.27%
60221/115217
# cloud/total = 18.44%
21244/115217

# total
#cloud = 23.43%
(39266 + 20471 + 21244 )/ 345556
#no cloud = 36.77%
(42882 + 50446+33752)/345556
#no label = 39.79%
(44312 + 60221 +32962)/345556

# Plot images (FIGURE 1)

# Image 1

image1_plot = ggplot(image1) +
  geom_point(aes(x, y, col=factor(exp_label))) +
  scale_color_manual(labels=c("Cloud", "No label", "No cloud"), values=c("light blue", "grey", "white")) +
  theme(
    plot.title = element_text(color="black", size=18),
    panel.ontop=TRUE, 
    panel.background = element_rect(fill=NA),
    axis.text = element_text(size=12, color="black"),
    axis.title = element_text(size=12, color="black")
  ) +
  scale_x_continuous(name="X Coordinate", breaks= seq(0, max(image1$x), by=50)) +
  scale_y_continuous(name="Y Coordinate", breaks = seq(0, max(image1$y), by=50)) +
  labs(title = "Image 1", color = "Expert Label")

image1_plot

# Image 2

image2_plot = ggplot(image2) +
  geom_point(aes(x, y, col=factor(exp_label))) +
  scale_color_manual(labels=c("Cloud", "No label", "No cloud"), values=c("light blue", "grey", "white")) +
  theme(
    plot.title = element_text(color="black", size=18),
    panel.ontop=TRUE, 
    panel.background = element_rect(fill=NA),
    axis.text = element_text(size=12, color="black"),
    axis.title = element_text(size=12, color="black")
  ) +
  scale_x_continuous(name="X Coordinate", breaks= seq(0, max(image2$x), by=50)) +
  scale_y_continuous(name="Y Coordinate", breaks = seq(0, max(image2$y), by=50)) +
  labs(title = "Image 2", color = "Expert Label")

image2_plot


# Image 3

image3_plot = ggplot(image3) +
  geom_point(aes(x, y, col=factor(exp_label))) +
  scale_color_manual(labels=c("Cloud", "No label", "No cloud"), values=c("light blue", "grey", "white")) +
  theme(
    plot.title = element_text(color="black", size=18),
    panel.ontop=TRUE, 
    panel.background = element_rect(fill=NA),
    axis.text = element_text(size=12, color="black"),
    axis.title = element_text(size=12, color="black")
  ) +
  scale_x_continuous(name="X Coordinate", breaks= seq(0, max(image3$x), by=50)) +
  scale_y_continuous(name="Y Coordinate", breaks = seq(0, max(image3$y), by=50)) +
  labs(title = "Image 3", color = "Expert Label")

image3_plot

# combine image plots 

ggarrange(image1_plot, image2_plot, image3_plot, nrow=1, common.legend=TRUE, legend="right")

# Examine pairwise relationships between features

# Remove data points with no expert label for correlation plots for clearer visualization
explab_1 = image1[image1$exp_label != 0,]
explab_2 = image2[image2$exp_label != 0,]
explab_3 = image3[image3$exp_label != 0,]

# create master dataset that includes data from all 3 images with expert labeled data only 
full_set_lab = bind_rows(explab_1, explab_2, explab_3)

# Plot pairwise interactions of features with colors indicating different labels (FIGURE 2)
# We use # around this because it takes a long time to plot and impedes smooth running of the rest of the code

#corr_plot <- ggpairs(
 #full_set_lab[,c(3, 4, 5, 6, 7, 8, 9, 10, 11)], 
 #mapping=aes(alpha=0.001, color=factor(exp_label, labels=c("No Cloud", "Cloud"))), 
 #columns = c("NDAI", "SD", "CORR","DF","CF","BF","AF","AN"))

 #corr_plot

#### DATA PREPARATION 

# Change -1 no-cloud label to 0
full_set_lab$exp_label[full_set_lab$exp_label == -1] <- 0


## Horizontal Slice (6 slices: 1 test, 1 validation, 4 training)

set.seed(12345)

# create function
hor_slice <- function(data, A) {
  y <- data$y
  y_range <- max(y) - min(y)
  indices <- seq(min(y), max(y), by = y_range %/% A)
  
  slices <- list()
  for (i in 1:(length(indices)-1)) {
    slices[[i]] <- which(y >= indices[i] & y < indices[i+1])
  }
  return (slices)
}

# run slice for 8 panes

horizontal_index <- hor_slice(full_set_lab, 8)
test_horizontal <- full_set_lab[horizontal_index[[2]],] # test slice 2
val_horizontal <- full_set_lab[horizontal_index[[3]],] # test slice 3
train_horizontal <- full_set_lab[-c(horizontal_index[[2]], horizontal_index[[3]]),] # put remaining 6 slices into training data

## Block Method (64 total blocks, 8 test, 8 validation, 48 training)

# create function

block_split <- function(data, A){
  length_x <- max(data$x) - min(data$x); length_y = max(data$y) - min(data$y)
  x_edge <- matrix(NA, A, 2); y_edge <- matrix(NA, A, 2) 
  for(i in 1:A){
    x_edge[i, 1] <- floor((length_x/A)*(i-1)) +  min(data$x)
    x_edge[i, 2] <- floor((length_x/A)*i) +  min(data$x)
    y_edge[i, 1] <- floor((length_y/A)*(i-1)) +  min(data$y)
    y_edge[i, 2] <- floor((length_y/A)*(i)) +  min(data$y)
}
  
bounds <- data.frame(x_edge_lower = rep(x_edge[,1], A),
                       x_edge_upper = rep(x_edge[,2], A),
                       y_edge_lower = sort(rep(y_edge[,1], A)),
                       y_edge_upper = sort(rep(y_edge[,2], A)))
  
# create A*A boxes
  
  blocks <- list()
  for(i in 1:(A*A)){
    blocks[[i]] <- which((data$x >= bounds[i,1] & data$x < bounds[i,2]) & (data$y >= bounds[i,3] & data$y < bounds[i,4]))
  }
  
# assign each box into one of the partitions
  
fold_index <- createFolds(1:(A*A), A)
  folds <- list()
  for(i in 1:A){
    boxes <- fold_index[[i]]
    appended <- vector("numeric")
    for(j in boxes){
      appended <- append(appended, blocks[[j]])
    }
    folds[[i]] <- appended
  }
  
  return(folds)
}

# block partition
set.seed(12345)
block_index <- block_split(full_set_lab, 8)
test_block <- full_set_lab[block_index[[2]],] 
val_block <- full_set_lab[block_index[[3]],] 
train_block <- full_set_lab[-c(block_index[[2]], block_index[[3]]),] 

## Trivial Classifier

trivial_horizontal <- round((nrow(val_horizontal[val_horizontal$exp_label == 0,]) + nrow(test_horizontal[test_horizontal$exp_label == 0,]))/(nrow(val_horizontal) + nrow(test_horizontal)),3)
# Trivial Horizontal partition: 0.493
trivial_block <- round((nrow(val_block[val_block$exp_label == 0,]) + nrow(test_block[test_block$exp_label == 0,]))/(nrow(val_block)+nrow(test_block)),3)
# Trivial Blocked partition: 0.604

## First Order Importance


# Linear Probability model (OLS on binary DV for TABLE 2)

fit1 <- lm(exp_label ~ NDAI + SD + CORR + DF + CF + BF + AF + AN, data = full_set_lab)

stargazer::stargazer(fit1, type = "latex",
                     title = "Linear Probability Model",
                     dep.var.labels = "Cloud",
                     omit.stat = c("f", "adj.rsq", "ser"))

# Also calculate correlations of features--results input directly into Overleaf (TABLE 2)
corr_table = data.frame(cor(full_set_lab$exp_label, full_set_lab$NDAI),
                   cor(full_set_lab$exp_label, full_set_lab$SD),
                   cor(full_set_lab$exp_label, full_set_lab$CORR),
                   cor(full_set_lab$exp_label, full_set_lab$DF),
                   cor(full_set_lab$exp_label, full_set_lab$CF),
                   cor(full_set_lab$exp_label, full_set_lab$BF),
                   cor(full_set_lab$exp_label, full_set_lab$AF),
                   cor(full_set_lab$exp_label, full_set_lab$AN))

# Box plots (FIGURE 3)

box1 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = NDAI)) +
  geom_boxplot(
color="blue",
fill="blue",
alpha=0.2) + 
  xlab("Cloud") + ggtitle("NDAI") 

box2 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = SD)) +
  geom_boxplot(
    color="tomato3",
    fill="tomato3",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("SD") 

box3 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = CORR)) +
  geom_boxplot(
    color="springgreen2",
    fill="springgreen2",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("COOR") 

box4 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = DF)) +
  geom_boxplot(
    color="gold2",
    fill="gold2",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("DF") 

box5 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = CF)) +
  geom_boxplot(
    color="deeppink2",
    fill="deeppink2",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("CF") 

box6 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = BF)) +
  geom_boxplot(
    color="aquamarine4",
    fill="aquamarine4",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("BF") 

box7 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = AF)) +
  geom_boxplot(
    color="plum4",
    fill="plum4",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("AF") 

box8 <- ggplot(full_set_lab, aes(x = factor(exp_label), y = AN)) +
  geom_boxplot(
    color="cyan",
    fill="cyan",
    alpha=0.2) + 
  xlab("Cloud") + ggtitle("AN") 

grid.arrange(box1, box2, box3, box4, box5, box6, box7, box8, nrow = 2, ncol = 4, top="Box Plots of Predictors by Cloud Label (0 = no cloud; 1 = cloud)")


# See CVMaster file for CV function.

#### MODELING


# Merge training and validation data
block_train_all <- rbind(train_block, val_block)
hor_train_all <- rbind(train_horizontal, val_horizontal)

# Define general features for variable input
features <- paste("factor(exp_label) ~ NDAI + CORR + AN")

## Logistic Regression 

# Create logistic regression function
log_reg <- function(features, train_dat, test_dat) {
  mod <- glm(features, train_dat, family="binomial")
  probabilites <- predict(mod, test_dat, type="response")
  predictions <- ifelse(probabilites > 0.5, 1, 0)
  return(predictions)
}

## Linear Discriminant Analysis

# Create LDA function
lda_reg <- function(features, train_dat, test_dat) {
  mod <- lda(as.formula(features), data=train_dat)
  predictions <- predict(mod, test_dat)
  return(predictions$class)
}

## Quadratic Discriminant Analysis

# Create QDA function
qda_reg <- function(features, train_dat, test_dat) {
  mod <- qda(as.formula(features), data=train_dat)
  predictions <- predict(mod, test_dat)
  return(predictions$class)
}

## Decision Tree

# Create DT function
dt_reg <- function(features, train_dat, test_dat) {
  mod <- rpart(as.formula(features), data=train_dat, method = "class")
  predictions <- predict(mod, test_dat, type = "class")
  return(predictions)
}


## Train Classifiers

# For loop to run classification models with CVmaster function
results <- data.frame(rep(NA, 5))
approach <- c(log_reg, lda_reg, qda_reg, dt_reg)

for(i in 1:length(approach)){
  res1 <- as.data.frame(matrix(NA, nrow = 5, ncol = 2))
  res1[,1] <- round(CVmaster(approach[[i]], features, hor_train_all, 5, acc, hor_slice)$metric, 3)
  res1[,2] <- round(CVmaster(approach[[i]], features, block_train_all, 5, acc, block_split)$metric, 3)
  results <- cbind(results, res1)
}
results <- results[,-1] # drop extra column
colnames(results) <- c("log.horizontal", "log.block", "lda.horizontal", "lda.block", "qda.horizontal", "qda.block", "dt.horizontal", "dt.block")


# get overall CV means
results <- rbind(results, round(colMeans(results), 3))

## Test Classifiers

#Create Test Error function, similar to CVmaster

TestErrorFunc <- function(classify, features, train_dat, test_dat, acc) {
  predicts <- classify(features, train_dat, test_dat)
  validation_error <- acc(predicts, test_dat$exp_label)
  comparison <- data.frame("predicted" = predicts, "true" = test_dat$exp_label) %>%
    mutate("false.pos" = ifelse((predicted == 1 & true == 0), 1, 0),
           "false.neg" = ifelse((predicted == 0 & true == 1), 1, 0),
           "true.pos" = ifelse((predicted == 1 & true == 1), 1, 0),
           "true.neg" = ifelse((predicted == 0 & true == 0), 1, 0))
  fpr = sum(comparison$false.pos)/nrow(comparison %>% filter(true == 0))
  fnr = sum(comparison$false.neg)/nrow(comparison %>% filter(true == 1))
  tpr = sum(comparison$true.pos)/nrow(comparison %>% filter(true == 1))
  tnr = sum(comparison$true.neg)/nrow(comparison %>% filter(true == 0))
  return(list("validation_error" = validation_error, "comparison" = comparison, "stats" = data.frame("fpr" = fpr,
                                                                             "fnr" = fnr,
                                                                             "tpr" = tpr,
                                                                             "tnr" = tnr)))
}

# Use for loop to calculate test error

test <- data.frame(NA)
for(i in 1:length(approach)){
  te <- as.data.frame(matrix(NA, nrow = 1, ncol = 2))  
  te[,1] <- round(TestErrorFunc(approach[[i]], features, hor_train_all, test_horizontal, acc)$validation_error, 3)
  te[,2] <- round(TestErrorFunc(approach[[i]], features, block_train_all, test_block, acc)$validation_error, 3)
  test<- cbind(test, te)
}

# append to overall results

test <- test[,-1] # drop extra column

colnames(test) <- c("log.horizontal", "log.block", "lda.horizontal", "lda.block", "qda.horizontal", 
                          "qda.block", "dt.horizontal", "dt.block")


results <- rbind(results, test)


row.names(results) <- c("F1", "F2", "F3", "F4", "F5", "Overall CV", "Test")


# Output results (TABLE 3)

stargazer(results, type = "latex", title = "Classification Performance", label = "results", summary = F)


### ROC Curves (FIGURE 4)

## Horizontal Partition

#Logistic Regression
mod1 <- glm(formula=as.formula(features), hor_train_all, family="binomial")
predictions <- prediction(predict(mod1, test_horizontal), test_horizontal$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "log.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values) 
colnames(roc.y) <- "log.y"
hor1 <- data.frame("log.x" = roc.x, 
                   "log.y" = roc.y)


#LDA
mod2 <- lda(formula=as.formula(features), data=hor_train_all)
predictions <- prediction(predict(mod2, test_horizontal)$posterior[,2], test_horizontal$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "lda.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values)
colnames(roc.y) <- "lda.y"
hor1 <- cbind(hor1, data.frame("lda.x" = roc.x, 
                               "lda.y" = roc.y))


#QDA
mod3 <- qda(formula=as.formula(features), data=hor_train_all)
predictions <- prediction(predict(mod3, test_horizontal)$posterior[,2], test_horizontal$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "qda.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values)  
colnames(roc.y) <- "qda.y"
hor1.1 <- data.frame("qda.x" = roc.x, 
                                   "qda.y" = roc.y)

#Decision Tree
mod4 <- rpart(as.formula(features), data=hor_train_all, method = "class")
predictions <- prediction(as.numeric(predict(mod4, test_horizontal, type = "class")), test_horizontal$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "dt.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values) 
colnames(roc.y) <- "dt.y"
hor1.1 <- cbind(hor1.1, data.frame("dt.x" = roc.x, 
                                   "dt.y" = roc.y))


## Block Partition


#Logistic Regression
mod5 <- glm(formula=as.formula(features), data = block_train_all, family="binomial")
predictions <- prediction(predict(mod5, test_block), test_block$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "log.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values) 
colnames(roc.y) <- "log.y"
block1 <- data.frame("log.x" = roc.x, 
                   "log.y" = roc.y)


#LDA
mod6 <- lda(formula=as.formula(features), data=block_train_all)
predictions <- prediction(predict(mod6, test_block)$posterior[,2], test_block$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "lda.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values)
colnames(roc.y) <- "lda.y"
block1 <- cbind(block1, data.frame("lda.x" = roc.x, 
                               "lda.y" = roc.y))


#QDA
mod7 <- qda(formula=as.formula(features), data=block_train_all)
predictions <- prediction(predict(mod7, test_block)$posterior[,2], test_block$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "qda.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values)  
colnames(roc.y) <- "qda.y"
block1.1 <- data.frame("qda.x" = roc.x, 
                     "qda.y" = roc.y)

#Decision Tree
mod8 <- rpart(as.formula(features), data=block_train_all, method = "class")
predictions <- prediction(as.numeric(predict(mod8, test_block, type = "class")), test_block$exp_label)
roc.x <- as.data.frame(performance(predictions, "tpr", "fpr")@x.values)
colnames(roc.x) <- "dt.x"
roc.y <- as.data.frame(performance(predictions, "tpr", "fpr")@y.values) 
colnames(roc.y) <- "dt.y"
block1.1 <- cbind(block1.1, data.frame("dt.x" = roc.x, 
                                   "dt.y" = roc.y))


### Plot ROC: Note need to make four plots; one horizontal logit and LDA; one horizontal QDA and dt; one block logit and LDA; one block QDA and DT

# Get more manageable dataframes for plotting predicted values, and prep order

set.seed(12345)

hor1 <- hor1[sample(nrow(hor1), 5000), ]
hor1.1 <- hor1.1[sample(nrow(hor1.1), 5000), ]
hor_roc <- cbind(hor1, hor1.1)

hor_roc_log <- hor_roc[,1:2]
hor_roc_log <- hor_roc_log[order(hor_roc_log$log.x),]

hor_roc_lda <- hor_roc[,3:4]
hor_roc_lda <- hor_roc_lda[order(hor_roc_lda$lda.x),]

hor_roc_qda <- hor_roc[,5:6]
hor_roc_qda <- hor_roc_qda[order(hor_roc_qda$qda.x),]

block1 <- block1[sample(nrow(block1), 5000), ]
block1.1 <- block1.1[sample(nrow(block1.1), 5000), ]
block_roc <- cbind(block1, block1.1)

block_roc_log <- block_roc[,1:2]
block_roc_log <- block_roc_log[order(block_roc_log$log.x),]

block_roc_lda <- block_roc[,3:4]
block_roc_lda <- block_roc_lda[order(block_roc_lda$lda.x),]

block_roc_qda <- block_roc[,5:6]
block_roc_qda <- block_roc_qda[order(block_roc_qda$qda.x),]


# Plot

par(mfrow = c(1,2))
plot(x = hor_roc_log$log.x, y = hor_roc_log$log.y, type = "l", col = "blue",
     main = "ROC Curve: Horizontal Partition",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate"
)
lines(x = hor_roc_lda$lda.x, y = hor_roc_lda$lda.y, lty = 2, col = "green")
lines(x = hor_roc_qda$qda.x, y = hor_roc_qda$qda.y, lty = 2, col = "red")
lines(x = c(0,.5,1), y = c(0,.5,1), lty = 1, col = "gray")
legend("bottomright", 
       col = c("blue", "green", "red"),
       lty = c(1,2, 3),
       legend = c("Logit", "LDA", "QDA"), cex = .8)


plot(x = block_roc_log$log.x, y = block_roc_log$log.y, type = "l", col = "blue",
     main = "ROC Curve: Block Partition",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate"
)
lines(x = block_roc_lda$lda.x, y = block_roc_lda$lda.y, lty = 2, col = "green")
lines(x = block_roc_qda$qda.x, y = block_roc_qda$qda.y, lty = 2, col = "red")
lines(x = c(0,.5,1), y = c(0,.5,1), lty = 1, col = "gray")
legend("bottomright", 
       col = c("blue", "green", "red"),
       lty = c(1,2, 3),
       legend = c("Logit", "LDA", "QDA"), cex = .8)



#### DIAGNOSTICS 

# Get dataframes with outcome of each observation for horizontal 

logit_df <- TestErrorFunc(log_reg, features, train_horizontal, test_horizontal, acc)
logit_df <- as.data.frame(logit_df)

lda_df <- TestErrorFunc(lda_reg, features, train_horizontal, test_horizontal, acc)
lda_df <- as.data.frame(lda_df)

qda_df <- TestErrorFunc(qda_reg, features, train_horizontal, test_horizontal, acc)
qda_df <- as.data.frame(qda_df)

dt_df <- TestErrorFunc(dt_reg, features, train_horizontal, test_horizontal, acc)
dt_df <- as.data.frame(dt_df)


# Get dataframes with outcome of each observation for blocked

logit_df_b <- TestErrorFunc(log_reg, features, train_block, test_block, acc)
logit_df_b <- as.data.frame(logit_df_b)

lda_df_b <- TestErrorFunc(lda_reg, features, train_block, test_block, acc)
lda_df_b <- as.data.frame(lda_df)

qda_df_b <- TestErrorFunc(qda_reg, features, train_block, test_block, acc)
qda_df_b <- as.data.frame(qda_df)

dt_df_b <- TestErrorFunc(dt_reg, features, train_block, test_block, acc)
dt_df_b <- as.data.frame(dt_df)


## Create confusion matrices for two top performing models 
#install.packages("ConfusionTableR")
library(ConfusionTableR)

# Plot CM for decision tree horizontal slice (FIGURE 5)

confusionMatrix(as.factor(dt_df$comparison.predicted), as.factor(dt_df$comparison.true), mode="everything")

dt_cm_hs <- ConfusionTableR::binary_visualiseR(train_labels = as.factor(dt_df$comparison.predicted),
                                   truth_labels= as.factor(dt_df$comparison.true),
                                   class_label1 = "Not Cloud", 
                                   class_label2 = "Cloud",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = "DT Confusion Matrix (HS)", 
                                   text_col= "black")

# Plot CM for LDA horizontal slice

confusionMatrix(as.factor(lda_df$comparison.predicted), as.factor(lda_df$comparison.true), mode="everything")

lda_cm_hs <- ConfusionTableR::binary_visualiseR(train_labels = as.factor(lda_df$comparison.predicted),
                                              truth_labels= as.factor(lda_df$comparison.true),
                                              class_label1 = "Not Cloud", 
                                              class_label2 = "Cloud",
                                              quadrant_col1 = "#28ACB4", 
                                              quadrant_col2 = "#4397D2", 
                                              custom_title = "LDA Confusion Matrix (HS)", 
                                              text_col= "black")


# Visualize Patterns of Performance (FIGURE 6)

explab_2$exp_label[explab_2$exp_label == -1] <- 0 #Prep image 2

#Block
set.seed(12345)
block.index.image2 <- block_split(explab_2, 6)[[1]]

image2.block.test <- explab_2[block.index.image2,]
image2.block.train <- explab_2[-block.index.image2,]

image2.block.results <- TestErrorFunc(dt_reg, features, image2.block.train, image2.block.test, acc)$comparison

image2.block.test[image2.block.results$false.pos == 1,"exp_label"] <- 2 
image2.block.test[image2.block.results$false.neg == 1,"exp_label"] <- 3
image2.block.test[image2.block.results$true.pos == 1,"exp_label"] <- 4
image2.block.test[image2.block.results$true.neg == 1,"exp_label"] <- 4
image2.block.plot <- bind_rows(image2.block.train, image2.block.test)

results.viz.block <- ggplot(image2.block.plot) +
  geom_point(aes(x, y, col=factor(exp_label))) +
  scale_color_manual(labels=c("No Cloud", "Cloud", "False Positive", "False Negative", "Correct"), values=c("light blue", "dark blue", "yellow", "red", "green"), "Expert and Predicted Label") +
  theme(
    panel.background = element_rect(fill=NA),
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(size=12, hjust=0.5),
    legend.position = "right"
  )+
  labs(title = "Block", color = "Label")

results.viz.block

# Horizontal
set.seed(12345)
hor.index.image2 <- hor_slice(explab_2, 6)[[2]]

image2.hor.test <- explab_2[hor.index.image2,]
image2.hor.train <- explab_2[-hor.index.image2,]

image2.hor.results <- TestErrorFunc(dt_reg, features, image2.hor.train, image2.hor.test, acc)$comparison

image2.hor.test[image2.hor.results$false.pos == 1,"exp_label"] <- 2 
image2.hor.test[image2.hor.results$false.neg == 1,"exp_label"] <- 3
image2.hor.test[image2.hor.results$true.pos == 1,"exp_label"] <- 4
image2.hor.test[image2.hor.results$true.neg == 1,"exp_label"] <- 4
image2.hor.plot <- bind_rows(image2.hor.train, image2.hor.test)

results.viz.hor <- ggplot(image2.hor.plot) +
  geom_point(aes(x, y, col=factor(exp_label))) +
  scale_color_manual(labels=c("No Cloud", "Cloud", "False Positive", "False Negative", "Correct"), values=c("light blue", "dark blue", "yellow", "red", "green"), "Expert and Predicted Label") +
  theme(
    panel.background = element_rect(fill=NA),
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(size=12, hjust=0.5),
    legend.position = "none"
  )+
  labs(title = "Horizontal", color = "Label")


grid.arrange(results.viz.hor, results.viz.block, nrow = 1)
