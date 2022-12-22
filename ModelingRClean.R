# 2nd Version of ModelingR with Relevant Models

# Libraries, Working Directory
library(fastDummies) # Dummy Encoding
library(MASS) # Discriminant Analysis
library(rattle) # Confusion Matrix
library(class) # KNN
library(e1071) # KNN CV, SVM
library(glmnet) # Ridge, Lasso
library(rpart) # Tree Plotting
library(tree) # Classification Tree Modeling
library(randomForest) # Random Forest
library(pls) # PCR

# Import Data
df <- read.csv('2000-2017CombineClean.csv')[,-1]
colnames(df)
head(df)
nrow(df)

# Creating New Vectors for Drafted/Undrafted
df$IsDrafted <- ifelse(df$Round == 8, 0, 1)
table(df$IsDrafted) / length(df$IsDrafted)

# Setting Position as factor
df$Pos <- as.factor(df$Pos)

# Setting up a new df with Position as Indicators
  # WR Column will be omitted in model creation
df <- dummy_cols(df, select_columns = 'Pos')[,-1]

# Setting seed
set.seed(16)

# Train-Test Split
sample <- sample(c(TRUE, FALSE), nrow(df),
                 replace=TRUE, prob=c(0.8,0.2))

Drafted_train <- df[sample, c(1:3, 12:18)]
Drafted_test <- df[!sample, c(1:3, 12:18)]

Drafted_Pos_train <- df[sample, c(2, 3, 12:18, 19:36)]
Drafted_Pos_test <- df[!sample, c(2, 3, 12:18, 19:36)]

# Standardized Versions of Data
standard_Drafted_train <- scale(cbind.data.frame(Drafted_train[,2:9],
                                                 Pos = as.numeric(Drafted_train[,1])))
standard_values <- attributes(standard_Drafted_train)
standard_Drafted_train <- cbind.data.frame(standard_Drafted_train,
                                IsDrafted = Drafted_train[,10])

standard_Drafted_test <- scale(cbind.data.frame(Drafted_test[,2:9],
                                                Pos = as.numeric(Drafted_test[,1])),
                               scale = standard_values$`scaled:scale`,
                               center = standard_values$`scaled:center`)
standard_Drafted_test <- cbind.data.frame(standard_Drafted_test,
                               IsDrafted = Drafted_test[,10])
# Matrix versions for Ridge, Lasso
sDtMat <- data.matrix(standard_Drafted_train) 
sDtestMat <- data.matrix(standard_Drafted_test)

# Matrix to track error across all tests
Error_Matrix <- matrix(NA, nrow = 1, ncol = 14)
colnames(Error_Matrix) <- c('LDA', 'LDA_Pos',
                            'QDA', 'QDA_Pos',
                            'KNN', 'KNN_Pos',
                            'Polynomial SVM', 'Radial SVM',
                            'Ridge Regression', 'Lasso Regression',
                            'Regression Tree', 'Random Forest',
                            'PCR', 'Log Regression')

                      ######## Log Regression ########
# Logit - Position
fit_Pos_logit <- glm(IsDrafted ~., data = Drafted_train, family = 'binomial')
pred_Pos_logit <- 1*(predict.glm(fit_Pos_logit, newdata = Drafted_test[,-10],
                          type = 'response') > 0.5)
(error_Pos_logit <- 1 - mean(1*(pred_Pos_logit == Drafted_test[,10])))
## Train Error
pred_Pos_logit_train <- 1*(predict.glm(fit_Pos_logit, newdata = Drafted_train[,-10],
                                 type = 'response') > 0.5)
(error_Pos_logit_train <- 1 - mean(1*(pred_Pos_logit_train == Drafted_train[,10])))

# Logit - No Position
fit_logit <- glm(IsDrafted ~., data = Drafted_train[,-1], family = 'binomial')
pred_logit <- 1*(predict.glm(fit_logit, newdata = Drafted_test[,2:9],
                             type = 'response') > 0.5)
(error_logit <- 1 - mean(1*(pred_logit == Drafted_test[,10])))
errorMatrix(Drafted_test[,10], pred_logit, percentage = F,
            count = T)
## Train Error
pred_logit_train <- 1*(predict.glm(fit_logit, newdata = Drafted_train[,2:9],
                             type = 'response') > 0.5)
(error_logit_train <- 1 - mean(1*(pred_logit_train == Drafted_train[,10])))


                        ######## LDA ########
# LDA - No Position
fit_lda <- lda(x = Drafted_train[,2:9],
               grouping = Drafted_train[,10])
pred_lda <- predict(fit_lda, Drafted_test[,2:9])$class
(error_lda <- 1 - mean(1*(pred_lda == Drafted_test[,10])))
Error_Matrix[,1] <- error_lda
errorMatrix(Drafted_test[,10], pred_lda, percentage = FALSE, count = TRUE)
# Training Error
pred_lda_train <- predict(fit_lda, Drafted_train[,2:9])$class
(error_lda_train <- 1 - mean(1*(pred_lda_train == Drafted_train[,10])))

# LDA - Position Indicator
fit_lda_Pos <- lda(x = Drafted_Pos_train[,c(-9,-27)],
                   grouping = Drafted_Pos_train[,9])
pred_lda_Pos <- predict(fit_lda_Pos,
                        Drafted_Pos_test[,c(-9,-27)])$class
(error_lda_Pos <- 1 - mean(1*(pred_lda_Pos == Drafted_Pos_test[,9])))
Error_Matrix[,2] <- error_lda_Pos
errorMatrix(Drafted_Pos_test[,9], pred_lda_Pos, percentage = F, count = T)
# Training Error
pred_lda_Pos_train <- predict(fit_lda_Pos, Drafted_Pos_train[,c(-9,-27)])$class
(error_lda_Pos_train <- 1 - mean(1*(pred_lda_Pos_train == Drafted_Pos_train[,9])))



                      ######## QDA ########
# QDA - No Position
fit_qda <- qda(x = Drafted_train[,2:9],
                  grouping = Drafted_train[,10])
pred_qda <- predict(fit_qda, Drafted_test[,2:9])$class
(error_qda <- 1 - mean(1*(pred_qda == Drafted_test[,10])))
Error_Matrix[,3] <- error_qda
errorMatrix(Drafted_test[,10], pred_qda, percentage = F, count = T)
# Training Error
pred_qda_train <- predict(fit_qda, Drafted_train[,2:9])$class
(error_qda_train <- 1 - mean(1*(pred_qda_train == Drafted_train[,10])))

#QDA - Position Indicator
fit_qda_Pos <- qda(x = Drafted_Pos_train[,c(-9,-27)],
                   grouping = Drafted_Pos_train[,9])
pred_qda_Pos <- predict(fit_qda_Pos,
                        Drafted_Pos_test[,c(-9,-27)])$class
(error_qda_Pos <- 1 - mean(1*(pred_qda_Pos == Drafted_Pos_test[,9])))
Error_Matrix[,4] <- error_qda_Pos
errorMatrix(Drafted_Pos_test[,9], pred_qda_Pos, percentage = F, count = T)
# Training Error
pred_qda_Pos_train <- predict(fit_qda_Pos, Drafted_Pos_train[,c(-9,-27)])$class
(error_qda_Pos_train <- 1 - mean(1*(pred_qda_Pos_train == Drafted_Pos_train[,9])))



                      ######## KNN ########
KNN <- function(x_train_set, y_train_set, x_test_set, y_test_set) {
  error_mat = matrix(NA, nrow = 50, ncol = 1)
  for (k in 1:100) {
    fit_knn <- knn(train = x_train_set, test = x_test_set,
                   cl = y_train_set, k)
    #print(table(fit_knn))
    knn_error = 1 - mean(1*(fit_knn == y_test_set))
    error_mat[k] = knn_error
  }
  return(error_mat)
}

# KNN - No Position
KNN_Fit <- KNN(Drafted_train[,2:9],
               Drafted_train[,10], 
               Drafted_test[,2:9],
               Drafted_test[,10])
KNN_Fit == min(KNN_Fit) # k = 39
fit_knn <- knn(train = Drafted_train[,2:9],
               test = Drafted_test[,2:9],
               cl = Drafted_train[,10], 39)
(knn_error = 1 - mean(1*(fit_knn == Drafted_test[,10])))
Error_Matrix[,5] <- knn_error
errorMatrix(Drafted_test[,10],fit_knn, percentage = F, count = T)
## Training Error
fit_knn_train <- knn(train = Drafted_train[,2:9],
               test = Drafted_train[,2:9],
               cl = Drafted_train[,10], 39)
(knn_error_train = 1 - mean(1*(fit_knn_train == Drafted_train[,10])))

# KNN - Position Indicator
KNN_Fit_Pos <- KNN(Drafted_Pos_train[,c(-9,-27)],
               Drafted_Pos_train[,9], 
               Drafted_Pos_test[,c(-9,-27)],
               Drafted_Pos_test[,9])
KNN_Fit_Pos == min(KNN_Fit_Pos) # k = 38
fit_knn_Pos <- knn(train = Drafted_Pos_train[,c(-9,-27)],
               test = Drafted_Pos_test[,c(-9,-27)],
               cl = Drafted_Pos_train[,9], 38)
(knn_error_Pos = 1 - mean(1*(fit_knn_Pos == Drafted_Pos_test[,9])))
Error_Matrix[,6] <- knn_error_Pos
errorMatrix(Drafted_test[,10],fit_knn, percentage = F, count = T)
## Training Error
fit_knn_Pos_train <- knn(train = Drafted_Pos_train[,c(-9,-27)],
                   test = Drafted_Pos_train[,c(-9,-27)],
                   cl = Drafted_Pos_train[,9], 38)
(knn_error_Pos_train = 1 - mean(1*(fit_knn_Pos_train == Drafted_Pos_train[,9])))



                      ######## Polynomial SVM ########
#Polynomial SVM - Positions as Factor, Drafted vs Undrafted
tune_svm_Poly <- tune(svm, IsDrafted ~., data = standard_Drafted_train,
                 kernel = 'polynomial',
                 ranges=list(cost=c(0.01, 1, 10),
                             gamma=c(0.001, 0.01, 0.1, 1)))
tune_svm_Poly$performances
final_svm_Poly <- tune_svm_Poly$best.model
pred_svm_Poly <- predict(final_svm_Poly, standard_Drafted_test[,-10])
(error_svm_Poly <- mean(1*(pred_svm_Poly == standard_Drafted_test[,10])))
Error_Matrix[,7] <- error_svm_Poly
errormatrix(standard_Drafted_test[,10],pred_svm_Poly)


                      ######## Radial SVM ########
#Radial SVM - Positions as Factor, Drafted vs Undrafted
tune_svm_Rad <- tune(svm, IsDrafted ~., data = standard_Drafted_train,
                     kernel = 'radial',
                     ranges=list(cost=c(seq(0.01, 10, 0.5)),
                                 gamma=c(0.001, 0.01, 0.1, 1)))
tune_svm_Rad$performances
final_svm_Rad <- svm(IsDrafted ~., data = standard_Drafted_train,
                     kernel = 'radial',
                     gamma = 0.1, cost = 1)#tune_svm_Rad$best.model
pred_svm_Rad <- 1*(predict(final_svm_Rad,
                           standard_Drafted_test[,-10]) > 0.5)
(error_svm_Rad <- 1 - mean(1*(pred_svm_Rad == Drafted_test[,10])))
Error_Matrix[,8] <- error_svm_Rad
errorMatrix(standard_Drafted_test[,10],pred_svm_Rad,
            percentage = F, count = T)



                      ######## Ridge Regression ########
# Ridge Regression - Positions, Drafted vs Undrafted Graphs of coefficients
cv_Ridge <- cv.glmnet(x = sDtMat[,-10], y = sDtMat[,10],
                      alpha = 0)
cv_Ridge$lambda.min
fit_ridge <- glmnet(x = sDtMat[,-10], y = sDtMat[,10], 
                    alpha = 0, lambda = cv_Ridge$lambda.min) # 0.00604512
pred_Ridge <- 1*((predict.glmnet(fit_ridge, 
                                 newx = sDtestMat[,-10],
                                 type = 'response') > 0.5))
(error_Ridge <- 1 - mean(pred_Ridge == sDtestMat[,10]))
Error_Matrix[,9] <- error_Ridge
errorMatrix(Drafted_test[,10], pred_Ridge, percentage = F, count = T)
plot(cv_Ridge)
# Training Error
train_pred_Ridge <- 1*((predict.glmnet(fit_ridge, newx = sDtMat[,-10],
                                       type = 'response') > 0.5))
(train_error_Ridge <- 1 - mean(train_pred_Ridge == sDtMat[,10]))
# Coefficients
fit_ridge$beta


                      ######## Lasso Regression ########
# Lasso Regression - Positions, Drafted vs Undrafted
cv_Lasso <- cv.glmnet(x = sDtMat[,-10], y = sDtMat[,10],
                      alpha = 1)
cv_Lasso$lambda.min
fit_Lasso <- glmnet(x = sDtMat[,-10], y = sDtMat[,10], 
                    alpha = 1, lambda = cv_Lasso$lambda.min) # 0.0002
pred_Lasso <- 1*((predict.glmnet(fit_Lasso, newx = sDtestMat[,-10],
                                 type = 'response') > 0.5))
(error_Lasso <- 1 - mean(pred_Lasso == sDtestMat[,10]))
Error_Matrix[,10] <- error_Lasso
errorMatrix(Drafted_test[,10], pred_Lasso, percentage = F, count = T)
# Training Error
train_pred_Lasso <- 1*((predict.glmnet(fit_Lasso, newx = sDtMat[,-10],
                                 type = 'response') > 0.5))
(train_error_Lasso <- 1 - mean(train_pred_Lasso == sDtMat[,10]))
# Coefficients
fit_Lasso$beta



                      ######## Classification Tree ########
# Tree - Position Drafted vs Undrafted
  #using rpart for graphical purposes
fit_tree <- rpart(IsDrafted ~., data = Drafted_train)
fancyRpartPlot(fit_tree)
## Fitting initial tree
fit_tree <- tree(IsDrafted ~., data = Drafted_train)
## CV to find tree depth
cv.fit_tree = cv.tree(fit_tree)
plot(cv.fit_tree$size, cv.fit_tree$dev, type='b')
## Pruning tree to have the size chosen by CV (Original Tree is best)
prune.fit_tree = prune.tree(fit_tree, best=5)
pred_tree <- (1*(predict(fit_tree, newdata = Drafted_test[,-10]) > 0.5))
(error_tree <- 1 - mean(1*(pred_tree == Drafted_test[,10])))
Error_Matrix[,11] <- error_tree
errorMatrix(Drafted_test[,10], pred_tree, percentage = F, count = T)
# Training Error Rate
train_pred_tree <- (1*(predict(fit_tree, newdata = Drafted_train[,-10]) > 0.5))
(train_error_tree <- 1 - mean(1*(train_pred_tree == Drafted_train[,10])))



                      ######## Random Forest ########
# Random Forest - Position, Drafted vs Undrafted
# Setting IsDrafted to a factor for this method to work
Drafted_train_alt <- cbind.data.frame(Drafted_train[,-10],
                                      IsDrafted = as.factor(Drafted_train$IsDrafted))
# Cross validation on M
cv_RF <- tuneRF(x = Drafted_train_alt[,-10],
                y = Drafted_train_alt[,10],
                mtryStart = 4, ntreeTry = 1000)
# Re-run with new M
fit_cv_RF <- randomForest(IsDrafted ~., data = Drafted_train_alt,
                          mtry = 2, ntree = 2000)
predict_cv_RF <- predict(fit_cv_RF, newdata = Drafted_test[,-10])
(error_cv_RF <- 1 - mean(1*(predict_cv_RF == Drafted_test[,10])))
varImpPlot(fit_cv_RF, main = 'Variable Importance of Random Forest')
errorMatrix(Drafted_test[,10], predict_cv_RF, percentage = F, count = T)



                ######## PCA with Binary Regression ########
# PCA on Training Data
pca <- princomp(Drafted_Pos_train[,-9], cor = T)
plot(pca)
pca1 <- pca$scores[,1]
pca2 <- pca$scores[,2]

# PCA on Testing Data using values of Training PCA
pca_test <- predict(pca, Drafted_Pos_test[,-9])

# Fitting Logit Model
fit_pcr <- glm(Drafted_Pos_train[,9] ~ pca1 + pca2,
               family = binomial)
# Predicting Test Data
pred_pcr <- 1*(predict.glm(fit_pcr,
                           data.frame(pca_test[,1],pca_test[,2])) > 0.5)
error_pcr <- mean(1*(pred_pcr == Drafted_Pos_test[,9]))

head(pred_pcr)
summary(fit_pcr)
pca$center


pca_test <- predict(pca, Drafted_Pos_test[,-9])
pred_pcr <- predict.glm(fit_pcr, data.frame(pca_test[,1]))
dim(pca_test)


# Graph of Model Errors
final_errors <- matrix(NA, ncol = 3, nrow = 12)
colnames(final_errors) <- c('Train Error',
                            'Test Error', 'Baseline')
rownames(final_errors) <- c('Logit', 'Logit(!Pos)',
                            'LDA', 'LDA(!Pos)',
                            'QDA', 'QDA(!Pos)',
                            'KNN', 'KNN(!Pos)',
                            'Ridge', 'Lasso',
                            'Tree', 'Forest')
final_errors[,1] <- c(0.3118, 0.3250,
                      0.3144, 0.3237,
                      0.3384, 0.3220,
                      0.3263, 0.3243,
                      0.3214, 0.3212,
                      0.3262, 0.3199)
final_errors[,2] <- c(0.3091, 0.3023,
                      0.3100, 0.3049,
                      0.3433, 0.3091,
                      0.3296, 0.3364,
                      0.3091, 0.3091,
                      0.3228, 0.3134)
final_errors[,3] <- 0.359
final_errors
plot(final_errors[,1], type = 'b',
     ylim = c(0.3, 0.4),
     main = 'Classification Error of Each Model',
     xlab = 'Model',
     ylab = 'Classification Error',
     col = 'blue', xaxt = 'n')
axis(1, at = 1:12,
     labels = rownames(final_errors))
lines(final_errors[,2], col = 'red', type = 'b')
text(x = 1:12 + 0.1,
     y = final_errors[,2] - 0.003,
     labels = final_errors[,2], col = 'red')
lines(final_errors[,3], type = 'b')
legend(x = 10, y = 0.4,
       legend = c('Baseline (0.359)',
                  'Training Error',
                  'Testing Error'),
       col = c('Black', 'Blue', 'Red'),
       lty = 1:2, cex = 0.6)
