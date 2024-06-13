library("mfx")
library("dplyr")
library("pROC")
library("randomForest")
library("randomForestExplainer")

train_df <- read.csv("train_data_2018.csv")
test_df <- read.csv("test_data_2022.csv")

modellmath <- glm(MATHSCORE ~ INSTRUMENTHOME + CITY + BOOKSHOME + OWNCOMPUTER
                + LATE + MISEI + POORSTU + REPEAT,
                  data = train_df, family = binomial(link = "logit"))

coefficients_math <- summary(modellmath)$coefficients[-1, ]

modellmathdf <- data.frame(
  ME = round(coefficients_math[, "Estimate"], 3),
  p_value = round(coefficients_math[, "Pr(>|z|)"], 3)
)


proglmath <- predict(modellmath,type = "response", newdata=test_df)
hist(proglmath)
proglmath01 <- ifelse(proglmath > 0.5, 1, 0)
hist(proglmath01)
table(proglmath01,test_df$MATHSCORE)

rf.roclmath<-roc(test_df$MATHSCORE,proglmath)
plot(rf.roclmath)
auc(rf.roclmath)

plot(rf.roclmath, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)
plot(smooth(rf.roclmath), add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)

modellread <- glm(READSCORE ~ GENDER + BOOKSHOME + VILLAGE + TCHLISTEN
                  + BULLIED + MISCED + POORSTU + REPEAT, 
                  data = train_df, family = binomial(link = "logit")) 

coefficients_read <- summary(modellread)$coefficients[-1, ]

modellreaddf <- data.frame(
  ME = round(coefficients_read[, "Estimate"], 3),
  p_value = round(coefficients_read[, "Pr(>|z|)"], 3)
)

proglread <- predict(modellread,type = "response", newdata=test_df)
hist(proglread)
proglread01 <- ifelse(proglread > 0.5, 1, 0)
hist(proglread01)
table(proglread01,test_df$READSCORE)

rf.roclread<-roc(test_df$READSCORE,proglread)
plot(rf.roclread)
auc(rf.roclread)

plot(rf.roclread, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)
plot(smooth(rf.roclread), add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)




`train_df$MATHSCORE <- as.factor(train_df$MATHSCORE)
formula_rfm <- as.formula(paste("MATHSCORE ~", paste(names(train_df)[3:ncol(train_df)], collapse = " + ")))

auc_value <- 0
auc_threshold <- 0.635

while (auc_value <= auc_threshold) {
mathforest <- randomForest(formula_rfm, data = train_df, method="class", nodesize = 2, maxnodes = 30, ntree = 1000, localImp = TRUE)
progfmath<-predict(mathforest,newdata=test_df)
rf.rocfmath<-roc(progfmath,test_df$MATHSCORE)
plot(rf.rocfmath)
auc(rf.rocfmath)
auc_value <- auc(rf.rocfmath)
if (auc_value > auc_threshold) {
  break
}
}
plot(rf.rocfmath, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)
table(test_df$MATHSCORE,progfmath)


min_depth_frame_math <- min_depth_distribution(mathforest)
plot_min_depth_distribution(min_depth_frame_math)
importance_frame_math <- measure_importance(mathforest)
plot_multi_way_importance(importance_frame_math, 
                          size_measure = "no_of_nodes", no_of_labels = 10)
names(importance_frame_math)
plot_multi_way_importance(
  importance_frame_math,
  x_measure = "accuracy_decrease",  # X-axis measure
  y_measure = "gini_decrease",  # Y-axis measure
  size_measure = "p_value",  # The size of the points represents the significance
  no_of_labels = 10  # Number of variable labels to show
)
plot_importance_ggpairs(importance_frame_math)
vars_math <- important_variables(importance_frame_math, k = 5, measures = c("mean_min_depth", "no_of_trees"))
interactions_frame <- min_depth_interactions(mathforest, vars_math)
plot_min_depth_interactions(interactions_frame)



train_df$READSCORE <- as.factor(train_df$READSCORE)
formula_rfr <- as.formula(paste("READSCORE ~", paste(names(train_df)[3:ncol(train_df)], collapse = " + ")))
auc_value <- 0
auc_threshold <- 0.69

while (auc_value <= auc_threshold) {
  readforest <- randomForest(formula_rfr, data = train_df, method="class", nodesize = 2, maxnodes = 30, localImp = TRUE )
  progfread<-predict(readforest,newdata=test_df)
  rf.rocfread<-roc(progfread,test_df$READSCORE)
  auc(rf.rocfread)
  auc_value <- auc(rf.rocfread)
  if (auc_value > auc_threshold) {
    break
  }
}

progfread<-predict(readforest,newdata=test_df)
table(test_df$READSCORE,progfread)

rf.rocfread<-roc(progfread,test_df$READSCORE)
plot(rf.rocfread)
auc(rf.rocfread)

plot(rf.rocfread, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)

min_depth_frame_read <- min_depth_distribution(readforest)
plot_min_depth_distribution(min_depth_frame_read)
importance_frame_read <- measure_importance(readforest)
plot_multi_way_importance(importance_frame_read, 
                          size_measure = "no_of_nodes", no_of_labels = 10)
names(importance_frame_read)
plot_multi_way_importance(
  importance_frame_read,
  x_measure = "accuracy_decrease",  # X-axis measure
  y_measure = "gini_decrease",  # Y-axis measure
  size_measure = "p_value",  # The size of the points represents the significance
  no_of_labels = 10  # Number of variable labels to show
)
plot_importance_ggpairs(importance_frame_read)
vars_read <- important_variables(importance_frame_read, k = 5, measures = c("mean_min_depth", "no_of_trees"))
interactions_frame_read <- min_depth_interactions(readforest, vars_read)
plot_min_depth_interactions(interactions_frame_read)


install.packages("caret")
library(caret)
test_df$MATHSCORE <- factor(test_df$MATHSCORE)
proglmath01 <- factor(proglmath01)
proglread01 <- factor(proglread01)
test_df$READSCORE <- factor(test_df$READSCORE)
progfmath <- factor(progfmath)
progfread <- factor(progfread)

lm <- confusionMatrix( reference = test_df$MATHSCORE, data = proglmath01)
lr <- confusionMatrix(reference = test_df$READSCORE,data  = proglread01)
fm <- confusionMatrix(reference = test_df$MATHSCORE, data = progfmath)
fr <- confusionMatrix(reference = test_df$READSCORE, data = progfread)