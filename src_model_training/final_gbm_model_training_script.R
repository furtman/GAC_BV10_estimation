library(gbm)
 library(readxl)
library(tidyverse)
getwd()

#load the rda file

final_set<-read_excel('data/Breakthrough Database.xlsx', sheet='Data')

# create grid search
hyper_grid <- expand.grid(
  learning_rate = c(0.05,0.06,0.07,0.08,0.09, 0.1,0.3),
  RMSE = NA,
  trees = NA,
  time = NA
)
# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit gbm
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = y~C0 + A  +V +pH+mp_ratio+charge+ BET + pzc  +CD+PD +EBCT +UV254+L+S+B+E+DOC,
      data = final_set,
      distribution = "gaussian",
      n.trees = 5000, 
      shrinkage = hyper_grid$learning_rate[i], 
      interaction.depth = 3, 
      n.minobsinnode = 10,
      cv.folds = 10,
      train.fraction=1,
      bag.fraction=0.5
    )
  })
  
  # add SSE, trees, and training time to results
  hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
  hyper_grid$trees[i] <- which.min(m$cv.error)
  hyper_grid$Time[i]  <- train_time[["elapsed"]]
  
}

# results
arrange(hyper_grid, RMSE)->finalshrinkage
finalshrinkage
write.csv(finalshrinkage,"outcome/final_shrinkage.csv")
# search grid now that we have best learning rate, look for best interaction depth and num of leaves at terminal node
hyper_grid <- expand.grid(
  n.trees = 3000,
  shrinkage = 0.1,
  interaction.depth = c(3,4,5,6),
  n.minobsinnode = c(3,4,5,6,7,8,9,10 ),
  bag.fraction = c( 0.5,0.6,0.7,0.8)
)
# create model fit function
model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode, bag.fraction) {
  set.seed(123)
  m <- gbm(
    formula = y~C0 + A  +V +pH+mp_ratio+charge+ BET + pzc  +CD+PD +EBCT +UV254+L+S+B+E+DOC,
    data = final_set,
    distribution = "gaussian",
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction=bag.fraction,
    cv.folds = 10
  )
  # compute RMSE
  sqrt(min(m$cv.error))
}

# perform search grid with functional programming
hyper_grid$rmse <- purrr::pmap_dbl(
  hyper_grid,
  ~ model_fit(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4,
    bag.fraction=..5
  )
)

# results
arrange(hyper_grid, rmse)->hyper_grid_final
head(hyper_grid_final)
write.csv(hyper_grid_final,"outcome/final_gbm_hyperparam.csv")
#find best tree number

set.seed(123)
final_gbm<-gbm(y~C0 + A  +V +pH+mp_ratio+charge+ BET + pzc  +CD+PD +EBCT +UV254+L+S+B+E+DOC ,data=final_set, 
          distribution="gaussian",n.trees=3000, interaction.depth=3, shrinkage=0.1, n.minobsinnode =10,bag.fraction=0.8,  
          cv.folds = 10)
final_gbm
# # find index for number trees with minimum CV error
 which.min(final_gbm$cv.error)
 gbm.perf(final_gbm, method = "cv")
save(final_gbm,file="outcome/final_gbm.rda") 
