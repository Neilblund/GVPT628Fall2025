library(tidyverse)
gtd<-read_csv('https://raw.githubusercontent.com/Neilblund/APAN/refs/heads/main/gtd_2020.csv')
codebook<-read_csv("https://raw.githubusercontent.com/Neilblund/APAN/refs/heads/main/included_gtd.csv")
gtd$attack_dummy<-factor(gtd$any_attacks)


# select features and drop missing to ensure consistent results
# (in many applied settings we might actually want to try to infer missing data
# instead - possibly by using values from prior years)

cols<-c("attack_dummy", "v2x_libdem", "wdi_resource_rents", "wdi_total_pop")
gtd<-gtd|>
  select(all_of(cols))|>
  drop_na()
set.seed(100)


gtd_testing<-gtd|>
  slice_sample(prop = .2)
gtd_training<-anti_join(gtd, gtd_testing)




model1<-glm(attack_dummy ~  v2x_libdem + wdi_resource_rents,data=gtd_training, family='binomial')
model2<-glm(attack_dummy ~  v2x_libdem + wdi_resource_rents  + log(wdi_total_pop),data=gtd_training, family='binomial')


getcmatStats<-function(ct){
  
  tp <- ct["yes","yes"]
  fp<-ct["yes","no"]
  fn <- ct['no','yes']
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1<-(2*tp) / ((2*tp) + fp + fn)
  accuracy <- sum(diag(ct)) /sum(ct)
  result<-list('accuracy' = accuracy,
               'precision' = precision,
               'recall' = recall,
               'f' = f1,
               'confusion' = ct
               
  )
  return(result)
}


getStats<-function(model, test_x, test_y){
  predictions<-factor(predict(model, type='response', newdata = test_x)>=.5,
                      levels=c(FALSE, TRUE),
                      labels=c('no',"yes")
  )
  ct<-table('predicted' = predictions, 
            'outcome'  = test_y)
  
  result<-getcmatStats(ct)
  

  return(result)
  
}


getStats(model1, test_x = gtd_testing|>select(-attack_dummy), test_y = gtd_testing$attack_dummy)

getStats(model2, test_x = gtd_testing|>select(-attack_dummy), test_y = gtd_testing$attack_dummy)

# LOOCV ----

cols<-c("attack_dummy", "v2x_libdem", "wdi_resource_rents", "wdi_total_pop")
gtd<-gtd|>
  select(all_of(cols))|>
  drop_na()

preds<-tibble(
  .rows = nrow(gtd),
  'model1_pred'=NA, 'model2_pred'=NA, 'truth'=NA)

for(i in 1:nrow(gtd)){
  gtd_training<-gtd[-i, ]
  gtd_testing = gtd[i, ]
  model1<-glm(attack_dummy ~  v2x_libdem + wdi_resource_rents,data=gtd_training, family='binomial')
  model2<-glm(attack_dummy ~  v2x_libdem + wdi_resource_rents  + log(wdi_total_pop),data=gtd_training, family='binomial')
  
  row<-data.frame(
             'model1_pred' = factor(predict(model1, newdata = gtd_testing, type='response') >=.5,
                                    levels=c(FALSE, TRUE),
                                    labels=c("no", "yes")),
             'model2_pred' = factor(predict(model2, newdata=gtd_testing, type='response')>=.5, 
                                    levels=c(FALSE, TRUE),
                                    labels=c("no", "yes")),
             'truth' = gtd_testing$attack_dummy)
  
  preds[i, ] <- row
  
}


getcmatStats(table(preds$model2_pred, preds$truth))




# Using caret ----
library(caret)

results <- confusionMatrix(ct, positive = "yes")
results          # view multiple stats
results$byClass  # get class-specific stats, including F stat



# LOOCV in caret

train_control <- trainControl(method = "LOOCV")

# Using LOOCV: 
model <- train(attack_dummy ~  v2x_libdem + wdi_resource_rents  + log(wdi_total_pop),
               data = gtd, method = "glm",
               family='binomial',
               trControl = train_control)

model