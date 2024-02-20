library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(tidyverse)
library(tidymodels)
df<-read.csv("final_rawcounts_merged.csv")
set.seed(245)
data_rows <- floor(0.80 * nrow(df))
train_indices <- sample(c(1:nrow(df)), data_rows)
train_data <- df[train_indices,]
test_data <- df[-train_indices,]

training<- data.frame(train_data$avg_intervention_time,
                      train_data$cardiac_intervention_count,
                      train_data$DOA_all_count,
                      train_data$interventions_count,
                      train_data$aed_count)
test<- data.frame(test_data$avg_intervention_time,test_data$cardiac_intervention_count,test_data$DOA_all_count,test_data$interventions_count,test_data$aed_count)
trainingtarget<-train_data$DOA_cardiac_count
testtarget<-test_data$DOA_cardiac_count

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)
training<-as.data.frame(training)
names(training)<-c("avg_intervention_time",
                   "cardiac_intervention_count",
                   "DOA_all_count",
                   "interventions_count",
                   "aed_count")
test<-as.data.frame(test)
names(test)<-c("avg_intervention_time",
               "cardiac_intervention_count",
               "DOA_all_count",
               "interventions_count",
               "aed_count")
training<-as.matrix(training)
test<-as.matrix(test)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 50, activation = 'relu', input_shape = c(5)) %>%
  layer_dropout(rate=0.4)  %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate=0.2)  %>%
  layer_dense(units = 1)

model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = 'mae') 

mymodel <- model %>%          
  fit(training,trainingtarget,
      epochs = 1000,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2) 
plot(testtarget, pred) 
abline(a=0,b=1)

full_testing<- data.frame(df$avg_intervention_time,df$cardiac_intervention_count,df$DOA_all_count,df$interventions_count,df$aed_count)
full_testtarget<-df$DOA_cardiac_count

add_vector_2<-integer(nrow(df))
aed_tick=1
aed_number=sum(df$aed_count)

for (i in 0:aed_number){
  print(i)
  full_data_new<-df
  full_data_new<- data.frame(full_data_new$avg_intervention_time,
                             full_data_new$cardiac_intervention_count,
                             full_data_new$DOA_all_count,
                             full_data_new$interventions_count,
                             full_data_new$aed_count
                             #full_data_new$population
                             )
  names(full_data_new)<-c("avg_intervention_time",
                          "cardiac_intervention_count",
                          "DOA_all_count",
                          "interventions_count",
                          "aed_count"
                          #"population"
                          )
  full_data_new$aed_count<-full_data_new$aed_count+(add_vector_2)
  full_data_w1<- full_data_new
  full_data_w1$aed_count<-full_data_w1$aed_count+(aed_tick)
  m <- colMeans(full_data_new)
  s <- apply(full_data_new, 2, sd)
  full_data_new <- scale(full_data_new, center = m, scale = s)
  full_data_w1<-scale(full_data_w1,center=m,scale=s)
  #full_data_new<-as.matrix(full_data_new)
  #full_data_w1<-as.matrix(full_data_w1)
  pred_new <- model %>% predict(full_data_new)
  pred_w1<- model %>% predict(full_data_w1)
  pred_diff<-pred_w1-pred_new
  #pred_diff<-pred_diff/pred_new
  best<-which.max(-pred_diff)
  add_vector_2[best]<-add_vector_2[best]+1
}
new_aed_1<-data.frame(df$postal_code,add_vector_2)
names(new_aed_1)<-c("postal_code","add_vector_2")
new_aed_1$postal_code<-as.character(new_aed_1$postal_code)
library(ggplot2)
pred_test<-data.frame(testtarget,pred,pred-testtarget)
names(pred_test)<-c("testtarget","pred","residuals")
ggplot(pred_test,aes(testtarget,pred))+
  geom_point()+
  geom_abline(intercept=0,slope=1)
ggplot(pred_test,aes(x=as.numeric(row.names(pred_test)),y=residuals))+
  geom_point()+
  geom_abline(intercept=0,slope=0)
