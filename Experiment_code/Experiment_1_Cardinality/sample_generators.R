library(MASS)
library(tidyverse)
library(truncnorm)
library(TruncatedNormal)
size = c(2,4,8,16,32,64,128)
# sd = c(1,1.5,2,2.5,3)
# cardinality condition sd = 1.5,2.5
# precision condition size = 3, 9
# sd_cardinality = c(1.5,2.5)
# size_precision = c(3, 9)

# no need
normalize <- function(array){
  sd_arr = sd(array)
  return((array)/sd_arr)
}

# cardinality condition
# size = 3, sd = 1.5
# mvrnorm(n, mean, cov, tol=1e-6, empirical = TRUE)

# sample from standard normal
# truncation boundary 1.5
#group_1
boundary = sqrt(0.2)*2
sample_list = list()
sample_list_1 = list()
N = 400
for(i in size){
  for(j in 1:N){
    x_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,1]
    y_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,2]
    
    while(max(abs(x_coord))> boundary){
      x_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,1]
    }
    while(max(abs(y_coord))> boundary){
      y_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,2]
    }
    sample_list = c(sample_list,list(data.frame(x = x_coord,y = y_coord)))
  }
  sample_list_1[[(which(size==i))]] = sample_list
  sample_list = list()
}

# sample_list_1[[6]][[4]] %>% ggplot(aes(x=x,y=y)) + geom_point()+coord_cartesian(xlim = c(-3,3),ylim = c(-3,3))

# n=64
# var = 0.2
# a = data.frame(x = mvrnorm(n, c(0,0), diag(2)*var, tol=1e-6, empirical = TRUE)[,1],
#                y = mvrnorm(n, c(0,0), diag(2)*var, tol=1e-6, empirical = TRUE)[,2],
#                group = 1)
# 
# b = data.frame(x = mvrnorm(n, c(0,0), diag(2)*var, tol=1e-6, empirical = TRUE)[,1],
#                y = mvrnorm(n, c(0,0), diag(2)*var, tol=1e-6, empirical = TRUE)[,2],
#                group=2)
# dat = rbind(a,b)
# dat %>% ggplot(aes(x=x,y=y,color=as.factor(group))) + geom_point()+coord_cartesian(xlim = c(-3,3),ylim = c(-3,3))
# 
# sd(a$x)
# sd(a$y)
# sd(b$x)
# a$x = a$x*2*0.6
# a$y = a$y*2*0.6
# b$x = b$x*2*0.6
# b$y = b$y*2*0.6
# dat = rbind(a,b)
# dat %>% ggplot(aes(x=x,y=y,color=as.factor(group))) + geom_point()+coord_cartesian(xlim = c(-3,3),ylim = c(-3,3))

# #group_2
boundary = sqrt(0.2)*2
sample_list = list()
sample_list_2 = list()
for(i in size){
  for(j in 1:N){
    x_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,1]
    y_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,2]
    
    while(max(abs(x_coord))> boundary){
      x_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,1]
    }
    while(max(abs(y_coord))> boundary){
      y_coord = mvrnorm(i, c(0,0), diag(2)*0.2, tol=1e-6, empirical = TRUE)[,2]
    }
    sample_list = c(sample_list,list(data.frame(x = x_coord,y = y_coord)))
  }
  sample_list_2[[(which(size==i))]] = sample_list
  sample_list = list()
}

all_sample = tibble(group_sample_1 = sample_list_1,
                    group_sample_2 = sample_list_2)

library(dplyr)
library(jsonlite)

all_sample = toJSON(all_sample)

setwd("/Users/young/Desktop/UCSD/Research/IEEE_VIS_2023/Experiment_code/Experiment_1/dist")
write_json(all_sample,"all_sample.js")

# #Precision condition
# #group_1
# sample_list_3 = list()
# # for(i in sd){
# for(j in size_precision){
#     sample_list_3 = c(sample_list_3,list(replicate(n=10,list(mvrnorm(j, c(0,0), diag(2), tol=1e-6, empirical = TRUE)))))
# }
# 
# #group_2
# sample_list_4 = list()
# for(i in sd){
#   for(j in size_precision){
#     sample_list_4 = c(sample_list_4,list(replicate(n=5,list(mvrnorm(j, c(0,0), diag(2)*i, tol=1e-6, empirical = TRUE)))))
#   }
# }
# 
# all_sample_precision = tibble(group_1 = sample_list_3, group_2 = sample_list_4)
# 
# all_sample_precision = toJSON(all_sample_precision)
# 
# setwd("/Users/young/Desktop/UCSD/Research/Mean_perception_position/Ensembles_mean_perception/dist")
# write_json(all_sample_precision,"all_sample_precision.json")










