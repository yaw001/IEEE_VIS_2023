library(rjson)
library(tidyverse)
library(lsr)
library(sp)
library(splancs)

setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab_data/Gaussian_Precision_2_data")
# setwd("/Users/young/Library/Mobile Documents/com~apple~CloudDocs/Desktop/UCSD/Research/Mean_position_perception_lab_data/Gaussian_Precision_2_data")
#data transformation
all.data_precision = list()
subject = 1
for(file.name in list.files(pattern = '*.json')) {
  json_file = fromJSON(file = file.name)
  json_file[['subject']] = subject
  all.data_precision[[subject]] = json_file
  subject = subject + 1
}

setwd("/Users/young/Desktop/UCSD/Research/IEEE_VIS_2023/Data&Analysis")
source("helper.R")

trial_response_precision = tibble()
num_subj = length(all.data_precision)
num_subj

for(i in 1:num_subj) {
  for(j in 1:506) {
    trial_response_precision = bind_rows(trial_response_precision,
                                    tibble(subject = all.data_precision[[i]]$subject,
                                           sid = all.data_precision[[i]]$client$sid,
                                           response_time = all.data_precision[[i]]$trials[[j]]$response_time,
                                           trial_number = all.data_precision[[i]]$trials[[j]]$trialNumber,
                                           mean_index = all.data_precision[[i]]$trials[[j]]$mean_index,
                                           group_1_size = all.data_precision[[i]]$trials[[j]]$group_1_size,
                                           group_2_size = all.data_precision[[i]]$trials[[j]]$group_2_size,
                                           group_1_sd = all.data_precision[[i]]$trials[[j]]$group_sd_1,
                                           group_2_sd = all.data_precision[[i]]$trials[[j]]$group_sd_2,
                                           inner_width = all.data_precision[[i]]$trials[[j]]$inner_width,
                                           inner_height = all.data_precision[[i]]$trials[[j]]$inner_height,
                                           width_height = min(inner_width,inner_height)*0.99,
                                           aspect_ratio = inner_width/inner_height,
                                           
                                           # normalized coordinate
                                           group_1_coord = list(coord_fcn(all.data_precision[[i]]$trials[[j]]$group_1_coord,width_height)),
                                           group_2_coord = list(coord_fcn(all.data_precision[[i]]$trials[[j]]$group_2_coord,width_height)),
                                           
                                           all_coordinates = list(rbind(coord_fcn(all.data_precision[[i]]$trials[[j]]$group_1_coord,width_height),
                                                                        coord_fcn(all.data_precision[[i]]$trials[[j]]$group_2_coord,width_height))),
                                           response_coordinate = list(c(all.data_precision[[i]]$trials[[j]]$response_coord$x/width_height,
                                                                        all.data_precision[[i]]$trials[[j]]$response_coord$y/width_height)
                                           ),
                                           mean_group_1 = list(c(all.data_precision[[i]]$trials[[j]]$mean_group_1$x/width_height,
                                                                 all.data_precision[[i]]$trials[[j]]$mean_group_1$y/width_height)
                                           ),
                                           mean_group_2 = list(c(all.data_precision[[i]]$trials[[j]]$mean_group_2$x/width_height,
                                                                 all.data_precision[[i]]$trials[[j]]$mean_group_2$y/width_height)
                                           ),
                                           mean_all = list(c(all.data_precision[[i]]$trials[[j]]$mean_all$x/width_height,
                                                             all.data_precision[[i]]$trials[[j]]$mean_all$y/width_height)
                                           )
                                    )
    )
  }
}

save(trial_response_precision,file="Experiment_2_dat.Rdata")

#Error analysis (mean estimation task)
tb.errors_precision= trial_response_precision %>% 
  # filter(trial_number>=3,trial_number<=402) %>%
  filter(trial_number>=6,group_1_size != 2, group_1_size !=8) %>%
  rowwise() %>% 
  mutate(size_ratio = group_1_size/group_2_size,
         sd_ratio = group_1_sd/group_2_sd,
         mean_index = mean_index,
         mean_all_recenter = list(c(mean_all[1]-0.5,mean_all[2]-0.5)),
         all_coordinates_recenter = list(recenter(all_coordinates,mean_all)),
         group_1_coord_recenter = list(recenter(group_1_coord,mean_all)),
         group_2_coord_recenter = list(recenter(group_2_coord,mean_all)),
         response_recenter = list(response_coordinate - mean_all),
         
         convex_hull_coord = list(compute_ch(all_coordinates_recenter)),
         mean_ch = list(colMeans(convex_hull_coord)),
         
         covariance = list(cov(all_coordinates_recenter)),
         eigenval = list(eigen(covariance)),
         eigenvec = list(eigen(covariance)$vector),
         group_1_mean_x= mean(group_1_coord_recenter$x),
         group_1_mean_y= mean(group_1_coord_recenter$y),
         group_2_mean_x= mean(group_2_coord_recenter$x),
         group_2_mean_y= mean(group_2_coord_recenter$y),
         
         proj.x_all = list(as.matrix(all_coordinates_recenter)%*%eigenvec[,1]),
         proj.y_all = list(as.matrix(all_coordinates_recenter)%*%eigenvec[,2]),
         proj.x_all_mean = mean(proj.x_all),
         proj.y_all_mean = mean(proj.y_all),
         
         proj.x_all_ch = list(as.matrix(convex_hull_coord)%*%eigenvec[,1]),
         proj.y_all_ch = list(as.matrix(convex_hull_coord)%*%eigenvec[,2]),
         proj.x_all_mean_ch = mean(proj.x_all_ch),
         proj.y_all_mean_ch = mean(proj.y_all_ch),
         
         proj.x_group_1 = list(as.matrix(group_1_coord_recenter)%*%eigenvec[,1]),
         proj.y_group_1 = list(as.matrix(group_1_coord_recenter)%*%eigenvec[,2]),
         proj.x_group_2 = list(as.matrix(group_2_coord_recenter)%*%eigenvec[,1]),
         proj.y_group_2 = list(as.matrix(group_2_coord_recenter)%*%eigenvec[,2]),
         
         proj.x_group_1_mean = mean(proj.x_group_1),
         proj.y_group_1_mean = mean(proj.y_group_1),
         proj.x_group_2_mean = mean(proj.x_group_2),
         proj.y_group_2_mean = mean(proj.y_group_2),
         proj.x_response = response_recenter%*%eigenvec[,1],
         proj.y_response = response_recenter%*%eigenvec[,2],
         min_x_proj = min(proj.x_all),
         max_x_proj = max(proj.x_all),
         min_y_proj = min(proj.y_all),
         max_y_proj = max(proj.y_all),
         # Check responses
         # responses located within the boundary of objects (main_axis)?
         wihtin_boundary_x = ifelse(proj.x_response>min_x_proj && proj.x_response<max_x_proj, "T", "F"),
         
         # responses located between the group means?
         between_means_x = ifelse(proj.x_response>min(proj.x_group_1_mean,proj.x_group_2_mean) &&
                                    proj.x_response<max(proj.x_group_1_mean,proj.x_group_2_mean),
                                  "T","F"),
         # Absolute error
         abs_error_to_all = abs_err_dist(response_recenter,c(0,0)),
         
         group_1_weight = compute_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_response)$weight_group_1,
         # group_1_weight = ifelse(group_1_weight_raw<=0, 0.001, group_1_weight_raw),
         true_group_1_weight = group_1_size/(group_1_size + group_2_size),
         group_2_weight= 1-group_1_weight,
         true_group_2_weight = group_2_size/(group_1_size + group_2_size),
         group_1_weight_edge = compute_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_1, proj.x_group_2, proj.x_response)$weight_group_1,
         group_2_weight_edge = 1-group_1_weight_edge
  )

# Data cleaning and sanity checks (exclusion)
# Exclusion criterion

#Check the out-of-boundary response trials
coord_out = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% 
  pull(all_coordinates)

# Add index column for each dataframes in a list
coord_out = coord_out %>% Map(cbind, ., trial_num = seq_along(.),type = "stimuli") %>% do.call(rbind,.)

coord_response_out = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% pull(response_coordinate) %>% do.call(rbind,.) %>% 
  as.data.frame() %>% mutate(trial_num = 1:nrow(.),type = "response")
colnames(coord_response_out)[1:2] = c("x", "y")
all_coord_out = rbind(coord_out, coord_response_out)
all_coord_out %>% ggplot(aes(x=x,y=y,color=type)) +geom_point() + facet_wrap(trial_num~.)

# within_boundary_x 
# Likely due to button misclick
tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% print(n=100)
tb.errors_precision = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="T",] 

# SD pairs:
# [1,2],[1,1],[1,0.5],[2,0.5]
# SD ratios (group_1/group_2): 1/2, 1, 2, 4
#Mean absolute error for individual subject
mean_abs_error_precision = tb.errors_precision %>% 
  filter(size_ratio == 1,sd_ratio==1) %>%
  group_by(subject) %>% 
  summarise(mean_abs_error = mean(abs_error_to_all)) %>% 
  mutate(z_score = (mean_abs_error - mean(mean_abs_error))/sd(mean_abs_error)) 

mean_abs_error_precision %>%  pull(mean_abs_error) %>% hist()
mean_abs_error_precision %>%filter(z_score>2)
mean_abs_error_precision[mean_abs_error_precision$mean_abs_error %>% order(decreasing=T),]

# Check each subject
tb.errors_precision %>%
  filter(subject>20, subject <=25, sd_ratio == 1) %>%
  # filter(size_ratio==0.25) %>% 
  ggplot(aes(x=as.factor(group_2_size), y = as.double(group_1_weight))) + 
  geom_point()+
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  # geom_smooth(method = "lm")+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_1_weight), shape=17, size=3, color = "red")+
  coord_cartesian(ylim = c(0,0.75))+ 
  facet_grid(group_1_size~subject)

# Given group sizes
tb.errors_precision %>% 
  filter(group_1_size==1,group_2_size == 4) %>%
  # filter(size_ratio==1) %>%
  ggplot(aes(x=sd_ratio, y = group_1_weight)) + 
  # geom_boxplot()+
  geom_point(shape=16,size=3)+
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  geom_point(aes(x=sd_ratio, y = true_group_1_weight), shape=17, size=3, color = "red") +
  facet_wrap(subject~size_ratio)+
  coord_cartesian(ylim = c(0,1))
# exclusion
all.data_precision[[25]]$client$sid

# Mean estimate analysis
# Group_1 weight vs.Group_2 cardinality
# raw data (subject)
tb.errors_precision$group_1_size = as.factor(tb.errors_precision$group_1_size)
tb.errors_precision$group_2_size = as.factor(tb.errors_precision$group_2_size)
tb.errors_precision$size_ratio = as.factor(tb.errors_precision$size_ratio)
tb.errors_precision$sd_ratio = as.factor(tb.errors_precision$sd_ratio)

# Average (equal cardinality)
# equal_cardinality data
tb.errors_equal = tb.errors_precision %>% 
  filter(as.numeric(group_1_size)>1) 

average_error_equal_subject = tb.errors_equal %>%  
  group_by(subject, group_1_size, group_2_size, sd_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight))

average_error_equal = tb.errors_equal %>%  
  group_by(subject, group_1_size, group_2_size, sd_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size,sd_ratio) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "group means")

#Identify the two trends
# upward trending
subject_upward = average_error_equal_subject %>% filter(group_1_size == 64,sd_ratio == 4, group_1_weight>0.5) %>% pull(subject)
subject_downward = average_error_equal_subject %>% filter(group_1_size == 64,sd_ratio == 4, group_1_weight<=0.5) %>% pull(subject)

tb.errors_equal_upward = tb.errors_equal %>% 
  filter(subject %in% subject_upward)

tb.errors_equal_downward = tb.errors_equal %>% 
  filter(subject %in% subject_downward)

#plot
average_error_equal %>% 
  ggplot(aes(x=sd_ratio,y=mean_group_1_weight,color=group_1_size,group=group_1_size)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=sd_ratio,y=mean_group_1_weight),size=1.5)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.5)+
  scale_color_manual(values=c("red","blue","green"))+
  
  geom_point(aes(x=sd_ratio, y = true_group_1_weight),shape=17,size=3,color="darkgrey")+
  geom_line(aes(x=sd_ratio, y = true_group_1_weight,group=2),color="darkgrey",size=1.5)+
  coord_cartesian(ylim = c(0.3,0.6))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank())+
  labs(x="SD ratio (small/large)",
       y="Mean small group weight")+
  guides(color=guide_legend(title= "Small group cardinality"))

#faceted
average_error_equal %>% 
  ggplot(aes(x=sd_ratio,y=mean_group_1_weight,color=group_1_size,group=group_1_size)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=sd_ratio,y=mean_group_1_weight),size=1.5)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.5)+
  scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a"))+
  
  geom_point(aes(x=sd_ratio, y = true_group_1_weight),shape=17,size=3,color="darkgrey")+
  geom_line(aes(x=sd_ratio, y = true_group_1_weight,group=2),color="darkgrey",size=1.5)+
  
  
  geom_point(data = average_error_equal_subject, aes(x=sd_ratio,y=group_1_weight,color=group_1_size,group=subject),alpha=0.2)+
  geom_line(data = average_error_equal_subject, aes(x=sd_ratio,y=group_1_weight,color=group_1_size,group=subject),alpha=0.2)+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(group_2_size~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        # strip.text.x = element_text(size = 14),
        strip.text.x = element_blank(),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))+
  labs(x="Dispersion multiplier ratio (outlier/main)",
       y="Mean outlier-cluster weight")+
  guides(color = "none")

  # guides(color=guide_legend(title= "Small group cardinality"))

# Inner boundary change
tb.errors_equal = tb.errors_equal %>% 
  mutate(group_1_boundary_weight = group_1_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_2),
         group_2_boundary_weight = group_2_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_2),
  )

tb.errors_equal %>% 
  group_by(group_1_size,sd_ratio) %>% 
  summarise(mean_group_1 = mean(group_1_boundary_weight),
            mean_group_2 = mean(group_2_boundary_weight))

# Histogram
tb.errors_equal %>%
  ggplot(aes(x=group_1_weight))+
  geom_histogram(binwidth = 0.01, fill = "dark grey")+
  geom_vline(data=average_error_equal,aes(xintercept = mean_group_1_weight),color="blue",size = 1.2,alpha=0.6)+
  geom_vline(aes(xintercept = true_group_1_weight),color="red",size = 1.2 ,linetype = "longdash",alpha=0.6)+
  facet_grid(sd_ratio~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 15)),
        axis.title.y=element_text(margin = margin(r = 15))) +
  labs(x = "Main group weight")

#edge weight
average_error_equal_edge = tb.errors_equal %>%  
  group_by(subject, group_1_size, group_2_size, sd_ratio) %>% 
  summarise(n = n(), 
            group_1_weight_edge = mean(group_1_weight_edge),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size,sd_ratio) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight_edge),
            se_group_1_weight = sd(group_1_weight_edge)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "edge")

average_error_equal_edge %>% 
  ggplot(aes(x=sd_ratio,y=mean_group_1_weight,color=group_1_size,group=group_1_size)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=sd_ratio,y=mean_group_1_weight))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.2)+
  geom_point(aes(x=sd_ratio, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=sd_ratio, y = true_group_1_weight,group=2),color="red")+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  coord_cartesian(ylim=c(0.3,0.8))+
  facet_wrap(group_2_size~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank())+
  labs(x="SD ratio (small/large)",
       y="Mean small group weight")+
  guides(color=guide_legend(title= "Small group cardinality"))

# Combined
average_error_equal %>% 
  rbind(average_error_equal_edge) %>% 
  ggplot(aes(x=sd_ratio,y=mean_group_1_weight,color = reference_frame)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=sd_ratio,y=mean_group_1_weight,linetype = reference_frame,group = reference_frame),size=1.5)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.2)+
  geom_point(aes(x=sd_ratio, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=sd_ratio, y = true_group_1_weight,group=2),color="red",size=1.5)+
  scale_linetype_manual(values=c("dashed", "solid"))+
  scale_color_manual(values=c("green", "black"))+
  
  # geom_point(data = average_error_equal_subject, aes(x=sd_ratio,y=group_1_weight,color=group_1_size,group=subject))+
  # geom_line(data = average_error_equal_subject, aes(x=sd_ratio,y=group_1_weight,color=group_1_size,group=subject))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(group_2_size~group_1_size)+
  coord_cartesian(ylim = c(0.3,0.6))+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 15)),
        axis.title.y=element_text(margin = margin(r = 15)))+
  labs(x="SD ratio (small/large)",
       y="Mean small group weight")


tb.errors_precision = tb.errors_precision %>% mutate(
  proj.x_group_2_boundary = ifelse(proj.x_group_1_mean > proj.x_group_2_mean, max(proj.x_group_2), min(proj.x_group_2)),
  group_2_boundary_weight = compute_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_2_boundary)$weight_group_1)

tb.errors_precision %>% filter(size_ratio == 1,sd_ratio==1) %>% pull(group_2_boundary_weight) %>% mean()
tb.errors_precision %>% filter(size_ratio == 1,sd_ratio==0.5) %>% pull(group_2_boundary_weight) %>% mean()
# edge difference: 0.125 - 0.06 = 0.065

#Histogram
#equal size
mean_weight_sd_ratio = tb.errors_precision %>% filter(size_ratio == 1) %>% group_by(group_1_size,sd_ratio) %>% 
  summarise(mean_weight = mean(group_1_weight))

tb.errors_precision %>% subset(size_ratio == 1) %>% ggplot(aes(x=group_1_weight)) +
  geom_histogram(binwidth = 0.02, fill = "dark grey")+
  geom_vline(data=mean_weight_sd_ratio,aes(xintercept = mean_weight),color="blue",size = 1.5)+
  geom_vline(aes(xintercept = true_group_1_weight),color="red",size = 1.5 ,linetype = "longdash")+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.5, group_2_size >= 4), aes(xintercept = 0.13),color = "green",size = 1.5, linetype = "longdash")+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_2_size >= 4),aes(xintercept = 0.14),color = "green",size = 1.5, linetype = "longdash")+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_2_size >= 4),aes(xintercept = 0.15),color = "green",size = 1.5, linetype = "longdash")+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_2_size >= 4),aes(xintercept = 0.17),color = "green",size = 1.5, linetype = "longdash")+
  # coord_cartesian(xlim = c(0,1))+
  facet_grid(group_1_size~sd_ratio)+
  # facet_grid(sd_ratio~size_ratio)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 15)),
        axis.title.y=element_text(margin = margin(r = 15))) +
  labs(x = "Main (Large) group weight")



# Statistical Analysis
library(rstatix)
library(tidyverse)
library(lsr)
average_error_equal_subject$subject = as.factor(average_error_equal_subject$subject)
average_error_equal_subject = average_error_equal_subject %>% ungroup()
res.aov = average_error_equal_subject %>% anova_test(dv = group_1_weight, wid = subject, within = c(group_1_size, sd_ratio))

get_anova_table(res.aov, correction = "GG")

# t-tests for accuracy
average_error_equal_subject %>% filter(group_1_size == "4", sd_ratio == "4") %>% pull(group_1_weight) %>% t.test(.,mu = 0.5)
cohensD(average_error_equal_subject %>% filter(group_1_size == "4", sd_ratio == "4") %>% pull(group_1_weight),mu = 0.5)

average_error_equal_subject %>% filter(group_1_size == "16", sd_ratio == "4") %>% pull(group_1_weight) %>% t.test(.,mu = 0.5)

average_error_equal_subject %>% filter(group_1_size == "64", sd_ratio == "0.5") %>% pull(group_1_weight) %>% t.test(.,mu = 0.5)
cohensD(average_error_equal_subject %>% filter(group_1_size == "64", sd_ratio == "0.5") %>% pull(group_1_weight),mu = 0.5)

average_error_equal_subject %>% filter(group_1_size == "64", sd_ratio == "4") %>% pull(group_1_weight) %>% t.test(.,mu = 0.5)
cohensD(average_error_equal_subject %>% filter(group_1_size == "64", sd_ratio == "4") %>% pull(group_1_weight),mu = 0.5)

#Outlier conditions
#Compute the weight of the edge of group 2
tb.errors_outlier = tb.errors_precision %>% filter(group_1_size == 1) %>% 
  mutate(group_2_boundary_weight = group_2_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_2))

#inconsistent trials (true mean inside the group while response outside the group)
tb.errors_outlier = tb.errors_outlier %>% mutate(true_mean_in_group = true_group_1_weight<group_2_boundary_weight,
                                           estimate_in_group = group_1_weight<group_2_boundary_weight,
                                           inconsistent = true_mean_in_group != estimate_in_group)

tb.errors_outlier %>% 
  filter(as.numeric(as.character(group_2_size))>=16) %>% 
  group_by(subject) %>% 
  summarise(prop_inconsistent = sum(inconsistent)/sum(true_mean_in_group)) %>% print(n=100)

# Identify subject's with edge bias
# Criterion: average mean estimates are both outside the large group
outlier_mean_subject = tb.errors_outlier %>% 
  group_by(subject,group_1_size,group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            mean_group_1_weight = mean(group_1_weight),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))

outlier_mean_subject$group_2_size = as.character(outlier_mean_subject$group_2_size)
edge_effect = outlier_mean_subject %>%
  filter(as.numeric(group_2_size) == 64, as.numeric(size_ratio) == 1) %>% 
  group_by(subject) %>% 
  summarise(mean_group_1_weight = mean(mean_group_1_weight),
            mean_group_2_boundary_weight = mean(mean_group_2_boundary_weight)) %>% 
  mutate(inconsistent = mean_group_1_weight > mean_group_2_boundary_weight)

# Subjects who are categorized as having edge effect (edge correction)
edge_subject = edge_effect[edge_effect$inconsistent == TRUE,] %>% pull(subject)
length(edge_subject)
# Subjects who are categorized as having no edge effect (no edge correction)
non_edge_subject =  edge_effect[edge_effect$inconsistent == FALSE,] %>% pull(subject)
length(non_edge_subject)

tb.errors_outlier$group_2_sd = as.factor(tb.errors_outlier$group_2_sd)

#Average across subject
outlier_mean = tb.errors_outlier %>%  
  group_by(subject, group_1_size, group_2_size, group_2_sd) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size,group_2_sd) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "group means")

outlier_subject_mean = tb.errors_outlier %>% 
  group_by(subject,group_1_size,group_2_size,group_2_sd,size_ratio) %>% 
  summarise(n = n(), 
            mean_group_1_weight = mean(group_1_weight),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))

#plot average response
outlier_mean %>% 
  ggplot(aes(x=group_2_sd,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_sd,y=mean_group_1_weight,group = 3),size=1.5)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.2)+
  geom_point(aes(x=group_2_sd, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=group_2_sd, y = true_group_1_weight,group=2),color="red",size=1.5)+
  geom_point(data = outlier_subject_mean, aes(x=group_2_sd,y=mean_group_1_weight,color=group_1_size,group=subject))+
  geom_line(data = outlier_subject_mean, aes(x=group_2_sd,y=mean_group_1_weight,color=group_1_size,group=subject))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(group_2_size~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 15)),
        axis.title.y=element_text(margin = margin(r = 15)))+
  labs(x="SD ratio (small/large)",
       y="Mean small group weight")

#raw response distribution
tb.errors_outlier %>% 
  ggplot(aes(x=group_1_weight))+
  geom_histogram(binwidth = 0.02, fill = "dark grey")+
  # facet_grid(group_2_size~sd_ratio)
  facet_grid(group_2_size~.)


tb.errors_outlier %>% filter(group_1_size==1, sd_ratio==0.5,group_2_size==64) %>% pull(group_1_weight) %>% dip.test(.)

outlier_mean_edge = tb.errors_outlier %>% filter(subject%in%edge_subject) %>% 
  group_by(subject,group_2_sd,group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_2_sd,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "edge_bias")

outlier_mean_edge_corrected = tb.errors_outlier %>%  
  filter(subject%in%edge_subject) %>%
  # filter(subject%in%non_edge_subject) %>%
  group_by(subject,group_2_sd,group_2_size) %>%
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight_edge),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_2_sd,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "corrected_edge_bias")

outlier_mean_edge %>% 
  ggplot(aes(x=group_2_sd,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_sd,y=mean_group_1_weight,group=group_2_size))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.09,size=1)+
  geom_point(aes(x=group_2_sd, y = true_group_1_weight),shape=17,size=3)+
  geom_line(aes(x=group_2_sd, y = true_group_1_weight,group=group_2_size))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  coord_cartesian(ylim=c(0,0.5))+
  facet_grid(.~group_2_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 15)),
        axis.title.y=element_text(margin = margin(r = 15)))+
  labs(x="Large group dispersion",
       y="Mean outlier weight")

#edge weights  
tb.errors_outlier$group_2_sd = as.factor(tb.errors_precision$group_2_sd)

outlier_mean_edge_corrected %>% ggplot(aes(x=group_2_sd,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_sd,y=mean_group_1_weight,group=group_2_size))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.09,size=1)+
  geom_point(aes(x=group_2_sd, y = true_group_1_weight),shape=17,size=3)+
  geom_line(aes(x=group_2_sd, y = true_group_1_weight,group=group_2_size))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_grid(.~group_2_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank())+
  labs(x="Large group dispersion",
       y="Mean small group weight")

tb.errors_outlier$group_2_sd = as.factor(tb.errors_precision$group_2_sd)
#non-edge subjects
outlier_mean_no_edge = tb.errors_outlier %>%  filter(subject%in%non_edge_subject) %>%
  group_by(subject,group_2_sd,group_2_size) %>%
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_2_sd,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "no edge bias")

outlier_mean_no_edge %>% 
  ggplot(aes(x=group_2_sd,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_sd,y=mean_group_1_weight,group=group_2_size))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.09,size=1)+
  geom_point(aes(x=group_2_sd, y = true_group_1_weight),shape=17,size=3)+
  geom_line(aes(x=group_2_sd, y = true_group_1_weight,group=group_2_size))+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_grid(.~group_2_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank())+
  labs(x="Large group dispersion",
       y="Mean small group weight")

# combined 
outlier_mean_combined = outlier_mean_no_edge %>% rbind(outlier_mean_edge)
                                                       # outlier_mean_edge_corrected)
outlier_mean_edge_corrected
outlier_mean_combined$reference_frame = as.factor(outlier_mean_combined$reference_frame)

outlier_mean_combined %>% 
  ggplot(aes(x=group_2_sd,y=mean_group_1_weight,color =reference_frame,group = reference_frame)) + 
  geom_point(shape=16,size=2) + 
  geom_line(aes(x=group_2_sd,y=mean_group_1_weight,group = reference_frame,
      # linetype = as.factor(reference_frame)
  ),size=1.5)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1)+
  geom_point(aes(x=group_2_sd, y = true_group_1_weight),shape=17,size=2,color="darkgrey")+
  geom_line(aes(x=group_2_sd, y = true_group_1_weight),color="darkgrey",size=1.5)+
  # facet_wrap(.~group_2_size)+
  # scale_linetype_manual(values = c("solid", "dashed", "solid"),
  #                       labels=c( "edge-means", "means-means"))+
  scale_color_manual(values = c("#4daf4a", "#e41a1c"),
                     labels=c("Edge bias", "Without edge bias"))+
  # coord_cartesian(ylim = c(0,0.75))+
  geom_point(data = outlier_mean_edge_corrected, aes(x=group_2_sd,y=mean_group_1_weight), color = "#4daf4a",size=2)+
  geom_line(data = outlier_mean_edge_corrected, aes(x=group_2_sd,y=mean_group_1_weight), color = "#4daf4a", linetype = "dashed",size=1.5)+
  geom_errorbar(data = outlier_mean_edge_corrected, aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1,
                color = "#4daf4a")+
  facet_wrap(.~group_2_size)+
  theme_bw()+
  theme(legend.position = c(0.36, 0.93), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA),
        axis.text = element_text(size= 12),
        # strip.text.x = element_text(size = 14),
        strip.text.x = element_blank(),
        text = element_text(size= 32),
        # text = element_text(size= 16),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))+
  labs(x = "Main-cluster dispersion multiplier", y = "Mean Outlier-cluster weight", color= "Reference frame")+
  guides(color = "none")
# guides(color = guide_legend(title = NULL))
# guides(linetype = "none")

# Statistical Analysis
outlier_mean_edge_corrected_subject = tb.errors_outlier %>%  
  filter(subject%in%edge_subject) %>%
  # filter(subject%in%non_edge_subject) %>%
  group_by(subject,group_2_sd,group_2_size) %>%
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight_edge),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame="corrected_edge")

outlier_mean_no_edge_subject = tb.errors_outlier %>%  filter(subject%in%non_edge_subject) %>%
  group_by(subject,group_2_sd,group_2_size) %>%
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame="no_edge")


outlier_mean_combined_subject = outlier_mean_edge_corrected_subject %>% rbind(outlier_mean_no_edge_subject)

library(rstatix)
library(tidyverse)
outlier_mean_combined_subject$subject = as.factor(outlier_mean_combined_subject$subject)
outlier_mean_combined_subject = outlier_mean_combined_subject %>% ungroup()
res.aov = outlier_mean_combined_subject %>% anova_test(dv = group_1_weight, wid = subject, between = reference_frame, within = c(group_2_size, group_2_sd))

get_anova_table(res.aov, correction = "GG")

one.way <- outlier_mean_combined_subject %>%
  group_by(group_2_size,reference_frame) %>%
  anova_test(dv = group_1_weight, wid = subject, within = c(group_2_sd)) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
one.way
outlier_mean_no_edge_subject %>% filter(group_2_size == 4,group_2_sd %in% c("1","2")) %>% t.test(data = .,group_1_weight~group_2_sd,paired = T)
cohensD(outlier_mean_no_edge_subject %>% filter(group_2_size == 4,group_2_sd =="2") %>% pull(group_1_weight),mu=0.2)

outlier_mean_no_edge_subject %>% filter(group_2_size == 64,group_2_sd %in% c("1","2")) %>% t.test(data = .,group_1_weight~group_2_sd,paired = T)
cohensD(outlier_mean_no_edge_subject %>% filter(group_2_size == 4,group_2_sd =="1") %>% pull(group_1_weight),
        outlier_mean_no_edge_subject %>% filter(group_2_size == 4,group_2_sd =="2") %>% pull(group_1_weight))

                                        