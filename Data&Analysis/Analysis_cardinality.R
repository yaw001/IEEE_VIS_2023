library(rjson)
library(tidyverse)
library(lsr)
library(sp)
library(splancs)

setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab_data/Gaussian_Cardinality_2_data")
# setwd("/Users/young/Library/Mobile Documents/com~apple~CloudDocs/Desktop/UCSD/Research/Mean_position_perception_lab_data/Gaussian_Cardinality_2_data")

#data transformation
all.data_size = list()
subject = 1
for(file.name in list.files(pattern = '*.json')) {
  json_file = fromJSON(file = file.name)
  json_file[['subject']] = subject
  all.data_size[[subject]] = json_file
  subject = subject + 1
}

setwd("/Users/young/Desktop/UCSD/Research/IEEE_VIS_2023/Data&Analysis")
source("helper.R")

trial_response_size = tibble()
num_subj = length(all.data_size)
num_subj
for(i in 1:num_subj) {
  for(j in 1:406) {
    trial_response_size = bind_rows(trial_response_size,
                                    tibble(subject = all.data_size[[i]]$subject,
                                           sid = all.data_size[[i]]$client$sid,
                                           response_time = all.data_size[[i]]$trials[[j]]$response_time,
                                           trial_number = all.data_size[[i]]$trials[[j]]$trialNumber,
                                           mean_index = all.data_size[[i]]$trials[[j]]$mean_index,
                                           group_1_size = all.data_size[[i]]$trials[[j]]$group_1_size,
                                           group_2_size = all.data_size[[i]]$trials[[j]]$group_2_size,
                                           group_1_num_est = all.data_size[[i]]$trials[[j]]$num_estimate$num_response_1,
                                           group_2_num_est = all.data_size[[i]]$trials[[j]]$num_estimate$num_response_2,
                                           inner_width = all.data_size[[i]]$trials[[j]]$inner_width,
                                           inner_height = all.data_size[[i]]$trials[[j]]$inner_height,
                                           width_height = min(inner_width,inner_height)*0.98,
                                           aspect_ratio = inner_width/inner_height,
                                           # normalized coordinate
                                           group_1_coord = list(coord_fcn(all.data_size[[i]]$trials[[j]]$group_1_coord,width_height)),
                                           group_2_coord = list(coord_fcn(all.data_size[[i]]$trials[[j]]$group_2_coord,width_height)),
                                           
                                           all_coordinates = list(coord_all_fcn(all.data_size[[i]]$trials[[j]]$all_coordinates,width_height)),
                                           response_coordinate = list(c(all.data_size[[i]]$trials[[j]]$response_coord$x/width_height,
                                                                        all.data_size[[i]]$trials[[j]]$response_coord$y/width_height)
                                           ),
                                           mean_group_1 = list(c(all.data_size[[i]]$trials[[j]]$mean_group_1$x/width_height,
                                                              all.data_size[[i]]$trials[[j]]$mean_group_1$y/width_height)
                                           ),
                                           mean_group_2 = list(c(all.data_size[[i]]$trials[[j]]$mean_group_2$x/width_height,
                                                                 all.data_size[[i]]$trials[[j]]$mean_group_2$y/width_height)
                                           ),
                                           mean_all = list(c(all.data_size[[i]]$trials[[j]]$mean_all$x/width_height,
                                                             all.data_size[[i]]$trials[[j]]$mean_all$y/width_height)
                                           )
                                    )
    )
  }
}

# save(trial_response_size, file = "Experiment_1_dat.Rdata")
#Error analysis (mean estimation task)
tb.errors_size = trial_response_size %>% filter(trial_number >= 6) %>%
  rowwise() %>% 
  mutate(size_ratio = group_1_size/group_2_size,
         mean_index = mean_index,
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
         
         group_2_boundary_weight = group_2_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_2),
         group_1_weight_edge = compute_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_group_1, proj.x_group_2, proj.x_response)$weight_group_1
         
  )

# Data cleaning and sanity checks (exclusion)
# Exclusion criterion
#Check the out-of-boundary response trials
coord_out = tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% 
  pull(all_coordinates)
# Add index column for each dataframes in a list
coord_out = coord_out %>% Map(cbind, ., trial_num = seq_along(.),type = "stimuli") %>% do.call(rbind,.)

coord_response_out = tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% pull(response_coordinate) %>% do.call(rbind,.) %>% 
  as.data.frame() %>% mutate(trial_num = 1:nrow(.),type = "response")
colnames(coord_response_out)[1:2] = c("x", "y")
all_coord_out = rbind(coord_out, coord_response_out)
all_coord_out %>% ggplot(aes(x=x,y=y,color=type)) +geom_point() + facet_wrap(trial_num~.)

# within_boundary_x 
# Likely due to button misclick (8 trials removed)
tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% print(n=100)
tb.errors_size = tb.errors_size[tb.errors_size$wihtin_boundary_x=="T",] 

# weird trials (weighting group 1 (smaller group) more than 0.75)
# tb.errors_size[tb.errors_size$group_1_weight>=0.75,]
tb.errors_size = tb.errors_size[(tb.errors_size$group_1_weight<=0.8),]

# Sanity check: Mean absolute error for individual subject in the trials with size_ratio = 1
mean_abs_error_size = tb.errors_size %>% 
  filter(size_ratio == 1) %>%
  group_by(subject) %>% 
  summarise(mean_abs_error = mean(abs_error_to_all)) %>% 
  mutate(z_score = (mean_abs_error - mean(mean_abs_error))/sd(mean_abs_error)) 

mean_abs_error_size %>%  pull(mean_abs_error) %>% hist()
mean_abs_error_size %>% filter(z_score>2)
mean_abs_error_size[mean_abs_error_size$mean_abs_error %>% order(decreasing=T),]

# Check each subject
tb.errors_size %>%
  filter(subject >30, subject <=35) %>%
  # filter(size_ratio==0.25) %>% 
  ggplot(aes(x=as.factor(group_2_size), y = as.double(group_1_weight))) + 
  geom_point()+
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  # geom_smooth(method = "lm")+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_1_weight), shape=17, size=3, color = "red")+
  coord_cartesian(ylim = c(0,0.75))+ 
  facet_grid(group_1_size~subject)

#Exclusion:
# 9,14,12,27,30,32,34,35,41
# all.data_size[[15]]$client$sid


### Analysis
# Y-position check
true_mean_y = data.frame(proj_y_mean = tb.errors_size$proj.y_all_mean, type = "true mean along minor axis")
response_y = data.frame(proj_y_mean =tb.errors_size$proj.y_response, type = "response")
# mean_y = rbind(true_mean_y,response_y)
response_y %>% ggplot(aes(x=proj_y_mean)) + 
  geom_histogram(binwidth = 0.001) +
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        text = element_text(size= 12),
        panel.grid = element_blank()) +
  labs(x = "Error along the minor axis")

# Calculate the average boundary weights
tb.errors_size %>% 
  group_by(group_1_size,size_ratio,group_2_size) %>% 
  summarise(mean_boundary_weight = mean(group_2_boundary_weight))

#average boundary weights by size ratio
tb.errors_size %>% 
  group_by(size_ratio) %>% 
  summarise(mean_boundary_weight = mean(group_2_boundary_weight))

# Histogram of the raw data given size ratio and group_1_size
mean_weight_size_ratio = tb.errors_size %>% group_by(group_1_size,size_ratio) %>% 
  summarise(mean_weight = mean(group_1_weight))

mean_weight_size_ratio_only = tb.errors_size %>% group_by(size_ratio) %>% 
  summarise(mean_weight = mean(group_1_weight))

tb.errors_size %>%
  # filter(group_1_size==1) %>% 
  ggplot(aes(x=group_1_weight)) +
  geom_histogram(binwidth = 0.01, fill = "dark grey")+
  geom_vline(data=mean_weight_size_ratio,aes(xintercept = mean_weight),color="blue",size = 1.2,alpha=0.6)+
  geom_vline(aes(xintercept = true_group_1_weight),color="red",size = 1.2 ,linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.5, group_1_size == 2), aes(xintercept = 0.0516),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.5, group_1_size == 4), aes(xintercept = 0.130),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.5, group_1_size == 8), aes(xintercept = 0.122),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_1_size == 1), aes(xintercept = 0.0972),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_1_size == 2), aes(xintercept = 0.130),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_1_size == 4), aes(xintercept = 0.122),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_1_size == 8), aes(xintercept = 0.170),color = "green",size = 1.5, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_1_size == 1), aes(xintercept = 0.130),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_1_size == 2), aes(xintercept = 0.122),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_1_size == 4), aes(xintercept = 0.170),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_1_size == 8), aes(xintercept = 0.184),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_1_size == 1), aes(xintercept = 0.122),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_1_size == 2), aes(xintercept = 0.170),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_1_size == 4), aes(xintercept = 0.184),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_1_size == 8), aes(xintercept = 0.195),color = "green",size = 1.2, linetype = "longdash",alpha=0.6)+
  # facet_grid(size_ratio~.)+
  facet_grid(group_1_size~size_ratio)+
  # facet_grid(size_ratio~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12))) +
  labs(x = "Outlier group weight")

#Histogram for size-ratio = 0.0625 (bimodal distribution suggesting using different reference frame)
tb.errors_size %>%
  # filter(group_1_size==1) %>%
  filter(size_ratio<0.25) %>%
  ggplot(aes(x=group_1_weight)) +
  geom_histogram(binwidth = 0.02, fill = "grey",alpha=0.9)+
  # geom_vline(data=mean_weight_size_ratio_only,aes(xintercept = mean_weight),color="black",size = 1.2,alpha=0.8)+
  geom_vline(aes(xintercept = true_group_1_weight),color="black",size = 1.5 ,linetype = "longdash")+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.5, group_2_size >= 4), aes(xintercept = 0.107),color = "green",size = 1.5, linetype = "longdash",alpha=0.8)+
  # geom_vline(data = filter(tb.errors_size, size_ratio == 0.25, group_2_size >= 4),aes(xintercept = 0.137),color = "#4daf4a",size = 1.5, linetype = "longdash")+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.125, group_2_size >= 4),aes(xintercept = 0.129),color = "#4daf4a",size = 1.5, linetype = "longdash")+
  geom_vline(data = filter(tb.errors_size, size_ratio == 0.0625, group_2_size >= 4),aes(xintercept = 0.175),color = "#4daf4a",size = 1.5, linetype = "longdash")+
  coord_cartesian(xlim = c(-0.2,1.2))+
  # facet_grid(size_ratio~.)+
  facet_grid(~size_ratio)+
  
  # facet_wrap(.~size_ratio)+
  theme_bw()+
  theme(
        axis.text = element_text(size= 12),
        # strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)),
        strip.text.x = element_blank()) +
  labs(x = "Outlier-cluster weight")

# average
# raw data (subject)
global_mean_subject = tb.errors_size %>% 
  group_by(subject,group_1_size,group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            mean_group_1_weight = mean(group_1_weight),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))

# Averaging across the Subjects' Mean performance grouped by outlier group cardinality
global_mean = tb.errors_size %>% group_by(subject, group_1_size, group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size,size_ratio) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) 

# True weight vs. outlier-cluster weight grouped by outlier cardinality 
global_mean %>% 
  ggplot(aes(x=true_group_1_weight,y=mean_group_1_weight))+
  geom_point(shape=16,size=2) + 
  geom_line(aes(x=true_group_1_weight,y=mean_group_1_weight, group =5),size = 1)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.01,size=1)+
  geom_point(aes(x=true_group_1_weight, y = true_group_1_weight),shape=17,size=2,color="red")+
  geom_line(aes(x=true_group_1_weight, y = true_group_1_weight,group=5),color="red",size=1.5)+
  theme_bw()+
  facet_wrap(.~group_1_size)+
  labs(x="True outlier-cluster weight", y = "Mean outlier-cluster weight")+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))

#Statistics
# global_mean_subject %>% 
#   filter(size_ratio<0.5) %>% 
#   lm(formula = mean_group_1_weight~true_group_1_weight) %>% summary()

global_mean_subject %>% 
  filter(size_ratio<1, group_1_size == 1) %>% 
  lm(formula = mean_group_1_weight~true_group_1_weight) %>% summary()
  
global_mean_subject %>% 
  filter(size_ratio<1, group_1_size == 2) %>% 
  lm(formula = mean_group_1_weight~true_group_1_weight) %>% summary()

global_mean_subject %>% 
  filter(size_ratio<1, group_1_size == 4) %>% 
  lm(formula = mean_group_1_weight~true_group_1_weight) %>% summary()

global_mean_subject %>% 
  filter(size_ratio<1, group_1_size == 8) %>% 
  lm(formula = mean_group_1_weight~true_group_1_weight) %>% summary()

# average performance grouped by cardinality ratio (aggregated across outlier group cardinality)
global_mean_ratio = tb.errors_size %>% group_by(subject, group_1_size, group_2_size, size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(size_ratio) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) 

# True weight vs. outlier-cluster weight grouped by cardinality ratio (aggregated across outlier group cardinality) 
global_mean_ratio %>% 
  ggplot(aes(x=true_group_1_weight,y=mean_group_1_weight))+
  geom_point(shape=16,size=2) + 
  geom_line(aes(x=true_group_1_weight,y=mean_group_1_weight, group =5),size = 1)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.01,size=1)+
  geom_point(aes(x=true_group_1_weight, y = true_group_1_weight),shape=17,size=2,color="red")+
  geom_line(aes(x=true_group_1_weight, y = true_group_1_weight,group=5),color="red",size=1.5)+
  theme_bw()+  
  labs(x="True outlier-cluster weight", y = "Mean outlier-cluster weight")+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))

# Identify subject's with edge bias
# Criterion: average mean estimates are both outside the large group for conditions of size_ratio = 0.125 and 0.0625
# Average weights for the group 2 boundary
group_2_boundary = tb.errors_size %>% filter(size_ratio<=0.125) %>% 
  group_by(group_1_size,size_ratio,group_2_size) %>% 
  summarise(mean_boundary_weight = mean(group_2_boundary_weight))
#inconsistent trials (true mean inside the group while response outside the group)
tb.errors_size = tb.errors_size %>% mutate(true_mean_in_group = true_group_1_weight<group_2_boundary_weight,
                                           estimate_in_group = group_1_weight<group_2_boundary_weight,
                                           inconsistent = true_mean_in_group != estimate_in_group)

# Subjects who are categorized as having edge effect (edge correction)
# Comparing average estimates and average boundary position
edge_effect = global_mean_subject %>%
  mutate(size_ratio = group_1_size/group_2_size) %>% 
  filter(size_ratio < 0.125) %>% 
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

# Using strict rule: never or almost never place estimates within boundary (prop_inconsistent > 0.9)
# min prop = 0.981
# edge_effect_2 = tb.errors_size %>% 
#   filter(size_ratio<0.125) %>% 
#   group_by(subject) %>% 
#   summarise(prop_inconsistent = sum(inconsistent)/sum(true_mean_in_group)) %>% print(n=100)
# # Subjects who are categorized as having edge effect (edge correction)
# edge_effect_2[order(edge_effect_2$prop_inconsistent,decreasing = T),]$prop_inconsistent %>% hist()
# edge_effect_2[order(edge_effect_2$prop_inconsistent,decreasing = T),] %>% print(n=100)
# edge_subject = edge_effect_2[edge_effect_2$prop_inconsistent>=0.9,] %>% pull(subject)
# non_edge_subject =  edge_effect[edge_effect_2$prop_inconsistent<0.9,] %>% pull(subject)

#Averaging across the subjects' mean performance with edge bias
#Without correction
global_mean_edge = tb.errors_size %>%
  filter(subject %in% edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))
#With correction
global_mean_non_edge = tb.errors_size %>%
  filter(subject %in% non_edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            mean_group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))

#Plot (with edge grouping)
global_mean %>% 
  ggplot(aes(x=as.factor(group_2_size),y=mean_group_1_weight))+
  geom_point(shape=16,size=1) + 
  geom_line(aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1)+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_1_weight),shape=17,size=2,color="red")+
  geom_line(aes(x=as.factor(group_2_size), y = true_group_1_weight,group=5),color="red",size=1.5)+
  # geom_hline(data = group_2_boundary, aes(yintercept = mean_boundary_weight),color="green",size = 1.2 ,linetype = "longdash",alpha=0.6)+
  
  # geom_point(data = group_2_boundary, aes(x = as.factor(group_2_size), y = mean_boundary_weight),color="green",size = 1, shape= 4,stroke=2)+
  
  geom_point(data = global_mean_edge, aes(x=as.factor(group_2_size),y=mean_group_1_weight),shape=16,size=1,color="blue") + 
  geom_line(data = global_mean_edge,aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1,color="blue",linetype="dashed")+
  geom_errorbar(data = global_mean_edge,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1,color="blue")+
  
  geom_point(data = global_mean_non_edge, aes(x=as.factor(group_2_size),y=mean_group_1_weight),shape=16,size=1,color="orange") + 
  geom_line(data = global_mean_non_edge,aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1,color="orange",linetype="dashed")+
  geom_errorbar(data = global_mean_non_edge,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1,color="orange")+
  
  coord_cartesian(ylim = c(0,0.75))+
  
  # geom_point(aes(x=as.factor(group_2_size), y = mean_group_2_boundary_weight),shape=17,size=3,color="red")+
  # geom_line(aes(x=as.factor(group_2_size), y = mean_group_2_boundary_weight,group=5),color="red",size=1.5)+
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))

# average performance for each subject given the group_size_1
global_mean_subject %>% 
  ggplot(aes(x=as.factor(group_2_size), y = mean_group_1_weight,group=subject)) + 
  geom_point(shape=16,size=1,alpha=0.12) + 
  geom_line(alpha=0.12)+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_1_weight),shape=17,size=2,color="red")+
  geom_line(aes(x=as.factor(group_2_size), y = true_group_1_weight,group=5),color="red",size=1.5)+
  # geom_hline(data = group_2_boundary, aes(yintercept = mean_boundary_weight),color="green",size = 1.2 ,linetype = "longdash",alpha=0.6)+
  geom_point(data = group_2_boundary, aes(x = as.factor(group_2_size), y = mean_boundary_weight),color="green",size = 1, shape= 4,stroke=2)+
  
  geom_point(data = global_mean, aes(x=as.factor(group_2_size),y=mean_group_1_weight),shape=16,size=1) + 
  geom_line(data = global_mean,aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1)+
  geom_errorbar(data = global_mean,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1)+
  
  geom_point(data = global_mean_edge, aes(x=as.factor(group_2_size),y=mean_group_1_weight),shape=16,size=1,color="blue") + 
  geom_line(data = global_mean_edge,aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1,color="blue",linetype="dashed")+
  geom_errorbar(data = global_mean_edge,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1,color="blue")+
  
  geom_point(data = global_mean_non_edge, aes(x=as.factor(group_2_size),y=mean_group_1_weight),shape=16,size=1,color="orange") + 
  geom_line(data = global_mean_non_edge,aes(x=as.factor(group_2_size),y=mean_group_1_weight, group =5),size = 1,color="orange",linetype="dashed")+
  geom_errorbar(data = global_mean_non_edge,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.2,size=1,color="orange")+
  
  coord_cartesian(ylim = c(0,0.75))+
  
  # geom_point(aes(x=as.factor(group_2_size), y = mean_group_2_boundary_weight),shape=17,size=3,color="red")+
  # geom_line(aes(x=as.factor(group_2_size), y = mean_group_2_boundary_weight,group=5),color="red",size=1.5)+
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        strip.text.x = element_text(size = 14),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        # legend.position = 'none',
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))

# Average performance with edge correction (with edge bias)
edge_weight_with_correction_subject = tb.errors_size %>%
  filter(subject %in% edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            # group_1_weight_edge = mean(group_1_weight_edge),
            group_1_weight = mean(group_1_weight_edge),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "edge with edge")

edge_weight_with_correction_subject[c("group_1_size","group_2_size","size_ratio","reference_frame")] = 
  edge_weight_with_correction_subject[c("group_1_size","group_2_size","size_ratio","reference_frame")] %>% lapply(factor)

edge_weight_with_correction = tb.errors_size %>%
  filter(subject %in% edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight_edge = mean(group_1_weight_edge),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight_edge),
            se_group_1_weight = sd(group_1_weight_edge)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "edge with edge")

edge_weight_with_correction[c("group_1_size","group_2_size")] = edge_weight_with_correction[c("group_1_size","group_2_size")] %>% lapply(factor)

# Average performance with edge group without edge correction (with edge bias)
edge_weight_without_correction_edge_subject = tb.errors_size %>%
  filter(subject %in% edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) 

edge_weight_without_correction_edge_subject[c("group_1_size","group_2_size")] = edge_weight_without_correction_edge_subject[c("group_1_size","group_2_size")] %>% lapply(factor)

edge_weight_without_correction_edge = tb.errors_size %>%
  filter(subject %in% edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "edge with group means")

edge_weight_without_correction_edge[c("group_1_size","group_2_size")] = edge_weight_without_correction_edge[c("group_1_size","group_2_size")] %>% lapply(factor)

# Average performance without edge correction (without edge bias)
edge_weight_without_correction_subject = tb.errors_size %>%
  filter(subject %in% non_edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size,size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "no edge with group means")

edge_weight_without_correction_subject[c("group_1_size","group_2_size","size_ratio", "reference_frame")] = 
  edge_weight_without_correction_subject[c("group_1_size","group_2_size","size_ratio","reference_frame")] %>% lapply(factor)

edge_weight_without_correction = tb.errors_size %>%
  filter(subject %in% non_edge_subject) %>% 
  group_by(subject, group_1_size, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  mutate(reference_frame = "no edge with group means")

edge_weight_without_correction[c("group_1_size","group_2_size")] = edge_weight_with_correction[c("group_1_size","group_2_size")] %>% lapply(factor)


# Combined
edge_weight =  edge_weight_without_correction_edge %>% rbind(
  edge_weight_without_correction,
  # edge_weight_with_correction
                                                    )
# using true outlier-cluster weight as x-axis
global_mean = global_mean %>% mutate(reference_frame = "raw")
global_mean$reference_frame = as.factor(global_mean$reference_frame)
edge_weight %>% 
  ggplot(aes(x=true_group_1_weight,y=mean_group_1_weight,color = as.factor(reference_frame))) + 
  geom_point(shape=16,size=2) + 
  geom_line(aes(x=true_group_1_weight,y=mean_group_1_weight,group = as.factor(reference_frame),
                # linetype = as.factor(reference_frame)
  ),size=1)+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.01,size=1)+
  geom_point(aes(x=true_group_1_weight, y = true_group_1_weight),shape=17,size=2,color="darkgrey")+
  geom_line(aes(x=true_group_1_weight, y = true_group_1_weight,group=5),color="darkgrey",size=1)+
  # scale_linetype_manual(values = c("solid", "dashed", "solid"),
  #                       labels=c( "edge-means", "means-means"))+
  scale_color_manual(values = c("#4daf4a", "#e41a1c"),
                     labels=c("Edge bias", "Without edge bias"))+
  # coord_cartesian(ylim = c(0,0.75))+
  geom_point(data = edge_weight_with_correction, aes(x=true_group_1_weight,y=mean_group_1_weight), color = "#4daf4a",size=2)+
  geom_line(data = edge_weight_with_correction, aes(x=true_group_1_weight,y=mean_group_1_weight), group = 5, color = "#4daf4a", linetype = "dashed",size=1)+
  geom_errorbar(data = edge_weight_with_correction, aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.01,size=1,
                color = "#4daf4a")+
  
  # geom_point(data = global_mean,aes(x=true_group_1_weight,y=mean_group_1_weight),color= "#984ea3",size=2) +
  # geom_line(data = global_mean,aes(x=true_group_1_weight,y=mean_group_1_weight), group = 5,color= "#984ea3", size = 1)+
  # geom_errorbar(data = global_mean,aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),
  #               width=0.01,size=1,color= "#984ea3")+
  
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(legend.position = c(0.36, 0.93), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA),
        axis.text = element_text(size= 12),
        # strip.text.x = element_text(size = 14),
        strip.text.x = element_blank(),
        panel.spacing = unit(2, "lines"),
        text = element_text(size= 16),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title.x=element_text(margin = margin(t = 12)),
        axis.title.y=element_text(margin = margin(r = 12)))+
  labs(x = "True outlier-cluster weight", y = "Mean Outlier-cluster weight")+
  guides(color = "none")
# guides(color = guide_legend(title = NULL))
# guides(linetype = "none")

# Weights based different reference framesra
# boundaries: outer, group means, inner
# Total 9 combinations
tb.errors_size = tb.errors_size %>% 
  mutate(group_1_outer = group_1_edge(proj.x_group_1_mean,proj.x_group_2_mean,proj.x_group_1)$group_1_outer,
         group_1_inner = group_1_edge(proj.x_group_1_mean,proj.x_group_2_mean,proj.x_group_1)$group_1_inner,
         group_2_outer = group_2_edge(proj.x_group_1_mean,proj.x_group_2_mean,proj.x_group_2)$group_2_outer,
         group_2_inner = group_2_edge(proj.x_group_1_mean,proj.x_group_2_mean,proj.x_group_2)$group_2_inner) %>% 
  mutate(
         # inner_outer_weight = reference_frame_weight(group_1_inner,group_2_outer,proj.x_response)$weight_group_1,
         # mean_outer_weight = reference_frame_weight(proj.x_group_1_mean,group_2_outer,proj.x_response)$weight_group_1,
         outer_outer_weight = reference_frame_weight(group_1_outer,group_2_outer,proj.x_response)$weight_group_1,
         
         # inner_mean_weight = reference_frame_weight(group_1_inner,proj.x_group_2_mean,proj.x_response)$weight_group_1,
         mean_mean_weight = reference_frame_weight(proj.x_group_1_mean,proj.x_group_2_mean,proj.x_response)$weight_group_1,
         # outer_mean_weight = reference_frame_weight(group_1_outer,proj.x_group_2_mean,proj.x_response)$weight_group_1,
         
         inner_inner_weight = reference_frame_weight(group_1_inner,group_2_inner,proj.x_response)$weight_group_1
         # mean_inner_weight = reference_frame_weight(proj.x_group_1_mean,group_2_inner,proj.x_response)$weight_group_1,
         # outer_inner_weight = reference_frame_weight(group_1_outer,group_2_inner,proj.x_response)$weight_group_1
         )
  

tb.reference_weight = tb.errors_size %>% select(subject,group_1_size,group_2_size,size_ratio,
                      # inner_outer_weight,
                      # mean_outer_weight,
                      outer_outer_weight,
                      # inner_mean_weight,
                      mean_mean_weight,
                      # outer_mean_weight,
                      inner_inner_weight,
                      # mean_inner_weight,
                      # outer_inner_weight,
                      true_group_1_weight
                      )
# weights based on average estimates by subjects
# format transform (gather)
# by subject (average performance)
# mean_reference_weight_subject = tb.reference_weight %>% gather(Reference, Weights, inner_outer_weight:outer_inner_weight) %>% 
mean_reference_weight_subject = tb.reference_weight %>% gather(Reference, Weights, outer_outer_weight:inner_inner_weight) %>% 
  group_by(subject,group_1_size,group_2_size, size_ratio, Reference) %>% 
  summarise(average_weight = mean(Weights)) %>% 
  mutate(true_group_weight = group_1_size/(group_1_size+group_2_size))

mean_reference_weight_subject %>% 
  filter(Reference == "mean_mean_weight") %>%
  ggplot(aes(x=as.factor(group_2_size),average_weight,group=interaction(subject,Reference)))+
  geom_point()+
  geom_line()+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=as.factor(group_2_size), y = true_group_weight,group=5),color="red",size=1.5)+
  facet_grid(.~group_1_size)

# mean for the average performance across subjects
# mean_reference_weight = tb.reference_weight %>% gather(Reference, Weights, inner_outer_weight:outer_inner_weight) %>% 
mean_reference_weight = tb.reference_weight %>% gather(Reference, Weights, outer_outer_weight:inner_inner_weight) %>% 
  group_by(subject,group_1_size,group_2_size, size_ratio, Reference) %>% 
  summarise(average_weight = mean(Weights)) %>% 
  group_by(group_1_size,group_2_size, size_ratio, Reference) %>% 
  summarise(n=n(),
            mean_weight = mean(average_weight),
            se_weight = sd(average_weight)/sqrt(n)) %>% 
  mutate(true_group_weight = group_1_size/(group_1_size+group_2_size))

mean_reference_weight %>% 
  ggplot(aes(x=as.factor(group_2_size),mean_weight,color=Reference,group=Reference))+
  geom_point()+
  geom_line()+
  geom_point(aes(x=as.factor(group_2_size), y = true_group_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=as.factor(group_2_size), y = true_group_weight,group=5),color="red",size=1.5)+
  facet_wrap(.~group_1_size)
  

#Subject mean position estimate aggregated across group_1_size
average_mean_subject_size_ratio = tb.errors_size %>% group_by(subject, size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            group_2_boundary_weight = mean(group_2_boundary_weight),
            true_group_1_weight=mean(true_group_1_weight))

average_mean_subject_size_ratio %>% 
  ggplot(aes(x=size_ratio, y = group_1_weight)) +
  geom_point()
# convex hull area (cardinality >= 4)
tb.errors_size=tb.errors_size %>% mutate(convex_hull_coord_group_1 = list(compute_ch(group_1_coord_recenter)),
                              convex_hull_coord_group_2 = list(compute_ch(group_2_coord_recenter)))

tb.errors_size = tb.errors_size %>% mutate(area_hull_group_1 = areapl(as.matrix(convex_hull_coord_group_1)),
                                           area_hull_group_2 = areapl(as.matrix(convex_hull_coord_group_2)),
                                           sqrt_area_hull_group_1 = sqrt(area_hull_group_1),
                                           sqrt_area_hull_group_2 = sqrt(area_hull_group_2)) 

tb.errors_size %>% filter(group_1_size == 4) %>% 
  mutate(group_1_area_weight = area_hull_group_1/(area_hull_group_1+area_hull_group_2),
         group_1_sqrt_area_weight = sqrt_area_hull_group_1/(sqrt_area_hull_group_1+sqrt_area_hull_group_2)) %>% 
  group_by(subject, group_2_size) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            area_1_weight = mean(group_1_area_weight),
            sqrt_area_1_weight = mean(group_1_sqrt_area_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            mean_area_1_weight = mean(area_1_weight),
            mean_sqrt_area_1_weight = mean(sqrt_area_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  ggplot(aes(x=group_2_size,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_size,y=mean_group_1_weight,group = 5))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.12,size=1.2)+
  
  geom_point(aes(x=group_2_size, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=group_2_size, y = true_group_1_weight,group = 5),color="red")+
  
  geom_point(aes(x=group_2_size, y = mean_area_1_weight),shape=17,size=3,color="blue")+
  geom_line(aes(x=group_2_size, y = mean_area_1_weight,group = 5),color="blue")+
  
  geom_point(aes(x=group_2_size, y = mean_sqrt_area_1_weight),shape=17,size=3,color="yellow")+
  geom_line(aes(x=group_2_size, y = mean_sqrt_area_1_weight,group = 5),color="yellow")+
  
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        text = element_text(size= 12),
        panel.grid = element_blank(),
        legend.position = 'none')
  

#Response time
tb.errors_size$response_time %>% mean()
tb.errors_size %>% group_by(size_ratio) %>% summarise(mean_response_time = mean(response_time))

# Statistical Analyses
# variables:
# 1. cardinality ratios (main group cardinality) (5 levels) 2. Outlier group cardinality (4 levels) 3. subjects 
# multiple one-sample t test
# Combined
# size_ratio = 1
library(lsr)
# global_mean_subject %>% filter(size_ratio == 1) %>% pull(mean_group_1_weight) %>% hist()
# global_mean_subject %>% filter(size_ratio == 1) %>% pull(mean_group_1_weight) %>% t.test(., mu = 0.5)
# cohensD(global_mean_subject %>% filter(size_ratio == 1) %>% pull(mean_group_1_weight),
#         mu = 0.5)
# 
# #size_ratio = 0.5
# global_mean_subject %>% filter(size_ratio == 0.5) %>% pull(mean_group_1_weight) %>% hist()
# global_mean_subject %>% filter(size_ratio == 0.5) %>% pull(mean_group_1_weight) %>% t.test(., mu = 1/3)
# cohensD(global_mean_subject %>% filter(size_ratio == 0.5) %>% pull(mean_group_1_weight),
#         mu = 1/3)
# 
# #size_ratio = 0.25
# global_mean_subject %>% filter(size_ratio == 0.25) %>% pull(mean_group_1_weight) %>% hist()
# 
# global_mean_subject %>% filter(size_ratio == 1/4) %>% pull(mean_group_1_weight) %>% t.test(., mu = 1/5)
# cohensD(global_mean_subject %>% filter(size_ratio == 1/4) %>% pull(mean_group_1_weight),
#         mu = 1/5)
# 
# #size_ratio = 0.125
# global_mean_subject %>% filter(size_ratio == 0.125) %>% pull(mean_group_1_weight) %>% hist()
# 
# global_mean_subject %>% filter(size_ratio == 0.125) %>% pull(mean_group_1_weight) %>% t.test(., mu = 1/9)
# cohensD(global_mean_subject %>% filter(size_ratio == 0.125) %>% pull(mean_group_1_weight),
#         mu = 1/9)
# 
# #size_ratio = 0.125
# global_mean_subject %>% filter(size_ratio == 0.0625) %>% pull(mean_group_1_weight) %>% hist()
# 
# global_mean_subject %>% filter(size_ratio ==0.0625) %>% pull(mean_group_1_weight) %>% t.test(., mu = 1/17)
# cohensD(global_mean_subject %>% filter(size_ratio == 0.0625) %>% pull(mean_group_1_weight),
#         mu = 1/17)

# Test for modality
library(fitdistrplus)
library(diptest)
data_1_8 = tb.errors_size %>% filter(size_ratio==0.125) %>% pull(group_1_weight) 
dip.test(data_1_8)
data_1_16 = tb.errors_size %>% filter(size_ratio==0.0625) %>% pull(group_1_weight) 
dip.test(data_1_16)


# Mixed ANOVA
# Within: Cardinality ratios (5), Outlier cluster cardinality (4)
# Between: Edge vs. no edge (2)
# dependent: outlier-cluster weight
combined_edge_corrected = edge_weight_with_correction_subject %>% rbind(edge_weight_without_correction_subject)
library(rstatix)
library(tidyverse)
combined_edge_corrected$subject = as.factor(combined_edge_corrected$subject)
combined_edge_corrected = combined_edge_corrected %>% ungroup()
res.aov = combined_edge_corrected %>% anova_test(dv = group_1_weight, wid = subject, between = reference_frame, within = c(group_1_size, size_ratio))

get_anova_table(res.aov, correction = "GG")

one.way <- combined_edge_corrected %>%
  group_by(size_ratio) %>%
  anova_test(dv = group_1_weight, wid = subject, between = reference_frame, within = c(group_1_size)) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
one.way

combined_edge_corrected %>% 
  # filter(size_ratio != "1") %>% 
  # group_by(reference_frame,size_ratio) %>%
  group_by(reference_frame) %>%
  pairwise_t_test(group_1_weight ~ group_1_size, p.adjust.method = "bonferroni") 
  # filter(reference_frame == "edge with edge") %>%
  # filter(reference_frame == "no edge with group means") %>%
  # print(n=30)



#overall comparison between two edge group
combined_edge_corrected[as.numeric(as.character(combined_edge_corrected$size_ratio))<1,]
t.test(data = combined_edge_corrected[as.numeric(as.character(combined_edge_corrected$size_ratio))<1,], group_1_weight~reference_frame)
cohensD(edge_weight_with_correction_subject %>% pull(group_1_weight),edge_weight_without_correction_subject %>% pull(group_1_weight))

t.test(data = combined_edge_corrected, group_1_weight~reference_frame)
cohensD(edge_weight_with_correction_subject %>% pull(group_1_weight),edge_weight_without_correction_subject %>% pull(group_1_weight))


#Group by edge bias
#Without edge bias

# edge_weight_without_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight) %>% hist()
# 1
edge_weight_without_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight) %>% t.test(., mu = 1/2)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight),
        mu = 1/2)
#0.5
# edge_weight_without_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight) %>% hist()

edge_weight_without_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight) %>% t.test(., mu = 1/3)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight),
        mu = 1/3)

#0.25
# edge_weight_without_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight) %>% hist()
edge_weight_without_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight) %>% t.test(., mu = 1/5)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight),
        mu = 1/5)

#0.125
# edge_weight_without_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight) %>% hist()

edge_weight_without_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight) %>% t.test(., mu = 1/9)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight),
        mu = 1/9)

#0.0625
edge_weight_without_correction_subject %>% filter(size_ratio == 0.0625) %>% pull(group_1_weight) %>% hist()

edge_weight_without_correction_subject %>% filter(size_ratio == 0.0625) %>% pull(group_1_weight) %>% t.test(., mu = 1/17)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 0.0625) %>% pull(group_1_weight),
        mu = 1/17)

# singleton
edge_weight_without_correction_subject %>% filter(size_ratio == 0.0625, group_1_size == 1) %>% pull(group_1_weight) %>% t.test(., mu = 1/17)
cohensD(edge_weight_without_correction_subject %>% filter(size_ratio == 0.0625, group_1_size == 1) %>% pull(group_1_weight),
        mu = 1/17)

#With edge bias (corrected)
# edge_weight_with_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight) %>% hist()

# 1 
edge_weight_with_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight) %>% t.test(., mu = 1/2)
cohensD(edge_weight_with_correction_subject %>% filter(size_ratio == 1) %>% pull(group_1_weight),
        mu = 1/2)

#0.5
# edge_weight_with_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight) %>% hist()

edge_weight_with_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight) %>% t.test(., mu = 1/3)
cohensD(edge_weight_with_correction_subject %>% filter(size_ratio == 0.5) %>% pull(group_1_weight),
        mu = 1/3)

#0.25
# edge_weight_with_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight) %>% hist()

edge_weight_with_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight) %>% t.test(., mu = 1/5)
cohensD(edge_weight_with_correction_subject %>% filter(size_ratio == 0.25) %>% pull(group_1_weight),
        mu = 1/5)

#0.125
# edge_weight_with_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight) %>% hist()
edge_weight_with_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight) %>% t.test(., mu = 1/9)
cohensD(edge_weight_with_correction_subject %>% filter(size_ratio == 0.125) %>% pull(group_1_weight),
        mu = 1/9)

#0.0625
# edge_weight_with_correction_subject %>% filter(size_ratio == 0.0625) %>% pull(group_1_weight) %>% hist()
edge_weight_with_correction_subject %>% filter(subject %in% edge_subject, size_ratio == 0.0625) %>% pull(group_1_weight) %>% t.test(., mu = 1/17)
cohensD(edge_weight_with_correction_subject %>% filter(size_ratio == 0.0625) %>% pull(group_1_weight),
        mu = 1/17)

# Mean_index analysis
tb.errors_size[c("mean_index","size_ratio")] = tb.errors_size[c("mean_index","size_ratio")] %>% lapply(factor)
tb.errors_size %>% 
  filter(size_ratio==0.25) %>%
  group_by(mean_index,size_ratio) %>% 
  summarise(mean_weight_1 = mean(group_1_weight)) %>% 
  ggplot(aes(x = size_ratio, y =mean_weight_1, fill = mean_index)) +
  geom_bar(stat = "identity", position = "dodge")
