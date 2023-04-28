coord_fcn <- function(coordinates,width_height){
  x = c()
  y = c()
  if(sum(coordinates[[1]]>0)<2){
    x = c(x,coordinates$x/width_height)
    y = c(y,coordinates$y/width_height)
  }else{
    for(i in 1:length(coordinates)){
      x = c(x,coordinates[[i]]$x/width_height)
      y = c(y,coordinates[[i]]$y/width_height)
    }
  }
  xy = data.frame(x=x,y=y)
  return(xy)
}

coord_all_fcn <- function(coordinates,width_height){
  x = c()
  y = c()
  for(i in 1:length(coordinates)){
    x = c(x,coordinates[[i]]$x/width_height)
    y = c(y,coordinates[[i]]$y/width_height)
  }
  xy = data.frame(x=x,y=y)
  return(xy)
}

#Absolute distance error 
abs_err_dist <- function(response,target_mean){
  return(sqrt((response[1]-target_mean[1])^2 + (response[2]-target_mean[2])^2))
}

recenter <- function(coord, center){
  coord$x = coord$x - center[1]
  coord$y = coord$y - center[2]
  return(coord)
}

compute_weight <- function(mean_group_1, mean_group_2, response){
  if(mean_group_1 > mean_group_2){
    weight_group_1 = (response - mean_group_2)/(mean_group_1-mean_group_2)
    weight_group_2 = 1-weight_group_1
  } else {
    weight_group_1 = (mean_group_2 - response)/(mean_group_2-mean_group_1)
    weight_group_2 = 1-weight_group_1
  }
  return(weight = list(weight_group_1 = weight_group_1,
                       weight_group_2 = weight_group_2))
}

#convext hull 

# X <- matrix(rnorm(2000), ncol = 2)
# plot(X, cex = 0.5)
# hpts <- chull(X)
# hpts_line <- c(hpts, hpts[1])
# lines(X[hpts_line, ])
# mean = X[hpts, ] %>% colMeans()
# points(x=mean[1],y=mean[2],col="red")
# mean_2 = colMeans(X)
# points(x=mean_2[1], y = mean_2[2],col = "blue")

compute_ch <- function(coord){
  return(coord[chull(coord),])
}

# box.coords <- matrix(c(1, 1,
#                        4, 1,
#                        1, 3,
#                        4, 4), nrow = 4, ncol = 2, byrow = T)
# plot(box.coords[,1], box.coords[,2], pch = 20)
# 
# box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
# box.hpts <- c(box.hpts, box.hpts[1])
# box.chull.coords <- box.coords[box.hpts,]
# 
# chull.poly <- Polygon(box.chull.coords, hole=F)
# chull.area <- chull.poly@area

compute_ch_area <- function(coord){
  return(polygon(coord,hole=F)@area)
}

compute_inner_boundary <- function(proj_coord_1,proj_coord_2,mean_group_1,mean_group_2){
  if(mean_group_1< mean_group_2){
    proj_coord_lower = max(proj_coord_1)
    proj_coord_upper = min(proj_coord_2)
  } else{
    proj_coord_lower = max(proj_coord_2)
    proj_coord_upper = min(proj_coord_1)
  }
  return(inner_boundary = list(lower_inner_bound = proj_coord_lower,
                               upper_inner_bound = proj_coord_upper))
}

group_1_edge <- function(mean_group_1,mean_group_2,proj_coord_1) {
  if(mean_group_1 > mean_group_2) {
    group_1_inner = min(proj_coord_1)
    group_1_outer = max(proj_coord_1)
  } else{
    group_1_inner = max(proj_coord_1)
    group_1_outer = min(proj_coord_1)
  }
  return(group_1_edge = list(group_1_inner=group_1_inner,
                             group_1_outer=group_1_outer))
}

group_2_edge <- function(mean_group_1,mean_group_2,proj_coord_2) {
  if(mean_group_1 < mean_group_2) {
    group_2_inner = min(proj_coord_2)
    group_2_outer = max(proj_coord_2)
  } else{
    group_2_inner = max(proj_coord_2)
    group_2_outer = min(proj_coord_2)
  }
  return(group_2_edge = list(group_2_inner=group_2_inner,
                             group_2_outer=group_2_outer))
}

reference_frame_weight <- function(group_1_reference,group_2_reference,response){
  if(group_1_reference > group_2_reference){
    weight_group_1 = (response - group_2_reference)/(group_1_reference-group_2_reference)
    weight_group_2 = 1-weight_group_1
  } else {
    weight_group_1 = (group_2_reference - response)/(group_2_reference-group_1_reference)
    weight_group_2 = 1-weight_group_1
  }
  return(weight = list(weight_group_1 = weight_group_1,
                       weight_group_2 = weight_group_2))
}

group_1_edge_weight <- function(mean_group_1, mean_group_2, proj_coord_1){
  if(mean_group_1 < mean_group_2){
    group_1_edge = max(proj_coord_1)
    weight_group_1_edge = (mean_group_2 - group_1_edge)/(mean_group_2 - mean_group_1)
  } else{
    group_1_edge = min(proj_coord_1)
    weight_group_1_edge = (group_1_edge - mean_group_2)/(mean_group_1 - mean_group_2)
  }
  return(weight_group_1_edge) 
}

group_2_edge_weight <- function(mean_group_1, mean_group_2, proj_coord_2){
  if(mean_group_1 < mean_group_2){
    group_2_edge = min(proj_coord_2)
    weight_group_2_edge = (mean_group_2 - group_2_edge)/(mean_group_2 - mean_group_1)
  } else{
    group_2_edge = max(proj_coord_2)
    weight_group_2_edge = (group_2_edge - mean_group_2)/(mean_group_1 - mean_group_2)
  }
  return(weight_group_2_edge) 
}

compute_edge_weight <- function(mean_group_1, mean_group_2, proj_coord_1, proj_coord_2, response){
  proj_coord_lower = compute_inner_boundary(proj_coord_1, proj_coord_2, mean_group_1, mean_group_2)$lower_inner_bound
  proj_coord_upper = compute_inner_boundary(proj_coord_1, proj_coord_2, mean_group_1, mean_group_2)$upper_inner_bound
  if(mean_group_1 > mean_group_2){
    weight_group_1 = (response - proj_coord_lower)/(proj_coord_upper-proj_coord_lower)
    weight_group_2 = 1-weight_group_1
  } else {
    weight_group_1 = (proj_coord_upper - response)/(proj_coord_upper-proj_coord_lower)
    weight_group_2 = 1-weight_group_1
  }
  return(weight = list(weight_group_1 = weight_group_1,
                       weight_group_2 = weight_group_2))
}




