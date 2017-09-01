library(rlist)
#library(purrr)
#' Variables as list
anc_id <- list()
filtered_shape_id <- list()
selected_shape_id <- list()
sorted_filtered_shape <- list()
select_sorted_filtered_shape <- list()
merged_filtered_shape <- list()
x <- list()
x <- list()
z <- list()

#' Steps to editing and merging spatial list in R
#' Step. 1: Get the Ids of both saptial lists features and the attribute data
#' Step. 2: match the ids and create an order match(x,y)
#' Step. 3: rearrange the spatial list according to the attribute data id
#' step. 4: Append the attribute data at each matching spatial id
#'
#' Lets now begin the code, HaH!!


  filtered_shape_id <- function(){
    for(i in 1:length(filtered_shape)){
      # get filtered shape id
      filtered_shape_id <- list.append(filtered_shape_id,filtered_shape[[i]][[2]])
    } 
    filtered_shape_id
  }
  
  anc_id <- function(){
    for(i in 1:length(map_ANC_list[[1]])){
      # get anc id
      anc_id <- list.append(anc_id,map_ANC_list[[1]][[i]])
    }
    anc_id
  }
  
  sorted_filtered_shape <- function(){
    for(i in 1:length(filtered_shape)){
      # select filtered shape id
      selected_shape_id <- list.append(selected_shape_id,filtered_shape[[i]])
      # sort
      sorted_filtered_shape <- selected_shape_id[order(match(filtered_shape_id(),anc_id()))]
    }
    sorted_filtered_shape
  }
  sorted <- list()
  select_sorted_filtered_shape <- function(){
    for(i in 1:length(filtered_shape)){
      # for each sorted_filtered_shape, append anc coverage,
      select_sorted_filtered_shape <- list.append(select_sorted_filtered_shape, sorted_filtered_shape()[[i]][[4]][6][[1]])
      #sorted <- list.append(sorted_filtered_shape()[[i]][[4]][6],Average_ANC=map_ANC_list[[2]][[i]])
      # Append ANC data
      select_sorted_filtered_shape <- list.append(select_sorted_filtered_shape, Average_ANC=map_ANC_list[[2]][[i]])
    }
    select_sorted_filtered_shape
    #sorted
  }
  
  merged_spatial_list <- function(){
    for (i in length(filtered_shape)){
      sorted_filtered_shape()[[i]][[4]][6][[1]] <- select_sorted_filtered_shape()[[i]]
    }
    sorted_filtered_shape()
  }
  
  
  #return(str(select_sorted_filtered_shape))
  #x=list(1,2,3,4,5)
  #map(function(x,y,z){x[z]<-y[z];x},x,y,z)
  
 
 
#map(function(x,y,z){
#    x <- sorted_filtered_shape()
#    y <- select_sorted_filtered_shape()
#    z <- list(1,2,3,4,5)
#    x[z]<-y[z]; 
#    x
#  },x,y,z)


