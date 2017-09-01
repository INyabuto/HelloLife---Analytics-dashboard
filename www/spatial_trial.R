
my_shape_name <- list()
my_anc_name <- list()
my_filtered_shape <- list()
my_sorted_shape <- list()
my_merged_shape <- list()

my_test<- function(){
  for(i in 1:length(filtered_shape)){
    my_shape_name <- list.append(my_shape_name,filtered_shape[[i]][[4]][[2]])
  }
  for(j in 1:length(map_ANC_list[[1]])){
    my_anc_name <- list.append(my_anc_name,map_ANC_list[[3]][[1]][[j]])
  }
  for(i in 1:length(filtered_shape)){
   my_filtered_shape <- list.append(my_filtered_shape, filtered_shape[[i]][[4]])
   # Re arrange the order to match anc
   my_sorted_shape <- my_filtered_shape[order(match(my_shape_name,my_anc_name))]
   
  }
 
  for(i in 1:length(filtered_shape)){
    my_merged_shape <- list.append(my_merged_shape,my_sorted_shape[[i]])
    # append ANC attribute data to the sorted shape file
    my_merged_shape <- list.append(my_merged_shape, Average_ANC=map_ANC_list[[2]][[i]])
  }
  str(my_sorted_shape)
}





