# Load libraries
library(tidyverse)
library(Matrix)
library(reshape2)
library(rlang)

# Helper function that builds sparse covariance structure
# based on a dataframe and given grouping variable
# INPUT: dataframe and grouping variable (needs to exist as column name)
# OUTPUT: sample wide, group specific covariance structure

build_samp_mat_struct <- function(df, grp_var){
  
  # Create group specific ids based on grp var
  df <- df %>% 
    mutate(id = group_indices(., (!!as.name(grp_var))))
  
  # Count size of groups
  grp_nums <- df %>% 
    group_by(id) %>% 
    dplyr::summarize(n = n())
  
  grp_id <- unique(df$id)
  
  # Great ones square (nxn) matrices based on group size
  grp_mats <- sapply(grp_id, 
                 function(x){
                   n_samp <- grp_nums %>% 
                     filter(id == x) %>% 
                     select(n) %>% 
                     as.integer()
                   matrix(1, n_samp, n_samp)
                 })
  
  # build block diagonal sparse matrix using nxn square mats
  tmp_bdiag <- bdiag(grp_mats) 
  
  return(tmp_bdiag)
  
}



