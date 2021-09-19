# Performs SVD on a congress data set, organizing it into a dataframe
# Params - congress: the dataset
#   states: optional, specifying whether the data should filter by state
# Returns list - left: the dataframe containing the left singular vectors
# right: data frame containing right vector
# energy: the energy captured by the first two vectors
organize_svd <- function(congress, states = NA) {
  if (!is.na(states)) {
    # filter by state if needed, then omit nas
    df_omit_NA <-filter(congress, state_abbrev %in% states) %>% na.omit()
  } else {
    # otherwise, just omit NAs
    df_omit_NA <- congress %>%
      na.omit() 
  }
  # filters out label, performs SVD
  congress_svd <- df_omit_NA[,-c(1:5)] %>%
    svd()
  # save labels
  congress_labels <- df_omit_NA[,c(1:5)]
  # organize first two U matrix cols
  svd_u_df <- data.frame("x" = congress_svd$u[, 1],
                         "y" = congress_svd$u[, 2]) %>%
    cbind(congress_labels)
  
  # organize the first two v vectors
  svd_v_df <- data.frame("v1" = congress_svd$v[, 1],
                         "v2" = congress_svd$v[, 2])
  # add a label for whether the role call received over 50% support
  svd_v_df$total = colSums(na.omit(congress)[,-c(1:5)])
  svd_v_df$passed = svd_v_df$total > 0
  
  # calculate energy
  sing_vals_sq <- congress_svd$d ^ 2
  energy = (sum(sing_vals_sq[1:2]) / sum(sing_vals_sq))  
  # return
  return(list("left" = svd_u_df, "right" = svd_v_df, "energy" = energy))
}

# plots the left two singular vectors using the df from organize_svd
plot_left <- function(svd_u_df, states = NA) {
  if (is.na(states)) {
    # graph geom_point of 2 left singular vectors, color = party
    u_plot <- ggplot(svd_u_df, aes(x = x, y = y, color = party_code))
  } else {
    # if state is specified we'll label the states
    u_plot <- ggplot(svd_u_df, aes(x = x, y = y, 
                                   color = party_code, shape = state_abbrev))
  }
  u_plot <- u_plot +  geom_point() +
    scale_color_manual(name = "Party",
                       values = c("D" = "dodgerblue1", "I" = "green", "R" = "firebrick2")) +
    labs(x = "Left Singular Vector 1", y = "Left Singular Vector 2") +
    theme_bw(base_size = 12)
}

# plots the right vectors using the df from organize_svd
plot_right <- function(svd_v_df) {
  v_plot <- ggplot(svd_v_df, aes(x = x, y = y, shape = passed)) +
    geom_point() +
    labs(x = "Right Vector 1", y = "Right Vector 2") +
    theme_bw(base_size = 12)
}






if (is.na(states)) {
  # graph geom_point of 2 left singular vectors, color = party
  u_plot <- ggplot(svd_u_df, aes(x = x, y = y, color = party_code))
} else {
  # if state is specified we'll label the states
  u_plot <- ggplot(svd_u_df, aes(x = x, y = y, 
                                 color = party_code, shape = state_abbrev))
}
u_plot <- u_plot +  geom_point() +
  scale_color_manual(name = "Party",
                     values = c("D" = "dodgerblue1", "I" = "green", "R" = "firebrick2")) +
  labs(x = "Left Singular Vector 1", y = "Left Singular Vector 2") +
  theme_bw(base_size = 12)
# calculate energy
sing_vals_sq <- congress_svd$d ^ 2
energy = (sum(sing_vals_sq[1:2]) / sum(sing_vals_sq))
# graph geom_point of the right singular vectors
v_plot <- ggplot(svd_v_df, aes(x = x, y = y, shape = passed)) +
  geom_point() +
  labs(x = "Right Vector 1", y = "Right Vector 2") +
  theme_bw(base_size = 12)
# return the svd data, the plot, and the energy as a list
returnList <- list("svd" = svd_u_df, "plot" = u_plot, "right_plot" = v_plot,
                   "energy" = energy)
return(returnList)