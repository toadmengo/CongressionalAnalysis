hr90 <- read.csv("https://raw.githubusercontent.com/bryandmartin/STAT302/master/docs/Projects/project2_svd/house_90_raw.csv")
write.csv(hr90, file = "Data/hr90.csv")
senate90 <- read.csv("https://raw.githubusercontent.com/bryandmartin/STAT302/master/docs/Projects/project2_svd/senate_90_raw.csv")
write.csv(senate90, file = "Data/senate90.csv")
hr116 <- read.csv("https://raw.githubusercontent.com/bryandmartin/STAT302/master/docs/Projects/project2_svd/house_116_raw.csv")
write.csv(hr116, file = "Data/hr116.csv")
senate116 <- read.csv("https://raw.githubusercontent.com/bryandmartin/STAT302/master/docs/Projects/project2_svd/senate_116_raw.csv")
write.csv(senate116, file = "Data/senate116.csv")

options(ggrepel.max.overlaps = Inf)

# svd_cluster - function that plots a geom_point graph of congress data set 
# using SVD. Also returns two left singular vectors and energy
# params - congress: the data
# returns list - svd: the data from the two left singular vectors, 
# plot - the plot of the two left singular vectors, 
# energy: the energy of the plot
svd_cluster <- function(congress, states = NA) {
  # omits NAs
  df_omit_NA <- congress %>%
    na.omit() 
  # filters out label, performs SVD
  congress_svd <- df_omit_NA[,-c(1:5)] %>%
    svd()
  # save labels
  congress_labels <- df_omit_NA[,c(1:5)]
  # organize first two U matrix cols
  svd_u_df <- data.frame("x" = congress_svd$u[, 1],
                        "y" = congress_svd$u[, 2]) %>%
    cbind(congress_labels)
  # graph geom_point of 2 left singular vectors, color = party
  u_plot <- ggplot(svd_u_df, aes(x = x, y = y, color = party_code)) +
    geom_point() +
    scale_color_manual(name = "Party",
                       values = c("D" = "blue", "I" = "green", "R" = "red")) +
    labs(x = "Left Singular Vector 1", y = "Left Singular Vector 2") +
    theme_bw(base_size = 20)
  # calculate energy
  sing_vals_sq <- congress_svd$d ^ 2
  energy = (sum(sing_vals_sq[1:2]) / sum(sing_vals_sq))
  # return the svd data, the plot, and the energy as a list
  returnList <- list("svd" = svd_u_df, "plot" = u_plot,
       "energy" = energy)
  return(returnList)
}



states = c("CA", "TX")

hr90_svd <- svd_cluster(hr90, states)
hr90_svd$plot

hr90_partisan_df <- hr90_svd$svd
colnames(hr90_partisan_df)[colnames(hr90_partisan_df) == "y"] <- "partisanship"

filter(hr90_partisan_df, state_abbrev %in% states) %>%
  ggplot(mapping = aes(partisanship, group = state_abbrev)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(~ state_abbrev)

hr90_partisan_df$adjusted_partisanship <- with(
  hr90_partisan_df, partisanship - ave(partisanship, party_code, FUN = median))

filter(hr90_partisan_df, state_abbrev == "CA") %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(~ party_code)

filter(hr90_partisan_df, state_abbrev == "TX") %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(~ party_code)

hr116_svd <- svd_cluster(hr116, states)
hr116_svd$plot
hr116_partisan_df <- hr116_svd$svd
colnames(hr116_partisan_df)[colnames(hr116_partisan_df) == "x"] <- 
  "partisanship"

filter(hr116_partisan_df, state_abbrev %in% states) %>%
  ggplot(mapping = aes(partisanship, group = state_abbrev)) +
  geom_histogram(binwidth = .01) +
  facet_wrap(~ state_abbrev)

hr116_partisan_df$partisanship <- with(
  hr116_partisan_df, partisanship - ave(partisanship, party_code, FUN = median))

filter(hr116_partisan_df, state_abbrev == "CA") %>%
  ggplot(mapping = aes(partisanship, group = party_code)) +
  geom_histogram(binwidth = .0025) +
  facet_wrap(~ party_code)

filter(hr116_partisan_df, state_abbrev == "TX") %>%
  ggplot(mapping = aes(partisanship, group = party_code)) +
  geom_histogram(binwidth = .0025) +
  facet_wrap(~ party_code)




senate90_svd <- svd_cluster(senate90)
senate90_svd$plot
med <- median(senate90_svd$svd$u[,2])
senate90_partisan_df <- data.frame("partisanship" = abs(senate90_svd$svd$u[,2] - med)) %>%
  cbind(senate90_svd$labels)

ggplot(senate90_partisan_df, 
       mapping = aes(born, partisanship, group = party_code)) +
  geom_jitter() +
  geom_smooth(method = loess, se = F) +
  facet_wrap(~ party_code)

senate116_svd <- svd_cluster(senate116)
senate116_svd$plot
med <- median(senate116_svd$svd$u[,1])
senate116_partisan_df <- data.frame("partisanship" = abs(senate116_svd$svd$u[,1] - med)) %>%
  cbind(senate116_svd$labels)

ggplot(senate116_partisan_df, 
       mapping = aes(born, partisanship, group = party_code)) +
  geom_jitter() +
  geom_smooth(method = loess, se = F) +
  facet_wrap(~ party_code)


# hide labels for states not explicitly passed in as parameter
svd_u_df$state <- ifelse(df_omit_NA$state %in% st_abbrev, 
                         df_omit_NA$state, "")
#find which state are bipartisan?
svd_u_df$state <- ifelse(
  (df_omit_NA$party_code == "R" & svd_u_df$y >= 0) | 
    (df_omit_NA$party_code == "D" & svd_u_df$y <= -.05),
  df_omit_NA$state, "")

#save code 
svd_u_df$bioname <- ifelse(df_omit_NA$party_code == "D" & svd_u_df$x <= 0,
                           df_omit_NA$bioname, "")

# organize the first two v vectors
svd_v_df <- data.frame("x" = congress_svd$v[, 1],
                       "y" = congress_svd$v[, 2])
# graph geom_point of the right singular vectors
v <- ggplot(svd_v_df, aes(x = x, y = y, label = label)) +
  geom_point()





### Are Democrats and Republicans from these States Especially Partisan?

```{r}
# subtract the partisanship score with the party's median partisanship
hr90_partisan_df$adjusted_partisanship <- with(
  hr90_partisan_df, partisanship - ave(partisanship, party_code, FUN = median))

# Visualize the voting tendencies of democrats and republicans in CA
filter(hr90_partisan_df, state_abbrev == states[1]) %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(~ party_code)

# Visualize the voting tendencies of democrats and republicans in TX
filter(hr90_partisan_df, state_abbrev == states[2]) %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(~ party_code)
```

```{r}
# subtract the partisanship score with the party's median partisanship
hr116_partisan_df$adjusted_partisanship <- with(
  hr116_partisan_df, partisanship - ave(partisanship, party_code, FUN = median))
# visualize the voting tendencies of democrats and republicans in CA for hr116
filter(hr116_partisan_df, state_abbrev == states[1]) %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .0025) +
  facet_wrap(~ party_code)
# visualize the voting tendencies of democrats and republicans in TX for hr116
filter(hr116_partisan_df, state_abbrev == states[2]) %>%
  ggplot(mapping = aes(adjusted_partisanship, group = party_code)) +
  geom_histogram(binwidth = .0025) +
  facet_wrap(~ party_code)
```
