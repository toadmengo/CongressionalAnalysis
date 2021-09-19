
two_state <- filter(hr116, state_abbrev %in% states)
two_state_svd <- na.omit(two_state)[,-c(1:5)] %>% svd()
data.frame("x" = two_state_svd$u[,1],
           "y" = two_state_svd$u[,2], 
           "state" = na.omit(two_state)$state_abbrev,
           "party" = na.omit(two_state)$party_code) %>%
  ggplot(mapping = aes(x, y, color = party, shape = state, label = state)) +
  geom_point() +
  geom_text_repel(size = 3)

