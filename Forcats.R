library(forcats)

library(dplyr)
data <- gss_cat

data %>% count(race)

ggplot(gss_cat, aes(race)) + geom_bar() + scale_x_discrete(drop = FALSE)

data %>% count(relig)

ggplot(data, aes(relig)) + geom_bar() + scale_x_discrete(drop = FALSE)

# reorder factor for relig basis TV viwership:

relig <- data %>% 
  group_by(relig) %>%
  summarise(age = mean(age, na.rm = TRUE),
            tvhours = mean(tvhours, na.rm = TRUE))

ggplot(relig, aes(tvhours, relig)) + geom_point()

# now you can see this data is not correctly displayed on graph hence need to reshuffle factor order properly using fct_reorder


ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()

# now in above it shows perfect view of graph:

# fct_relevels
level1 <- levels(data$race)
level1

level2 <- fct_relevel(data$race, "Black", after = 3)
levels(level2)


