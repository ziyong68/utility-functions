library(tidyverse)

source("./grouped density and cum. density reporting/grouped ecdf function.R")

set.seed(70)

# Create Groups
df <- data.frame(group1 = sample(c("H","M","L"), 1000000, replace = TRUE), group2 = sample(LETTERS[1:3], 1000000, replace = TRUE))

# Simulate 3 different mean and 3 different sd (9 possible height distribution)
df2 <- df %>% 
  group_by(group1, group2) %>% 
  mutate(height = rnorm(n(), mean = sample(c(100, 150, 200), 1), sd = sample(c(10,20,30),1))) %>%
  ungroup()

# Check to make sure the sampling process work for each group
check <- df2 %>% group_by(group1,group2) %>% summarize(height_mean = mean(height),
                                              height_sd = sd(height))



# Test the functions
ecdf_summary_g1 <- group_ecdf(df2, height, group1)
ecdf_summary_g1_args <- group_ecdf(df2, height, group1, lower_limit = 0, upper_limit = 300, step_size = 1)
ecdf_summary_g1_g2 <- group_ecdf(df2, height, group1, group2, lower_limit = 0, upper_limit = 300, step_size = 10)

# Compare plots (first by stat_ecdf, second by geom_line on our summary table)
plot1 <- ggplot(df2, aes(x = height, col = group1)) + stat_ecdf()
plot2 <-ggplot(ecdf_summary_g1, aes(x = SCORE, y = CUMULATIVE_DENSITY, col = group1)) + geom_line()

plot3 <- ggplot(df2 %>% mutate(GROUP1_2 = factor(paste(group1,group2))), aes(x = height, col = GROUP1_2)) + 
  geom_density() +
  facet_wrap(~group1)
plot4 <- ggplot(ecdf_summary_g1_g2 %>% mutate(GROUP1_2 = as.factor(paste(group1,group2))), 
                aes(x = SCORE, y = DENSITY, col = GROUP1_2)) + 
  geom_line() + 
  facet_wrap(~group1)

View(check)
