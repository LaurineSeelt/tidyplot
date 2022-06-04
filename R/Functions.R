library(devtools)
library(tidyverse)
library(car)
library(dbplyr)

hematocrit_paired <- read_csv2(here::here("Data/hematocrit_paired"))

tidydataframe <- function (data, x, y, a, b) {
  pivot_longer(data = data, cols = c(x, y),
              names_to = a, values_to = b)
}

# tidy <-  tidydataframe(data, x, y, a = "", b = "")

summarydataframe <- function(tidy, x, y) {
  tidy %>% group_by(x) %>%
  summarize(mean = mean(y), stdev = sd(y))
}

stats <-  function (data, tidy, x, y, a, b) {
  data %>% shapiro.test(data$x)
  data %>% shapiro.test(data$y)
  leveneTest(tidy)
  t.test(tidy$b, as.factor(tidy$a), center = mean)
}

plotting <- function (tidy, summary, title, subtitle) {
  summary %>% ggplot(aes(x = a, y = mean, fill = a))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = 0.2)+
    theme_classic()+
    labs(title = title,
         subtile = subtitle,
         x = tidy$a,
         y = tidy$b)+
    theme(legend.position = "none", text = element_text(size = 14))
}

hematocrit_tidy <-  pivot_longer(data = hematocrit_paired, cols = c(voor, na),
                                 names_to = "condition",  values_to = "hematocrit")
hematocrit_tidy_summary <- hematocrit_tidy %>% group_by(condition) %>%
  summarize(mean = mean(hematocrit), stdev = sd(hematocrit))

shapiro.test(voor & na) %>% leveneTest() %>% t.test()
hematocrit_tidy %>% leveneTest()

hematocrit_tidy_summary %>% ggplot(aes(x = condition, y = mean, fill = condition))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = 0.2)+
  theme_minimal()+
  labs(title = "Hematocrit values before and after an altitude workout",
       subtitle = "errorbars depict 1 standard devation",
       x = "Condition",
       y = "Hematocrit (%)")+
  theme(legend.position = "none", text = element_text(size=14))
