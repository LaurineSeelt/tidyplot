hematocrit_paired <- read_csv2(here::here("Data/hematocrit_paired"))

hematocrit_tidy <-  pivot_longer(data = hematocrit_paired, cols = c(voor, na),
                                 names_to = "condition",  values_to = "hematocrit")
hematocrit_tidy_summary <- hematocrit_tidy %>% group_by(condition) %>%
  summarize(mean = mean(hematocrit), stdev = sd(hematocrit))

shapiro.test(voor en na) %>% leveneTest() %>% t.test()
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
