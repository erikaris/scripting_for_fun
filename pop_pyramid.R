# data is in thousand persons
# source: bps.go.id

library(readr)
library(ggplot2)
library(ggthemes)

# step 1: load the dataset
pop_as <- read_csv('https://raw.githubusercontent.com/erikaris/scripting_for_fun/master/population_age_sex_2019.csv')

# step 2: take a brief look at the dataset. 
head(pop_as)

# step 3: convert the value of male population to negative numbers. 
pop_as <- pop_as %>% 
                mutate(pops_thousand2 = ifelse(gender == 'Laki-Laki', pops_thousand*(-1), pops_thousand))

# axis breaks and labels
brks <- seq(-10000, 10000, 2500)
lbls <-  paste0(as.character(c(seq(10, 0, -2.5), seq(2.5, 10, 2.5))))
                
# step 3: create a bar chart
# step 4: flip the coordinate
# step 5: order the x axis (age_group)
# step 6: adust the graph: add title, subtitle, modify axes, modify legend
# step 7: beautify the graph: use theme. 
ggplot(pop_as, aes(x = factor(age_group, levels = unique(age_group)), y = pops_thousand2, fill = gender)) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  labs(title="Population Pyramid of Indonesia (2019)", x = "age group", y = "population (in thousands)", subtitle = "Source: BPS, Statistik Indonesia 2020") + 
  scale_fill_discrete(name = "Gender", labels = c("Laki-laki (Male)", "Perempuan (Female)")) +
  #scale_x_discrete(limits = reorder(pop_as$age_group)) +
  scale_y_continuous(breaks = brks, labels = lbls) + 
  theme(plot.title = element_text(hjust = .5), axis.ticks = element_blank()) +
  #theme_tufte()
  theme_igray()
                
