library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
# library(gridExtra)
library(grid)

# import dataset
# data is taken from https://kawalcovid19.blob.core.windows.net/viz/statistik_harian.html
covid_0411 <- read_csv("covid_0411.csv", na = c("", "NA", "NULL", "NaN", "null", "Null", "<NA>"))

# rename the column to make it readable with R
names(covid_0411) <- c("dt", "kum", "baru", "sembuh", "meninggal")

# gather the column baru, sembuh, and meninggal
# set column 'dt' format as date
covid_0411_gather <- covid_0411 %>% 
                      gather(key = "cases", value = "num_cases", -c(dt, kum)) %>% 
                      mutate(dt = as.Date(dt, "%Y-%m-%d"))

# covid_0411_gather2 <- covid_0411 %>% 
#   gather(key = "cases", value = "num_cases", -c(dt))

# create the plot
line_kum <- ggplot(covid_0411_gather) +
              geom_line(aes(x = dt, y = kum), colour="#000099") + 
              labs(title = "Covid-19 Cases in Indonesia as of April 10, 2020", x = "dates", y = "jumlah kumulatif kasus") +
              theme_minimal() + 
              theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# create the plot
bar_case <- ggplot(covid_0411_gather) +
              geom_bar(aes(x = dt, y = num_cases, fill = cases), stat = "identity", position = "dodge") +
              labs(x = "dates", y = "jumlah kasus harian") + 
              scale_x_date(date_breaks = "1 day") +
              theme_minimal() + 
              theme(axis.text.x = element_text(angle=90), legend.position = 'bottom')

# grid.newpage()
# grid.draw(rbind(ggplotGrob(line_kum), ggplotGrob(bar_case)))

# grid.arrange(line_kum, bar_case, ncol=1, heights = c(2, 1))

grid.newpage()
grid.draw(rbind(ggplotGrob(line_kum), ggplotGrob(bar_case), size = "last"))
  