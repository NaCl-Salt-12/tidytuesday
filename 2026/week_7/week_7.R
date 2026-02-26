library(tidyverse)
library(tidytuesdayR)
library(mosaic)

tt_data <- tidytuesdayR::tt_load(2026,week = 7 )

?tt_data
df <- tt_data$dataset
glimpse(df) 

unique(df$measure)
df %>% count(measure,sort = TRUE) %>% print(n = 30)

unique(df$value_unit)
exp_df2 <- df  %>%  filter(value_label == "Hectares")

exp_df <- df  %>%  filter(measure == "Kiwifruit")
glimpse(exp_df)
glimpse(exp_df2)


unique(exp_df2$measure)

ggplot(exp_df,aes(x = year_ended_june,y = value)) +
  geom_line() +
  geom_point()

favstats(value~measure, data = df_filtered)

df_top <- df %>% 
  count(measure,sort = TRUE) %>% 
  slice_head(n = 20) %>%
  pull(measure)

df_filtered <- df %>% 
  filter(measure %in% df_top)

glimpse(df_filtered)

esquisse::esquisser()
