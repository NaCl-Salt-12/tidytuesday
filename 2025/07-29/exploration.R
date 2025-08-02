library(tidyverse)
library(mosaic)
library(ggrepel)
library(showtext)
library(sysfonts)
library(ggtext)

font_add("Bebas Neue", "BebasNeue.otf")

# Enable showtext for rendering
showtext_auto()



tuesdata <- tidytuesdayR::tt_load(2025, week = 30)

movies <- tuesdata$movies
shows <- tuesdata$shows

tidy_time <- function(time_str) {
  hours <- as.numeric(str_extract(time_str,"\\d+(?=H)"))
  minutes <- as.numeric(str_extract(time_str,"\\d+(?=M)"))
  # seconds <- as.numeric(str_extract(time_str,"\\d+(?=S)"))
  
  hours <- replace_na(hours, 0)
  minutes <- replace_na(minutes, 0)
  # seconds <- replace_na(seconds, 0)
  
 minutes + hours * 60 
}

movies_clean <- movies %>% 
  group_by(title,runtime) %>% 
  summarize(views = sum(views)) %>% 
  ungroup() %>% 
  mutate(runtime_exact = tidy_time(runtime)) %>% 
  filter( runtime_exact > 0)

longest_movies <- movies_clean %>% arrange(desc(runtime_exact)) %>% head(10)

view(longest_movies)

shortest_movies <- movies_clean %>% 
  arrange(runtime_exact) %>% 
  head(10)

view(shortest_movies)


most_popular <- movies_clean %>%  arrange(desc(views)) %>% head(10)

view(most_popular)


least_popular <- movies_clean %>%  arrange(views) %>% head(10)
view(least_popular)

movies_net <- movies_clean %>% 
    filter(runtime_exact <= quantile(runtime_exact,0.99)) %>% 
    ggplot(aes(x=runtime_exact))+
      geom_density(color="#B20710",linewidth=0.5, fill = '#B2071040')+
      labs(x="Runtime in Minutes", title="Movie Length on Netflix",
           caption = "__Source__: Netflix | __Visualization__: Nathaniel Clark")+
      scale_x_continuous(breaks = c(seq(0,180,30)))+
      theme_minimal(base_family = "Bebas Neue")+
      theme(
        plot.background = element_rect(fill="#101010"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color= "grey94",size = 16),
        axis.title.x = element_text(color= "grey94",size = 24, vjust = -1,family = 'sans', face = 'bold'),
        plot.title = element_text(color = "#E50914",family="Bebas Neue", size = 36,hjust = 0),
        axis.ticks.x = element_line(color = "grey94"),
        plot.margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt"),
        plot.caption = element_markdown(color = 'grey70',size = 11,family = 'sans', hjust = .98)
        # plot.title.position = "plot"
      )

movies_net
  
ggsave("movies_net.png",movies_net,width=4,height = 2.5)

popularity_by_length <- movies_clean %>% 
  # mutate(quantile = ntile(runtime_exact,12)) %>% 
  # group_by(quantile) %>% 
  slice_max(order_by = views,n=10) %>% 
  ungroup()



movies_clean %>% 
  # slice_max(order_by = views, n = 1000) %>% 
  filter(runtime_exact <= quantile(runtime_exact,0.99)) %>% 
ggplot(aes(x=runtime_exact,y=views))+
  geom_point(alpha=.5, color= "#E50914")+
  theme_void(base_family = "sans serif")+
  theme(
    plot.background = element_rect(fill="#101010")
  )
  # geom_text_repel(data=popularity_by_length,aes(x=runtime_exact,y=views,label = title),
                  color = "white")


shows_clean  <- shows %>% 
  group_by(title,runtime) %>% 
  summarize(views = sum(views)) %>% 
  ungroup() %>% 
  mutate(runtime_exact = tidy_time(runtime)) %>% 
  filter( runtime_exact > 0)

shows_clean %>% 
  filter(runtime_exact <= quantile(runtime_exact,0.98)) %>% 
ggplot(aes(x=runtime_exact))+
  geom_density()


popularity_by_length_shows <- shows_clean %>% 
  # mutate(quantile = ntile(runtime_exact,12)) %>% 
  # group_by(quantile) %>% 
  slice_max(order_by = views,n=10) %>% 
  ungroup()



shows_clean %>% 
  filter(runtime_exact <= quantile(runtime_exact,0.99)) %>% 
ggplot(aes(x=runtime_exact,y=views))+
  geom_point(alpha=.2)+
  geom_text(data=popularity_by_length_shows,aes(x=runtime_exact,y=views,label = title))+
  scale_x_continuous(labels = function(x) round(x / 60, 1))
