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