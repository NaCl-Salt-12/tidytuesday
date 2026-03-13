library(tidyverse)
library(tidytuesdayR)
library(gghighlight)
library(purrr)
library(ggstatsplot)
library(patchwork)
library(ggpomological)
library(ggtext)
library(showtext)


tuesdata <- tidytuesdayR::tt_load(2026,week = 9 )

clutch_size <- tuesdata$clutch_size_cleaned

body_condition <- tuesdata$tortoise_body_condition_cleaned #|> 
  # filter(!locality == "Konjsko")
n_distinct(clutch_size$individual)
n_distinct(body_condition$individual)
glimpse(body_condition)

sex_ratio <- body_condition |> 
  group_by(year,sex,locality) |> 
  summarise(count = n(), .groups = "drop")  |> 
  pivot_wider(names_from = sex, values_from = count) |> 
  group_by(year,locality) |> 
  summarise(male_females_ratio= m/f, .groups = "drop")


historical_pop <- read_csv("./Historical_population_sizes.csv")  |>
  rename(population = Population, population_size = Population_Size, year = Year, sex = Sex) |> 
  mutate(population = case_when(population == "Konjsko" ~ "Konjsko", population == "Beach" ~ "Golem Grad", population == "Plateau" ~ "Golem Grad")) |>
  group_by(year, population,sex) |> 
  summarise(population_size = sum(population_size), .groups = "drop") |>
  unite(
    col = "sex_region",
    sex,
    population,
    sep = "_",
    remove = FALSE
  )

projected_pop <- read_csv("Projected_median_population_sizes.csv") |>
  rename(population = Population, population_size = Population_Size, year = Year, sex = Sex) |> 
  mutate(population = case_when(population == "Konjsko" ~ "Konjsko", population == "Beach" ~ "Golem Grad", population == "Plateau" ~ "Golem Grad")) |>
  group_by(year, population,sex) |> 
  summarise(population_size = sum(population_size), .groups = "drop") |>
  unite(
    col = "sex_region",
    sex,
    population,
    sep = "_",
    remove = FALSE
  )

combinded_pop <- bind_rows(historical_pop, projected_pop) |> 
  filter(year <= 2100) |> 
  filter(population_size <= 1500) |> 
  mutate(population = factor(population, levels = c("Konjsko", "Golem Grad")))






font_add_google("Homemade Apple", "apple")

showtext_auto()



body_con <- body_condition |> 
  mutate(locality = case_when(locality == "Konjsko" ~ "Konjsko", locality  == "Beach" ~ "Golem Grad", locality == "Plateau" ~ "Golem Grad")) |>
  mutate(locality = factor(locality, levels = c("Konjsko", "Golem Grad")),
          sex = factor(case_when(sex == "m" ~ "Male", sex == "f" ~ "Female", TRUE ~ "Unknown"), levels = c("Male","Female"))) |> 
  unite(
    col = "sex_region",
    sex,
    locality,
    sep = "_",
    remove = FALSE
  )

box_color <- "gray30"

text_facets <- data.frame(
  population = c("Konjsko", "Golem Grad"), 
  year = 2080, 
  population_size = 300, 
  label = "Last Female\ndies",
  label_color = c("grey85", "grey20") 
)

text_facets <- data.frame(
  population = c("Konjsko", "Golem Grad"), 
  year = 2080, 
  population_size = 300, 
  label = "Last Female\ndies",
  label_color = c("grey85", "grey20"),
  x_seg = 2083,
  xend_seg = 2083,
  y_seg = 350,
  yend_seg = 15,
  seg_color = c("grey85","#c03728")
)

text_facets$population <- factor(
  text_facets$population, 
  levels = levels(combinded_pop$population)
)

p1<- ggplot(body_con, aes(x=sex,y=body_condition_index)) +
  geom_jitter(size = 2.5,position = position_jitter(width = 0.2), alpha = 0.18, aes(color = sex_region),) +
  geom_boxplot(outliers = FALSE,fill = NA,width = 0.3,color = box_color) +
  geom_violin(fill = NA,width = 0.5, color = box_color) +
  guides(color = "none") +
  scale_color_pomological()+
  facet_wrap(~locality) +
  labs(x = NULL, y = "Body Condition Index") +
  theme_pomological_fancy("apple")+
  theme(
    strip.text = element_blank(),
  )

p1



p2 <- ggplot(combinded_pop, aes(x = year, y = population_size)) +
  geom_line(aes(color = sex_region)) +
  # scale_linetype_manual(values = c("dashed", "solid")) +
  labs(y = "Population Size") +
  scale_color_pomological() +
  gghighlight(
    use_direct_label = TRUE, 
    label_key = sex,
    unhighlighted_params = list(color = alpha("gray85", 1)),
    label_params = list(
      family = "apple", 
      size = 4,                   
      label.size = NA,            
      fill = alpha("white", 0),
      hjust = -0.1,                # Nudges labels slightly to the right of the line end
      nudge_x = 0.5,              # Nudges labels slightly up to avoid overlap
      segment.color = NA            # Removes the line segment connecting label to line end
    )
  ) +
  annotate(
    "segment", 
    x = 2083, xend = 2083, yend = 15, y = 350, 
    alpha = 0.5, 
    color = "#c03728", 
    # linetype = "dashed",
    arrow = arrow(length = unit(0.3, "cm"), type = "open")
  )+
  # annotate(
  #   "text",
  #   x = 2085, y = 550,
  #   label = "Projected\nExtinction",
  #   hjust = 0,
  #   size = 3.5,
  #   family = "apple",
  #   color = "grey20"
  # )+
  geom_segment(
    data = subset(text_facets, seg_color == "#c03728"),
    aes(x = x_seg, xend = xend_seg, y = y_seg, yend = yend_seg,),
    arrow = arrow(length = unit(0.25, "cm"), type = "open"),
    alpha = 0.5,
    color = "#c03728",
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = subset(text_facets, seg_color == "grey85"),
    aes(x = x_seg, xend = xend_seg, y = y_seg, yend = yend_seg,),
    arrow = arrow(length = unit(0.3, "cm"), type = "open"),
    color = "grey85",
    inherit.aes = FALSE
  ) +

  geom_text(
    data = subset(text_facets,label_color == "grey20"),
    aes(
      x = year,
      y = population_size,
      label = label,
    ),
    hjust = 1,
    color = "grey20",
    size = 4.0,
    family = "apple",
    inherit.aes = FALSE # Essential to avoid color/sex_region errors
  ) +
  geom_text(
    data = subset(text_facets,label_color == "grey85"),
    aes(
      x = year,
      y = population_size,
      label = label,
    ),
    hjust = 1,
    color = "grey85",
    size = 4.0,
    family = "apple",
    inherit.aes = FALSE # Essential to avoid color/sex_region errors
  ) +
  ylim(0, 1500) +
  facet_wrap(~population) +
  labs(y = "Population Size") +
  theme_pomological(
    base_family = "apple",
    base_size = 19,
    )+
  theme(
    axis.title.x = element_blank(),
    strip.text = element_text(margin = margin(b = 8)) # Increase 'b' (bottom) as needed
  )



p2


f1 <- p2 / p1
f1
  # theme(plot.background = element_rect(fill = "antiquewhite", color = NA))
#
# paint_pomological(f1, res = 110)
#
#
#
#
# Lesson learned simplify data ask why is this being shown what story am I trying to tell and keep the plot simple 
# I was initially seperationg Plateau and Bearch but they are both part of Golem Grad 
# so what I should have done which I started is combined the two. to directly compare golem grad and konjsko.
# the comparison between plateau and beach was not releveant to the story I wanted to tell though important to the paper
# the faceting with the highlighted lines whas more confusing than helpful
# so what I needed to do is remove the faceting and condense to two graphs and maybe added additional ones to tell the story better
# so think about the story and then identify the key/simplest way to tell that story.
#
