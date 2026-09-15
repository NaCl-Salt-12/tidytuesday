library(tidyverse)
library(priceR)
library(paletteer)
library(tidytext)


# game_films <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')
# local for offline work
game_films <- read_csv("/home/nathaniel/Downloads/game_films.csv")


df <- game_films |> 
  filter(budget_currency != "£") |> 
  drop_na(worldwide_box_office)|>
  group_by(original_game_publisher) |>
  mutate(pub_count = n()) |>
  ungroup() |>
  mutate(
    release_year = year(release_date),
    avg_budget = rowMeans(pick(budget_high,budget_low)),
    world_box_office_millions = worldwide_box_office / 1000000,
    metacritic_favored = ifelse(metacritic >= rotten_tomatoes, "Metacritic favored", "Rotten Tomatoes favored" ),
    profit = worldwide_box_office - avg_budget,
    # publisher = replace_when(
    #   pub_count == 1 ~ "Other"),
    publisher = case_when(
        pub_count == 1 ~ "Other",
        TRUE ~ original_game_publisher
        ),
    release_decade = as.factor((release_year %/% 10) * 10),
    original_pub = original_game_publisher,
    
  )|>
  mutate(genre = case_when(
    str_detect(title, "Mario|Sonic") ~ "Platformer",
    str_detect(title, "Mortal Kombat|Street Fighter|Tekken|DOA") ~ "Fighting",
    str_detect(title, "Resident Evil|Silent Hill|Five Nights|Until Dawn|Alone in the Dark") ~ "Horror/Survival",
    str_detect(title, "Tomb Raider|Uncharted|Assassin's Creed|Prince of Persia") ~ "Action/Adventure",
    str_detect(title, "Doom|Far Cry|Max Payne|Hitman|Borderlands") ~ "Shooter/Action",
    str_detect(title, "Warcraft|Pokémon|Monster Hunter|Angry Birds|Minecraft") ~ "Strategy/RPG/Other",
    TRUE ~ "Other"
  ))


common_words_by_pub <- df %>%
  filter(publisher != "Other")|>
  unnest_tokens(word, title) %>%
  group_by(publisher, word) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  select(publisher, word)

common_words_by_pub |> print(n=33)

df_series <- df %>%
  mutate(temp_id = row_number()) %>%
  unnest_tokens(word, title) %>%
  # Join with the common words list to check matches specific to that publisher
  inner_join(common_words_by_pub, by = c("publisher", "word")) %>%
  group_by(temp_id) %>%
  filter(n_distinct(word) >= 2 | word == "pokémon") %>%
  ungroup() %>%
  distinct(temp_id) %>%
  inner_join(df %>% mutate(temp_id = row_number()), by = "temp_id")|>
  select(-temp_id)|>
  mutate(
    pub2 = case_when(
      str_detect(title, "Evil") ~ "Capcom1",
      TRUE ~ publisher
      )
  )

df_series$title

df$title

unique(game_films$budget_currency)

unique(df$release_year)

df$genre

df|>filter(is.na(world_box_office_millions))|>select(title)

game_films |> count(budget_currency)

df |> select(title,worldwide_box_office) |> arrange(desc(worldwide_box_office)) |> head(20)

df |> select(title,profit)|> arrange(desc(profit)) |> head(20)

n_distinct(df$original_game_publisher)

unique(df$original_game_publisher) |> print()

df|> count(original_game_publisher)|>arrange(desc(n)) |> head(30)|>print(n=30)


ggplot(df, aes(x = metacritic, y= rotten_tomatoes, color = metacritic_favored, size = world_box_office_millions))+
    geom_point()+
    geom_abline(slope = 1, intercept = 0, color = "blue", linetype = 2)+
    scale_x_continuous(breaks = seq(10,100,by = 20), labels = seq(10,100,by=20), limits = c(0,100))+
    scale_y_continuous(breaks = seq(10,100,by = 20), limits = c(0,100))+
    labs(
    y = "Rotten Tomatoes Score",
    x = "Metacritic Rating",
    size = "Box office (Millions)",
  )+
  guides(
    color = "none"
  )+
  theme_minimal()



# --------


not_series <- df|>
  anti_join(df_series, by = "title")


ggplot(df, aes(x = release_year, y = world_box_office_millions, color = publisher))+
  geom_point()+
  geom_point(data = not_series, show.legend = F)+
  geom_line(data = df_series, aes(groups = pub2, color = publisher),show.legend = F, alpha = 0.7, linetype  = 2)+
  geom_label(data = df_series,aes(label = title), show.legend = FALSE)


ggplot(df, aes(x = world_box_office_millions, y = rotten_tomatoes, color = genre))+
  # geom_point(size = 4)+
  geom_label(aes(label = title),)+
  scale_colour_paletteer_d("tvthemes::Alexandrite")

  geom_line(data = df_pub, aes(groups = publisher),)

df$world_box_office_millions


# -------------------------------------------------------------------


df_genre <- df |>
  group_by(genre) |>
  summarise(
    avg_box_office_millions = mean(world_box_office_millions, na_rm = TRUE),
    total_box_office_millions = sum(world_box_office_millions, na_rm = TRUE),
    number_of_movies = n(),
    median_box = median(world_box_office_millions)
  )
df_pub <- df |>
  filter(pub_count > 1 | world_box_office_millions > mean(df$world_box_office_millions)) |>
  group_by(original_pub) |>
  summarise(
    avg_box_office_millions = mean(world_box_office_millions, na_rm = TRUE),
    total_box_office_millions = sum(world_box_office_millions, na_rm = TRUE),
    number_of_movies = n(),
    total_box_office = sum(worldwide_box_office),
    median_box = median(world_box_office_millions),
  )|>
  mutate(
    pub_count = str_glue("{original_pub}","\n(n = {number_of_movies})"),
    avg_box_millions_f = scales::dollar(avg_box_office_millions),
  )

df_pub$total_box_office_millions

glimpse(df_pub)

glimpse(df_genre)

ggplot(df, aes(y = genre, x = world_box_office_millions))+
  geom_point()+
  geom_boxplot(fill = NA)

ggplot(df_genre, aes(y = genre, x = avg_box_office_millions, fill = genre))+
  geom_col()

ggplot(df_genre, aes(y = genre, x = total_box_office_millions, fill = genre))+
  geom_col()


ggplot(df_pub, aes(y = pub_count, x = total_box_office_millions))+
  geom_col(aes(fill = original_pub), show.legend=F)
  geom_text(aes(label = box_millions_f))

ggplot(df_pub, aes(x = pub_count, y = avg_box_millions_f))+
  geom_col(aes(fill = original_pub), show.legend=F)+
  coord_flip()
  # geom_text(aes(label = box_millions_f))



ggplot(df_genre, aes(y = genre, x = number_of_movies, fill = genre))+
  geom_col()

n_distinct(df$genre)

ggplot(df_genre, aes(y = genre, x = median_box, fill = genre))+
  geom_col()

geom
