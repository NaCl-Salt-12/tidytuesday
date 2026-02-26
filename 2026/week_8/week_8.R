library(tidyverse)
library(tidytuesdayR)
library(mosaic)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load(2026,week = 8 )

sfi_grants <- tuesdata$sfi_grants


glimpse(sfi_grants)

df <- sfi_grants |> 
  mutate(
    funding_1k = current_total_commitment / 1000,
    length = round(time_length(interval(start_date, end_date), "years"),2),
    funding_per_year = round(funding_1k / length, 0)
  )

glimpse(df)

data(stop_words)

text_df <- df |> 
  # mutate(proposal_test = proposal_title) |>
  unnest_tokens(word, proposal_title) |> 
  distinct() |> 
  filter(word != "2") |>
  group_by(word) |>
  summarize(
    count = n(),
    average_funding = mean(funding_1k, na.rm = TRUE),
    average_funding_per_year = mean(funding_per_year, na.rm = TRUE),
    average_number_of_years = mean(length, na.rm = TRUE),
    total_funding = sum(funding_1k, na.rm = TRUE)
  ) |> anti_join(stop_words) |> 
  filter(count > 20) |>
  filter(!word %in% c("sfi","ireland")) |> 
  # filter(average_funding > 10) |>
  arrange(desc(total_funding))

glimpse(text_df)

# investment_by_year <- df |> 
#   group_by(year = year(start_date)) |> 
#   summarise(total_investment = sum(current_total_commitment, na.rm = TRUE))
#
# print(unique(df$proposal_title))
#
ggplot(aes(x = count, y = average_funding_per_year), data = text_df) +
 geom_point()+
 geom_point(data = top_funding, color = "Red") +
 geom_smooth(se = FALSE) +
 # geom_point(data = top_avg_funding, color = "blue", shape = 21, size =2) +
 scale_y_log10()

  # geom_smooth(method = "lm") +
#
ggplot(aes(x = count, y = total_funding), data = text_df) +
 geom_point()+
 geom_point(data = top_avg_funding, color = "Red") +
 geom_smooth(se = FALSE) +
 scale_y_log10()
 
ggplot(aes(x = average_funding_per_year, y = total_funding), data = text_df) +
 geom_point()+
 geom_point(data = top_count, color = "Red") +
 geom_smooth(se = FALSE) +
 scale_y_log10()
 
ggplot(aes(x = average_funding_per_year, y = total_funding, ), data = text_df) +
 geom_point(shape = 21,aes(size = count))+
 geom_point(data = top_2, color = "Red") +
 # geom_smooth(se = FALSE) +
 scale_y_log10()
 

top_count <- text_df |> 
  slice_max(count, n = 20) |>
  mutate(word = fct_reorder(word, count))

top_funding <- text_df |> 
  slice_max(total_funding, n = 20) |>
  mutate(word = fct_reorder(word, total_funding))

top_avg_funding <- text_df |> 
  slice_max(average_funding_per_year, n = 20) |>
  mutate(word = fct_reorder(word, average_funding_per_year))

count_over_100 <- text_df |> 
  filter(count > 100) |>
  mutate(word = fct_reorder(word, count))

top <- text_df |>
  intersect(top_count) |>
  intersect(top_funding) |>
  intersect(top_avg_funding)

top_2_filter <- bind_rows(
  top_count |> select(word),
  top_funding |> select(word),
  top_avg_funding |> select(word),
) |> 
  group_by(word) |>
  summarize(n = n()) |>
  filter(n > 1)
top_2 <- text_df |> 
  filter(word %in% top_2_filter$word) 
  # mutate(word = fct_reorder(word, average_funding_per_year))

ggplot(top_count,aes(x = count, y = word))+
  geom_bar(stat = "identity")

ggplot(top_avg_funding,aes(x = average_funding_per_year, y = word))+
  geom_bar(stat = "identity")

ggplot(top_funding,aes(x = total_funding, y = word))+
  geom_bar(stat = "identity")

# how much each year

funding_by_year <- df |> 
  group_by(year = year(start_date)) |> 
  summarise(total_funding = sum(current_total_commitment, na.rm = TRUE)/ 1000000)

largest_projects <- df |> 
  slice_max(current_total_commitment, n = 50)

largest_projects$proposal_title

ggplot(funding_by_year,aes(x = year, y = total_funding))+
  geom_line()



top_projects <- total_funding_summary |> 
  slice_max(total_funding, n = 20) |>
  mutate(clean_name = fct_reorder(clean_name, total_funding))
  
centers <- c("INSIGHT", "AMBER", "CÚRAM", "ADAPT", "CONNECT", 
             "Lero", "APC", "BiOrbic", "SSPC", "iCRAG", 
             "IPIC", "VistaMilk", "MaREI", "I-Form", "FutureNeuro",
             "Confirm", "Tyndall", "CRANN", "REMEDI", "BDI", "CTVR")

# Create a regex pattern: (INSIGHT|AMBER|CÚRAM|...)
center_pattern <- paste0("(", paste(centers, collapse = "|"), ")")

total_funding_summary <- df %>%
  mutate(
    clean_name = case_when(
      # If the name contains one of our keywords, extract that keyword
      str_detect(proposal_title, regex(center_pattern, ignore_case = TRUE)) ~ 
        str_extract(proposal_title, regex(center_pattern, ignore_case = TRUE)),
      
      # Otherwise, keep the original name (prevents grouping "The" or "Advanced")
      TRUE ~ proposal_title
    ),
    # Optional: Clean up the extracted names to be all Uppercase for consistency
    clean_name = ifelse(clean_name %in% centers, toupper(clean_name), clean_name)
  ) %>%
  group_by(clean_name) %>%
  summarise(
    total_funding = sum(current_total_commitment, na.rm = TRUE),
    start_date = min(start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    length_years = round(time_length(interval(start_date, end_date), "years"), 2),
    records_combined = n(),
    original_names = paste(unique(proposal_title), collapse = "; "),
    .groups = "drop"

  ) %>%
  arrange(desc(total_funding)) 


head(total_funding_summary,20)



ggplot(top_projects, aes(x = clean_name, y = total_funding / 1e6)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Funding by Cleaned Proposal Title",
       x = "Cleaned Proposal Title",
       y = "Total Funding (Millions)") +
  theme_minimal()


glimpse(df)
top_groups <- df |> 
  group_by(research_body) |> 
  summarise(total_funding = sum(current_total_commitment, na.rm = TRUE)) |> 
  slice_max(total_funding, n = 5)

group_funding <- df |> 
  mutate(year = year(start_date)) |>
  group_by(research_body,year) |> 
  summarise(total_funding = sum(current_total_commitment, na.rm = TRUE)) |> 
  arrange(desc(total_funding)) |>
  filter(research_body %in% top_groups$research_body)

head(group_funding)


ggplot(group_funding, aes(x = year, y = total_funding / 1e6, color = research_body)) +
  geom_line() +
  labs(title = "Total Funding by Research Body Over Time",
       x = "Year",
       y = "Total Funding (Millions)",
       color = "Research Body") +
  theme_minimal() +
  theme(legend.position = "bottom")

top_projects <- total_funding_summary |> 
  slice_max(total_funding, n = 10) |>
  mutate(clean_name = fct_reorder(clean_name, total_funding))

ggplot(top_projects, aes(x = clean_name, y = total_funding / 1e6)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Funding by Cleaned Proposal Title",
       x = "Cleaned Proposal Title",
       y = "Total Funding (Millions)") +
  theme_minimal()
