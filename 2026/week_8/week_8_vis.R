library(tidyverse)
library(tidytuesdayR)
library(mosaic)
library(patchwork)
library(ggtext)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2026,week = 8 )

sfi_grants <- tuesdata$sfi_grants


glimpse(sfi_grants)

df <- sfi_grants |> 
  mutate(
    funding_1k = current_total_commitment / 1000,
    length = round(time_length(interval(start_date, end_date), "years"),2),
    funding_per_year = round(funding_1k / length, 0)
  )

centers <- c("INSIGHT", "AMBER", "CÚRAM", "ADAPT", "CONNECT", 
             "Lero", "APC", "BiOrbic", "SSPC", "iCRAG", 
             "IPIC", "VistaMilk", "MaREI", "I-Form", "FutureNeuro",
             "Confirm", "Tyndall", "CRANN", "REMEDI", "BDI", "CTVR")

center_pattern <- paste0("(", paste(centers, collapse = "|"), ")")

total_funding_summary <- df %>%
  mutate(
    clean_name = case_when(
      str_detect(proposal_title, regex(center_pattern, ignore_case = TRUE)) ~ 
        str_extract(proposal_title, regex(center_pattern, ignore_case = TRUE)),
      
      TRUE ~ proposal_title
    ),
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

top_projects <- total_funding_summary |> 
  slice_max(total_funding, n = 10) |>
  mutate(clean_name = fct_reorder(clean_name, total_funding))

top_projects

plot_dates <- top_projects |>
  pivot_longer(cols = c(start_date, end_date), names_to = "date_type", values_to = "date")

# theme

# theme_set(theme_minimal())
#
# theme_update(
#   plot_margin = margin(25, 15, 15, 25),
#   panel.grid.major.x = element_line(color = "grey94"),
#   panel.grid.major.y = element_blank(),
#   panel.grid.minor =  element_blank(),
#
# )

font_add_google("Playfair Display", "playfair")
font_add_google("IBM Plex Sans", "plex")
font_add_google("Fira Sans", "fira")
font_add_google("Spectral", "spectral")
font_add_google("Inter", "inter")
font_add_google("Work Sans", "work")
font_add_google("Work Sans", "work2", bold.wt = 500)

showtext_auto()

subtitle_text <- "The Science Foundation Ireland (SFI) has funded a variety of projects across different research centers. This visualization highlights the top 10 funded p rojects,
  showcasing both their total funding amounts and their durations.a large amount of funding is concentrated in a few key centers,
  with some projects receiving significantly more funding than others. The duration of these projects varies, with some spanning several years while others are shorter-term initiatives. This distribution of funding and project lengths reflects the strategic priorities and research focus areas of SFI. The name of the centres listed below are:
  Irelands Big Data and Analytics Reasearch Centre (INSIGHT), Alimentary Pharmabiotic Centre (APC), Advanced Materials and BioEngineering Reasearch Centre (AMBER),
  The Irish Software Engineering Research Centre (LERO), Tyndall National Institute (Tyndall), Centre for Reasearch in Medical Devices (CÚRAM),
  Centre for Digital Content Platform Reasearch (ADAPT), The Centre for Future Networks & Communications (CONNECT), Solid State Pharmaceuticals Cluster (SSPC),
  Irish Center for Reasearch in Applied Geoscienes (iCRAG)"

subtitle_text <- "The Science Foundation Ireland (__SFI__) has funded a variety of projects across different research centers. This visualization highlights the top 10 funded projects,<br>demonstrating both their total funding amounts and durations. A large amount of funding is concentrated in a few key research centers.<br><br>
__Research Centres:__<br>
Ireland's Big Data and Analytics Research Centre (__INSIGHT__) | Alimentary Pharmabiotic Centre (__APC__) | Advanced Materials and BioEngineering Research Centre (__AMBER__) |<br> The Irish Software Engineering Research Centre (__LERO__) | Tyndall National Institute (__TYNDALL__) | Centre for Research in Medical Devices (__CÚRAM__) |<br>Centre for Digital Content Platform Research (__ADAPT__) | The Centre for Future Networks & Communications (__CONNECT__) | Solid State Pharmaceuticals Cluster (__SSPC__) |<br> Irish Centre for Research in Applied Geosciences (__iCRAG__)"

p1 <- ggplot(top_projects) +
  geom_segment(aes(x = start_date, xend = end_date, y = clean_name, yend = clean_name),
    size = 9, color = "#358558", lineend = "round") +
  # geom_point(data = plot_dates, aes(x = date), color = "red", size = 3, legend = NA)+
  labs(x = "Project Duration", y = NULL) +
  theme_minimal() +
  theme(
  axis.text.y = element_blank(),
  axis.text.x = element_text(family = "work",size = 16),
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "work2",size = 18, face = "bold",vjust = -0.5, color = "grey35"),
  panel.grid.major.x = element_line(color = "grey75", linetype = "dashed"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor =  element_blank(),
  plot_margin = margin(5, 0, 10, 5),
  )
  

p2 <- ggplot(top_projects, aes(y = clean_name, x = total_funding / 1e6)) +
  geom_bar(stat = "identity", fill = "#c27525", width = 0.7) +
  geom_label(aes(label = paste0(round(total_funding / 1e6, 1), "M")), 
            position = position_nudge(x = 7), color = "black", size = 5, family = "plex",fill = "#f5f0e1") +
  theme_minimal(base_family = "work") +
  labs(x = "Total Funding (Millions)", y = NULL) +
  theme(
    plot_margin = margin(5, 5, 10, 0),
    # axis.text.y = element_text(hjust = 0.5, margin = margin(l = 2)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "work",size = 14),
    axis.title.x = element_text(family = "work2",size = 18, face = "bold",vjust = -0.5, color = "grey35"),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor =  element_blank(),
    )

p_mid <- ggplot(top_projects, aes(y = clean_name)) +
  geom_text(aes(label = clean_name), hjust = 0.5, size = 6.6, x = 0,family = "spectral",fontface = "bold", color = "grey20") +
  theme_void() +
  scale_x_continuous(limits = c(-1,1))+
  theme(
  margin = margin(5, 0, 10, 0),
)

p1 + p_mid +p2 + 
  plot_layout(widths = c(4, 0.75, 4) ) &
  plot_annotation(title = "Top 10 Funded Projects by SFI",
                 subtitle = subtitle_text,
                 caption = "Data Source: SFI Grants Dataset | Visualization: Nathaniel Clark") &
  theme(
    # aspect.ratio = 0.8,
    plot.background = element_rect(fill = "#f0e9d3"),
    plot.title = element_text(family = "spectral",size = 42, face = "bold"),
    plot.subtitle = element_markdown(family = "work",size = 16,
                                   color = "grey30", lineheight = 1.2,
                                   hjust = 0, margin = margin(10, 10, 10, 10)),
    plot.caption = element_text(family = "work",size = 12, vjust = -1),
    # plot.margin = margin(20, 10, 2, 10)
  )

ggsave("final_plot3.svg",)
