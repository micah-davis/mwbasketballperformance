library(tidyverse)
library(lubridate)
library(cbbdata)
library(cbbplotR)
library(ggrepel)

mw_25 <- cbd_torvik_ratings_archive(conf = "MWC",
                           year = 2025)

mw_25 <- mw_25 |> 
  mutate(month = floor_date(date, "month")) |> 
  group_by(team, date) |> 
  summarize(avg_barthag = mean(barthag, na.rm = TRUE)) |> 
  ungroup()

begin <- mw_25 |> 
  group_by(team) |> 
  filter(date == min(date))

end <- mw_25 |> 
  group_by(team) |> 
  filter(date == max(date))

ggplot(mw_25, aes(x = date, y = avg_barthag, team = team)) +
  scale_color_cbb_teams() +
  geom_line(aes(color = team)) +
  facet_grid(~team) +
  labs(title = "Mountain West Performance over the 2024/2025 Season",
       subtitle = "Measured by Bart Torvik's Barthag",
       caption = "By Micah Davis
       data and logos from cbbdata and cbbplotR",
       y = "Average Barthag") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.5))
