# taken from Adam Sonty's kaggle notebook
# https://www.kaggle.com/adamsonty/nfl-big-data-bowl-a-basic-field-control-model
library(tidyverse)
library(gganimate)
library(mvtnorm)

# plot image settings
options(repr.plot.width=20, repr.plot.height = 10)

df_games <- read_csv("games.csv")

df_plays <- read_csv("plays.csv")

# read in play data
play_ <- df_plays %>%
  filter(gameId == 2018111200, playId == 1036)
game_ <- df_games %>%
  filter(gameId == 2018111200)
tracking_data_file = read.csv('week10.csv')

df_track <- tracking_data_file %>%
  dplyr::filter(gameId == play_$gameId, playId == play_$playId)

play_direction_ <- df_track %>% head(1) %>% dplyr::pull(playDirection)

df_track <- df_track %>%
  dplyr::select(x, y, s, dir, event, displayName, jerseyNumber, frameId, team)


df_track <- df_track %>%
  dplyr::mutate(
    dir_rad = dir * pi / 180,
    v_x = sin(dir_rad) * s,
    v_y = cos(dir_rad) * s,
    v_theta = atan(v_y / v_x),
    v_theta = ifelse(is.nan(v_theta), 0, v_theta),
    team_name = case_when(
      team == "home" ~ game_$homeTeamAbbr,
      team == "away" ~ game_$visitorTeamAbbr,
      TRUE ~ team,
    )
  ) %>%
  dplyr::select(frameId, event, team = team_name, jerseyNumber, displayName, x, y, s, v_theta, v_x, v_y)


plot_field <- function(field_color="#ffffff", line_color = "#212529", number_color = "#adb5bd") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      legend.title.align = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 10,
      colour = number_color,
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 10,
      colour = number_color, 
    )
  
  return(field)
}


fetch_team_colors <- function(team_colors_=NULL, h_team_, a_team_, diverge_=FALSE) {
  colors_url <- "https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"
  
  if (is.null(team_colors_)) {
    team_colors_ <- suppressMessages(readr::read_tsv(colors_url))
  }
  
  h_team_color1 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color1)
  h_team_color2 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color2)
  a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color1)
  a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color2)
  
  if (diverge_ == TRUE) {
    h_team_color1_family <- team_colors_ %>% filter(teams == h_team_) %>% select(color1_family) %>% pull()
    a_team_color1_family <- team_colors_ %>% filter(teams == a_team_) %>% select(color1_family) %>% pull()
    
    if (h_team_color1_family == a_team_color1_family) {
      a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% select(color2) %>% pull()
      a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% select(color1) %>% pull()
    }
  }
  
  df_colors <- tibble(
    home_1 = h_team_color1, home_2 = h_team_color2, away_1 = a_team_color1, away_2 = a_team_color2
  )
  
  
  return(df_colors)
}


if (play_direction_ == "left") {
  line_of_scrimmage = play_$absoluteYardlineNumber
  to_go_line = line_of_scrimmage - play_$yardsToGo
} else {
  line_of_scrimmage = 100 - play_$absoluteYardlineNumber
  to_go_line = line_of_scrimmage + play_$yardsToGo
}

df_colors <- fetch_team_colors(h_team_ = game_$homeTeamAbbr, a_team_ = game_$visitorTeamAbbr, diverge_ = T)

play_frames <- plot_field() + 
  # line of scrimmage
  annotate(
    "segment",
    x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
    colour = "#0d41e1", size = 1.5
  ) +
  # 1st down marker
  annotate(
    "segment",
    x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
    colour = "#f9c80e", size = 1.5
  ) +
  # away team velocities
  geom_segment(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) + 
  # home team velocities
  geom_segment(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) +
  # away team locs and jersey numbers
  geom_point(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y),
    fill = "#f8f9fa", colour = df_colors$away_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$away_1, size = 4.5
  ) +
  # home team locs and jersey numbers
  geom_point(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y),
    fill = df_colors$home_1, colour = df_colors$home_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$home_2, size = 4.5, 
  ) +
  # ball
  geom_point(
    data = df_track %>% dplyr::filter(team == "football"),
    mapping = aes(x = x, y = y),
    fill = "#935e38", colour = "#d9d9d9",
    shape = 21, alpha = 1, size = 4, stroke = 1
  ) +
  # title 
  labs(title = play_$playDescription) +
  # animation stuff
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

play_length <- length(unique(df_track$frameId))
play_anim <- animate(
  play_frames,
  fps = 10, 
  nframe = play_length,
  width = 800,
  height = 400,
  end_pause = 0
)

play_anim
anim_save("myanimation.gif", play_anim)
