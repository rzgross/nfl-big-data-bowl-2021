library(tidyverse)
library(gganimate)
library(magick)
one_week_data <- read_csv("week13.csv")
plays_data <- read_csv("plays.csv")

generate_play = function(game, play){
  
  one_play = one_week_data %>% filter(gameId == game & playId == play)    
  one_play_summary = plays_data %>% filter(gameId == game, playId == play)
  
  description = one_play_summary$playDescription[1]
  
  one_play_for_plot = one_play
  one_play_for_plot$time = format(one_play_for_plot$time, format = "%Y-%m-%d %H:%M:%OS3")
  
  one_play_for_plot_home =  one_play_for_plot %>% filter(team == 'home') 
  one_play_for_plot_away =  one_play_for_plot %>% filter(team == 'away')
  one_play_for_plot_ball =  one_play_for_plot %>% filter(team == 'football') 
  
  # specify where the extra markings should go
  x_markings = seq(from = 11, to = 109, by = 1)
  x_markings = x_markings[x_markings %% 5 != 0]
  
  y_bottom = rep(1, length(x_markings))
  y_lower_mid = rep(18, length(x_markings))
  y_upper_mid = rep(53.3-18, length(x_markings))
  y_top = rep(53.3 - 1, length(x_markings))
  
  # specify where the numbers should go
  numbers_x = seq(from = 20, to = 100, by = 10)
  numbers_bottom_y = rep(3, length(numbers_x))
  numbers_top_y = rep(53.3-3, length(numbers_x))
  
  # generate the base plot to animate
  base_plot = ggplot(one_play_for_plot_ball, aes(x = x, y = y)) +
    xlim(0,120) + ylim(0, 53.3) +
    theme(panel.background = element_rect(fill='darkgreen', colour='red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks =  element_blank(),
          plot.title = element_text(size = 12)) + 
    ggtitle(description) +
    geom_hline(yintercept=0, colour = 'white') +
    geom_hline(yintercept=53.3, colour = 'white') +
    geom_vline(xintercept = 0, colour = 'white') + 
    geom_vline(xintercept = 10, colour = 'white') +
    geom_vline(xintercept = 15, colour = 'white') +
    geom_vline(xintercept = 20, colour = 'white') +
    geom_vline(xintercept = 25, colour = 'white') +
    geom_vline(xintercept = 30, colour = 'white') +
    geom_vline(xintercept = 35, colour = 'white') +
    geom_vline(xintercept = 40, colour = 'white') +
    geom_vline(xintercept = 45, colour = 'white') +
    geom_vline(xintercept = 50, colour = 'white') + 
    geom_vline(xintercept = 55, colour = 'white') +
    geom_vline(xintercept = 60, colour = 'white') +
    geom_vline(xintercept = 65, colour = 'white') +
    geom_vline(xintercept = 70, colour = 'white') +
    geom_vline(xintercept = 75, colour = 'white') +
    geom_vline(xintercept = 80, colour = 'white') +
    geom_vline(xintercept = 85, colour = 'white') +
    geom_vline(xintercept = 90, colour = 'white') +
    geom_vline(xintercept = 95, colour = 'white') +
    geom_vline(xintercept = 100, colour = 'white') +
    geom_vline(xintercept = 105, colour = 'white') +
    geom_vline(xintercept = 110, colour = 'white') +
    geom_vline(xintercept = 120, colour = 'white') +
    geom_rect(aes(xmin = 0.2 , xmax = 9.8, ymin = 0.2, ymax = 53.1) ) + 
    geom_rect(aes(xmin = 110.2 , xmax = 119.8, ymin = 0.2, ymax = 53.1) ) + 
    geom_text(data = data.frame(x = x_markings, y =y_bottom), aes(label = 'l'), color = 'white')+
    geom_text(data = data.frame(x = x_markings, y =y_lower_mid), aes(label = 'l'), colour = 'white') +
    geom_text(data = data.frame(x = x_markings, y =y_upper_mid), aes(label = 'l'), color = 'white') +
    geom_text(data = data.frame(x = x_markings, y =y_top), aes(label = 'l'), color = 'white') + 
    geom_text(data = data.frame(x = 12.5, y = 53.3 / 2.0), aes(label = 'l'), color = 'white', size = 10) + 
    geom_text(data = data.frame(x = 107.5, y = 53.3 / 2.0), aes(label = 'l'), color = 'white', size = 10) + 
    geom_text(data= data.frame(x = numbers_x, y = numbers_bottom_y), aes(label = 50 - abs( 50 - (numbers_x-10) )), colour = 'white', size = 5) +
    geom_text(data= data.frame(x = numbers_x, y = numbers_top_y), aes(label = 50 - abs( 50 - (numbers_x-10) )), colour = 'white', size = 5) +
    geom_point(data = one_play_for_plot_home, aes(x=x,y=y), colour = 'red', size = 5) + 
    geom_point(data = one_play_for_plot_away, aes(x=x,y=y), colour = 'black', size = 5) +
    geom_text(data = one_play_for_plot_home, aes(x = x, y = y, label = jerseyNumber), colour = 'white') +
    geom_text(data = one_play_for_plot_away, aes(x = x, y = y, label = jerseyNumber), colour = 'white') +
    geom_point(colour = 'white', size = 2)
  
  
  anim = base_plot  +   transition_states(time,
                                          transition_length = 2, state_length = 1)
  
  
  animate(anim, renderer = magick_renderer(), width = 800, height = 400)
}

generate_play(game = 2018120205, play = 1196)
