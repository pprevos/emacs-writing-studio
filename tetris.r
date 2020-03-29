## Visualise Tetris scores
library(tidyverse)

tetris_scores <- read.table("~/.emacs.d/games/tetris-scores")

tetris_scores %>%
    transmute(DateTime = as.POSIXct(paste(V4, V3, V6, V5),
                                    format = "%d %B %Y %T"),
           Player = paste(V7, V8),
           Score = V1) %>%
    ggplot(aes(DateTime, Score, colour = Player)) +
    geom_point(col = "blue", size = 4) +
    geom_smooth(col = "red") + 
    facet_wrap(~Player)
