## Visualise Tetris scores
library(tidyverse)

tetris_scores <- read.table("~/.emacs.d/games/tetris-scores")

str(tetris_scores)

tetris_scores %>%
    transmute(DateTime = as.POSIXct(paste(V8, V7, V10, V9), format = "%d %B %Y %T"),
           Player = paste(V3, V4),
           Score = V1) %>%
    ggplot(aes(DateTime, Score, colour = Player)) +
        geom_point(col = "blue") +
        geom_smooth(col = "red") + 
        facet_wrap(~Player)
