# Quick chart
library(ggplot2)
library(dplyr)
df <- readxl::read_xlsx("GEEAP Systematic Search (All Studies).xlsx", sheet = 2)

df %>% 
    ggplot() +
    geom_point(aes(x = `Cost Per SD (USD) (Calculated)`, y = `Treatment Arm`)) +
    # geom_point(aes(x = DB, y = LR)) +
    geom_segment(aes(x = `Cost Per SD (USD) (Calculated)`, y = `Treatment Arm`, xend = 0, yend = 0)) 


df %>% ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 size=3)
