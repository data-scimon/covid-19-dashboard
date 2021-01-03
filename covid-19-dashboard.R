rt <- read.csv("Rt_cases_2020_12_29.csv", sep = ";")

last_row <- tail(rt, n = 1)

rt_grep <- grep("estimate", last_row)

rt_grep

last_row$estimate

library(tidyverse)
library(plotly)

p <- ggplot(rt, aes(date_sample, estimate, group = 1)) +
  geom_line() +
  geom_smooth()

p

ggplotly(p)


ggplot(rt, aes(as.Date(date_sample), estimate, group = 1)) +
  geom_smooth() +
  scale_x_date(date_breaks = "1 month") +
  geom_hline(yintercept = 1) +
  labs(x = "",
       y = "Kontakttal") +

rt_tend

rt_tend2 <- rt_tend + abline(h = 1.0, col = "red")
