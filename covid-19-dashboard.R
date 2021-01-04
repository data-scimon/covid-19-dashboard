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


pos <- read.csv("Test_pos_over_time.csv", sep = ";")

pos <- slice(pos, 1:(n()-2))

pos$PosPct

valueBox(value = pos$PosPct, "Positiv %", subtitle = paste0("Positiv % ", pos$Date), icon = icon("percent"),
         color = "yellow")


pos1 <- slice(pos, 1:(n()-2))

ggplot(pos1, aes(as.Date(Date), PosPct, group = 1)) +
  geom_smooth() +
  coord_cartesian(ylim = c(0,50)) +
  scale_x_date(date_breaks = "1 month", expand = c(0,22)) +
  labs(x = "",
       y = "Positiv %") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Times", size = 15))


ind <- read.csv("Newly_admitted_over_time.csv", sep = ";")

library(grid)
library(gridExtra)

phoved <- ggplot(ind, aes(as.Date(Dato), Hovedstaden)) +
  geom_line()

pnord <- ggplot(ind, aes(as.Date(Dato), Nordjylland)) +
  geom_line()

grid.arrange(phoved, pnord, ncol = 2)

dead <- read.csv("Deaths_over_time.csv", sep = ";")

dead1 <- slice(dead, 1:(n()-1))

dead2 <- slice(dead, 1:(n()-2))

library(mapDK)

library(readr)

mapDK()

mun <- read_table2("Municipality_test_pos.csv",
                                     locale = locale(decimal_mark = ",", grouping_mark = "."))


mun <- read_delim("Municipality_test_pos.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                grouping_mark = "."), trim_ws = TRUE)


mun <- read.table2("Municipality_test_pos.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)

mun1 <- read.csv2("Municipality_test_pos.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)

mun$Kumulativ_incidens_.per_100000. <- as.numeric(gsub())

#
mapDK(values = "Kumulativ_incidens_.per_100000.", detail = "municipal", id = "Kommune_.navn.", data = mun)

#
reg <- read_delim("Region_summary.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                    grouping_mark = "."), trim_ws = TRUE)

reg %>%
  rename(`Region Hovedstaden` = Hovedstaden)

mapDK(values = "Indlagt_total", detail = "region", id = "Region", data = reg)

mun$`Antal_bekræftede_COVID-19`



mapDK(values = "`Antal_bekræftede_COVID-19`", detail = "municipal", id = "Kommune_.navn.", data = mun, 
              guide.label = "Bekræftede smittede totalt")


mapDK(values = "Kumulativ_incidens_.per_100000.", detail = "municipal", id = "Kommune_.navn.", data = mun, 
              guide.label = "Bekræftede smittede \nper 100.000 indbyggere")


