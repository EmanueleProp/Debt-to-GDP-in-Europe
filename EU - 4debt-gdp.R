#public debt evolution
library(readxl)
library(ggplot2)
library(gganimate)
library(dplyr)
library(png)
library(gifski)
library(animation)


data$debtgdp <- ifelse(data$debtgdp == "no data", NA, data$debtgdp)

data = data %>%
    filter(!is.na(debtgdp)) 

data %>%
  ggplot(aes(x = Year, y = as.numeric(debtgdp), col = Country)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 60), lty = 2) +
  annotate("text", x = 2011, y = 53.5, 
           label = "Maastricht Treaty\n debt-to-GDP criterion", size = 3) +
  scale_color_viridis_d(option = "C", end = .75) +
  labs(
    title = "European debt-to-GDP ratios (1950 - 2020)",
    x = "Year", 
    y = "Debt-to-GDP (%)"
  ) + scale_y_continuous(breaks=seq(0,225,25)) + scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  transition_reveal(Year)



anim2 = data %>%
  ggplot(aes(x = Year, y = as.numeric(debtgdp), col = Country)) +
  geom_line(size = 1) +   
  geom_segment(aes(xend = 2035, yend = as.numeric(debtgdp)), linetype = 2) + 
  geom_text(aes(x = 2030, label = Country), hjust = 0) +
  geom_hline(aes(yintercept = 60), lty = 2) +
  annotate("text", x = 2020, y = 53.5, 
           label = "Maastricht Treaty\n debt-to-GDP criterion", size = 3) +
  scale_color_viridis_d(option = "C", end = .75) +
  labs(
    title = "European debt-to-GDP ratios (1950 - 2020)",
    x = "Year", 
    y = "Debt-to-GDP (%)"
  ) + 
  xlim(1950, 2040) + 
  coord_cartesian(clip = 'off') + 
  guides(col = "none")  + scale_y_continuous(breaks=seq(0,225,25)) + scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  transition_reveal(Year)

z <- animate(anim2, nframes = 150, end_pause = 50)

