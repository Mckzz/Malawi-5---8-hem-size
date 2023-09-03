library(tidyverse)
library(ggplot2)
library(readr)


lys.tube <- read_csv("~/student_documents/UBC/Research/Malawi/data/capillary/glamour shots and lys buff tube run/lys.tube_hem.size.csv")


# strip/ line chart of area x pH, coloured by indvd

lys.tube <- lys.tube %>%
  group_by(indvd) %>%
  mutate(indvd = as.character(indvd)) %>%
  mutate(norm5.pct = ((area - area[1])/area[1])*100) %>%
  ungroup() %>%
  group_by(pH) %>%
  mutate(mean_area = mean(area)) %>%
  mutate(stdv_area = (sd(area))) %>%
  ungroup()

print(lys.tube)

# absolute plot
ggplot(data = lys.tube,
       aes(x = pH,
           y = area,
           colour = indvd)) +
  geom_point() +
  geom_line()

# norm 5 plot
ggplot(data = lys.tube,
       aes(x = pH,
           y = norm5.pct,
           colour = indvd)) +
  geom_point() +
  geom_line()

# mean abs area
ggplot(data = lys.tube,
       aes(x = pH,
           y = mean_area,
           colour = indvd)) +
  geom_point(colour = "black") +
  geom_line(colour = "black") +
  geom_point(aes(
    x= pH, 
    y= area),
    shape = 1,
    size = 3) +
  geom_errorbar(mapping = aes(x = pH,
                              ymin = mean_area - stdv_area,
                              ymax = mean_area + stdv_area),
                width = 0.2,
                colour = "black")
