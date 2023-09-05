library(tidyverse)
library(ggplot2)
library(readr)


data <- read_csv("~/student_documents/UBC/Research/Malawi/data/capillary/glamour shots and lys buff tube run/lys.tube_hem.size.csv")


# strip/ line chart of area x pH, coloured by indvd

lys.tube <- data %>%
  group_by(indvd) %>%
  mutate(indvd = as.character(indvd)) %>%
  mutate(norm5.pct = ((area - area[1])/area[1])*100) %>%
  ungroup() %>%
  group_by(pH) %>%
  mutate(mean_area = mean(area)) %>%
  mutate(stdv_area = (sd(area))) %>%
  ungroup()

print(lys.tube)

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
  geom_hline(yintercept = mean(in_vivo_dimensions$area)) +
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


# absolute plot
ggplot(data = lys.tube,
       aes(x = pH,
           y = area,
           colour = indvd)) +
  geom_point(size  = 4) +
  geom_line() +
  geom_hline(yintercept = 0.0869, colour = "red") + #indvd 2
  geom_hline(yintercept = 0.08060, colour = "darkgoldenrod") + #indvd 3
  geom_hline(yintercept = 0.06342, colour = "chartreuse4") + #indvd 4
  geom_hline(yintercept = 0.10681, colour = "aquamarine2") + #indvd 5
  geom_hline(yintercept = 0.07710, colour = "deepskyblue") + #indvd 6
  geom_hline(yintercept = 0.08072, colour = "blueviolet") + #indvd 7
  geom_hline(yintercept = 0.06351, colour = "deeppink1") #indvd 8



# splitting in vivo sizes into the tube data frame

lys.tube_for.vivo <- data %>%
  group_by(indvd) %>%
  mutate(indvd = as.character(indvd))
print(lys.tube_for.vivo)


# data frame with in vivo sac sizes

in_vivo_dimensions <- read_csv("~/student_documents/UBC/Research/Malawi/data/capillary/glamour shots and lys buff tube run/in_vivo_dimensions.csv") %>%
  mutate(indvd = as.character(indvd)) %>%
  mutate(pH = NA) %>%
  select(indvd, pH, area, diameter)
print(in_vivo_dimensions)

mean(in_vivo_dimensions$area)
