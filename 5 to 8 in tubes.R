library(tidyverse)
library(ggplot2)
library(readr)


# read in and do some stuff with sizes_in_tube.csv

sizes_in_tube <- read_csv("~/student_documents/UBC/Research/Malawi/data/capillary/glamour shots and lys buff tube run/sizes_in_tube.csv")

tube.sacs.calc <- sizes_in_tube %>%
  group_by(indvd) %>%
  mutate(indvd = as.character(indvd)) %>%
  mutate(area.norm5.pct = ((area - area[1])/area[1])*100) %>%
  ungroup() %>%
  group_by(pH) %>%
  mutate(mean_area = mean(area)) %>%
  mutate(stdv_area = (sd(area))) %>%
  ungroup()

print(tube.sacs.calc)


# read in and do some stuff with size_in_animal.csv

in_vivo_dimensions <- read_csv("~/student_documents/UBC/Research/Malawi/data/capillary/glamour shots and lys buff tube run/size_in_animal.csv") %>%
  mutate(indvd = as.character(indvd)) %>%
  #mutate(pH = NA) %>%
  select(indvd, area, diameter)

print(in_vivo_dimensions)

mean(in_vivo_dimensions$area)


# mean areas in experiment with horizontal mean line for size in intact animal
## I was hoping for a better way to estimate this intersection, like via a sigmoid model??
ggplot(data = tube.sacs.calc,
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

# sac sizes in the experiment, with horizontal lines for each indicating airsac size in the intact animal
## I tried matching the colours as best I could for now
ggplot(data = tube.sacs.calc,
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


# sizes normalized to pH5
# with a mean %change line 

tube.sacs.calc <- tube.sacs.calc %>%
  group_by(pH) %>%
  mutate(mean_for_pH = mean(area.norm5.pct)) %>%
  ungroup() %>%
  mutate(vivo_diff_5 = (((mean(in_vivo_dimensions$area)) - mean_area[1]) / mean_area[1]) *100 )

print(tube.sacs.calc)

ggplot(data = tube.sacs.calc,
       aes(x = pH)) +
  geom_point(aes(y = area.norm5.pct, colour = indvd)) +
  geom_line(aes(y = area.norm5.pct, colour = indvd)) +
  geom_line(aes(y = mean_for_pH)) +
  geom_hline(yintercept = 1.3)



# splitting in vivo sizes into the tube data frame

lys.tube_for.vivo <- data %>%
  group_by(indvd) %>%
  mutate(indvd = as.character(indvd))
print(lys.tube_for.vivo)



