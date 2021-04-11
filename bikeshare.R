# This script was my submission for question 1.

setwd("C:/Users/BOWDLE/Documents/Udacity/udacity-git-course/pdsnd_github")

library(tidyverse)

dat.NYC = read.csv('new-york-city.csv')
dat.CHI = read.csv('chicago.csv')

dat.CHI.mutated <- 
  dat.CHI %>%
  mutate(City = "Chicago") %>%
  mutate(Age = (2021-Birth.Year))

dat.NYC.mutated <- 
  dat.NYC %>%
  mutate(City = "New York City") %>%
  mutate(Age = 2021-Birth.Year)

dat.Q1.combined <- rbind(dat.CHI.mutated, dat.NYC.mutated)

##

sum.dat.combined <-
  dat.Q1.combined %>%
  filter(Gender == 'Male' | Gender == 'Female') %>%
  filter(Birth.Year > 1920) %>%
  na.omit() %>%
  group_by(Gender, City) %>%
  summarise(mean.age = mean(Age, na.rm = TRUE))

##

dat.Q1.combined %>%
  filter(Gender == 'Male' | Gender == 'Female') %>%
  filter(Birth.Year > 1920) %>%  
  na.omit() %>%
  ggplot(aes(x=Age)) +
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white", na.rm=TRUE) +
  geom_density(alpha=.2, fill="lightpink") +
  coord_cartesian(xlim = c(0, 100)) +
  facet_grid(Gender ~ City) +
  labs(x = "Approximate Age (Years)",
       y = "Density",
       subtitle = "The approximate age distributions of Bikeshare users born after 1920",
       caption = "Data source: Udacity Bikeshare dataset") +
  theme_bw()

dat.Q1.combined %>%
  filter(User.Type == "Subscriber") %>%    
  filter(Gender == 'Male' | Gender == 'Female') %>%
  filter(Birth.Year > 1920) %>%
  group_by(City) %>%
  summarize(
    city.mean.age = sprintf("%.1f", mean(Age, na.rm = TRUE)),
    city.median.age = sprintf("%.1f", median(Age, na.rm = TRUE))
      )

dat.Q1.AOV <-
  dat.Q1.combined %>%
  filter(User.Type == "Subscriber") %>%    
  filter(Gender == 'Male' | Gender == 'Female') %>%
  filter(Birth.Year > 1920)

aov.Q1 <- aov(Age ~ City, dat.Q1.AOV)
TukeyHSD(aov.Q1)