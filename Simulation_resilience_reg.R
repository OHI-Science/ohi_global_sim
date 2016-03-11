#############################################
## Altering regulatory resilience data
#############################################
library(dplyr)
library(pander)

## ID assessement folder
assess <- "eez2015"

## organizing information
goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')


## ID regulatory resilence layers

res <- read.csv(sprintf('%s/conf/resilience_weights.csv', assess))

layers <- res$layer[res$type %in% "regulatory"]
layers <- paste0(layers, ".csv")


############################################
# Goal: Determine how scores would change if all regulatory resilience layers had a perfect score.

# read each one, change values to one and save in layers file

for(layer in layers) { # layer = 'alien_species.csv'
  
  data <- read.csv(sprintf("%s/layers_real_data/%s", assess, layer))
  data[, 2] <- 1
  write.csv(data, sprintf("%s/layers/%s", assess, layer), row.names=FALSE)
}

## Calculate scores (calculate_scores_all.R)

## Compare scores

real <- read.csv("eez2015/scores.csv") %>%
  select(goal, dimension, region_id, score_real = score)

data <- read.csv("eez2015/scores_resil_perfect.csv") %>%
  select(goal, dimension, region_id, score_perfect = score) %>%
  left_join(real) %>%
  mutate(difference = score_perfect - score_real)

difference <- data %>%
  filter(region_id != 0) %>%
  filter(dimension == "score") %>%
  group_by(goal) %>%
  summarize(mean_dif = round(mean(difference, na.rm = TRUE), 2), 
            min = round(min(difference, na.rm = TRUE), 2),
            max = round(max(difference, na.rm = TRUE), 2), 
            sd =  round(sd(difference, na.rm = TRUE), 2),
            CI_95 = round(2*sd, 2)) %>%
  mutate(goal = factor(goal, levels=goals)) %>%
  arrange(goal)
  
scores <- data %>%
  filter(region_id != 0) %>%
  filter(dimension == "score") %>%
  group_by(goal) %>%
  summarize(mean = round(mean(score_real, na.rm = TRUE), 1), 
            mean_sim = round(mean(score_perfect, na.rm = TRUE), 1)) %>%
  mutate(goal = factor(goal, levels=goals)) %>%
  arrange(goal)

final <- scores %>%
  left_join(difference) %>%
  select(-sd, -CI_95)
  
 table <-  kable(final,format="pandoc")
table <- pandoc.table(final, style = 'rmarkdown')



############################################
# Goal: Determine how scores would change if all regulatory resilience layers had a zero score.

# read each one, change values to one and save in layers file

for(layer in layers) { # layer = 'alien_species.csv'
  
  data <- read.csv(sprintf("%s/layers_real_data/%s", assess, layer))
  data[, 2] <- 0
  write.csv(data, sprintf("%s/layers/%s", assess, layer), row.names=FALSE)
}

## Calculate scores (calculate_scores_all.R)

## Compare scores

real <- read.csv("eez2015/scores.csv") %>%
  select(goal, dimension, region_id, score_real = score)

data <- read.csv("eez2015/scores_resil_zero.csv") %>%
  select(goal, dimension, region_id, score_perfect = score) %>%
  left_join(real) %>%
  mutate(difference = score_perfect - score_real)

check <- data %>%
  filter(dimension=="score")

check %>%
  filter(difference < -4)

data %>% 
  filter(dimension == "resilience") %>%
  filter(goal == "SPP")

difference <- data %>%
  filter(region_id != 0) %>%
  filter(dimension == "score") %>%
  group_by(goal) %>%
  summarize(mean_dif = round(mean(difference, na.rm = TRUE), 2), 
            min = round(min(difference, na.rm = TRUE), 2),
            max = round(max(difference, na.rm = TRUE), 2), 
            sd =  round(sd(difference, na.rm = TRUE), 2),
            CI_95 = round(2*sd, 2)) %>%
  mutate(goal = factor(goal, levels=goals)) %>%
  arrange(goal)

scores <- data %>%
  filter(region_id != 0) %>%
  filter(dimension == "score") %>%
  group_by(goal) %>%
  summarize(mean = round(mean(score_real, na.rm = TRUE), 1), 
            mean_sim = round(mean(score_perfect, na.rm = TRUE), 1)) %>%
  mutate(goal = factor(goal, levels=goals)) %>%
  arrange(goal)

final <- scores %>%
  left_join(difference) %>%
  select(-sd, -CI_95)

table <- pandoc.table(final, style = 'rmarkdown')




