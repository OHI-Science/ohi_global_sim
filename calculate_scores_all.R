
# STEP 1: be sure to pull ohiprep

library(devtools)
#devtools::install_github("ohi-science/ohicore@dev") # when testing changes to ohicore
#devtools::install_github("ohi-science/ohicore@master")
library(ohicore)
library(zoo)


source('../ohiprep/src/R/common.R')

# new paths based on host machine
dirs = list(
  neptune_data  = dir_neptune_data, 
  neptune_local = dir_neptune_local,
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')


scenario = 'eez2015'
layer   = 'layers_eez'
fld_dir      = 'dir_2015a'
fld_fn       = 'fn_2015a'
f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js')


## Layers check:

    conf   = Conf(sprintf('%s/conf', scenario))
    
    # run checks on layers
    CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario), 
                layers.dir = sprintf('%s/layers', scenario), 
                flds_id    = conf$config$layers_id_fields)

## Calculate:  
    setwd(sprintf('%s', scenario)) # load_all(dirs$ohicore)
   
    # load configuration and layers
    conf   = Conf('conf')
    layers = Layers('layers.csv','layers')
  
    scores = CalculateAll(conf, layers, debug=T)
    write.csv(scores, 'scores_resil_zero.csv', na='', row.names=F)
    
    # restore working directory
    setwd('..') 
    
    

    
    
### make a plot to compare different commits within a scenario
## for some reason, the devtools package needs to be turned off for this to work 
detach("package:devtools", unload=TRUE)
source('../ohiprep/src/R/VisGlobal.R')
changePlot(repo="~/ohi-global", scenario="antarctica2014", commit="1d4dcb1", 
           fileSave="antarctica2014_finalCompare")
compare <- read.csv('figures/DataCheck/eez2015_Hackathon_julie_updates_diff_data_2015-10-21.csv')
difs_only <- filter(compare, change != 0)
table(difs_only$dimension)
tmp <- filter(compare, is.na(score) & !is.na(old_score))
tmp <- filter(compare, !is.na(score) & is.na(old_score))

# looking within a goal:
scatterPlot(repo="~/ohi-global", scenario="antarctica2014", commit="previous", goal="HAB", dim="pressure", fileSave="antarctica_hd_sea_ice_2014")
goalHistogram(scenario="antarctica2014", goal="HAB", dim="status", fileSave="HAB_new_sea_ice")

#   scenario options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
#   commit options: 'final_2014' (the final commit for the 2014 analysis), 'previous' (previous commit), a commit code (ie., 'e30e7a4')
#   saved to: ohi-global/figures/DataCheck with name from fileSave argument
### this code needs to be redone!
#source('global2014/merge_scores.R')

## Scatterplot for Antarctica:
scatterPlot <- function(repo="~/ohi-global", scenario="eez2013", commit="previous", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   commit <- "final_2014"   ## 'final_2014', 'previous', a commit code (ie., 'e30e7a4')
  #   fileSave <- 'LSP_trend_data'
  #   goal <- 'LSP'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  # devtools::install_github('ropensci/git2r') # to get latest version
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  if(commit=="previous"){
    commit2 = substring(commits(repository(repo))[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  path = paste0(scenario, '/scores.csv')
  
#   names <- read.csv(sprintf("%s/layers/rgn_labels.csv", scenario)) %>%
#     filter(type=="eez") %>%
#     select(region_id=rgn_id, label)
  
  data_old <- read_git_csv(repo, commit2, path) %>%
    select(goal, dimension, region_id, old_score=score)
  
  criteria <- ~dimension == dim
  
  data_new <- read.csv(file.path(repo, path)) %>%
    left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
    mutate(change = score-old_score) %>%
    filter_(criteria) %>%
    group_by(goal) %>% 
    mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup()
  
  data_new <- data_new[data_new$goal==goal,]  
  
  ggplot(data_new, aes(x=old_score, y=score)) +
    geom_point(shape=19) +
    theme_bw() + 
    labs(title=paste(scenario, goal, dim, commit, sep=": "), y="New scores", x="Scores from previous analysis") +
    geom_abline(slope=1, intercept=0, color="red") + 
    xlim(c(0,100))
  
  ggsave(file.path(repo, 'figures/DataCheck', paste0(fileSave, "_scatterPlot_", Sys.Date(), '.png')), width=10, height=8)
}



## look into following code:
# # DEBUG comparison for 2013a
#source('../ohidev/report/compare_scores.R')
#suppressWarnings(source('../ohidev/report/visualizeScores/visualizeScores.R'))

# prepare data for Radical 2012 and 2013 eez (need to add Antarctica and High Seas)
#source('../ohidev/report/radical.R')

# comparison 2014a
# source('../ohidev/report/compare_scenarios.R')

# library(git2r)
# devtools::install_github('ropensci/git2r')


repository(repo)
lookup(repository(repo), hex)
o=tree(lookup(repository(repo), hex))
dirname(path)
tools::file_path_sans_ext(basename(path))
git2r::content(o[basename(path)])
git2r::content(o)



data_new %>%
  filter(goal=="LSP",
         dimension=="score",
         region_id==2)

t=0
x=.5937
r=.2
p=.2
DISCOUNT=1
BETA=0.67

DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p))) * x

(1 + (0.67*trend) + ((1-0.67)*(resilience-pressures)))*status

