# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
# ---- declare-globals ---------------------------------------------------------
path_file_input_data       <- "./data-unshared/derived/saveData_SYTYCG_Season1.csv"
path_file_input_parameters <- "./data-unshared/derived/saveParams_SYTYCG_Season1.csv"
# ---- load-data ---------------------------------------------------------------
ds      <- readr::read_csv(path_file_input_data); ds %>% glimpse()
ds_pars <- readr::read_csv(path_file_input_parameters); ds_pars %>% glimpse()

ds %>% head()
# ---- tweak-data ---------------------------------------------------------------

# ---- basic-graph -------------------------------------------------------

t1 <- ds %>% 
  dplyr::filter(graphNum == 12) 

# start with a basic boxplot
# see https://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html

# boxplot
g1 <- t1 %>% 
  ggpubr::ggboxplot(
    x         = "group"
    , y       = "val"
    , color   = "group"
    , palette = c("#00AFBB", "#E7B800")
    , add     = "jitter"
    , shape   = "group"
  )
g1 %>% print()

# histogram
g2 <- t1 %>% 
  ggpubr::gghistogram(
    x = "val"
    ,color = "group"
    ,fill  = "group"
    ,add = "mean"
    ,rug = TRUE
    ,palette = c("#00AFBB", "#E7B800")
    ,alpha = .2
  )
g2

# density
g3 <- t1 %>% 
  ggpubr::ggdensity(
    x = "val"
    ,color = "group"
    ,fill  = "group"
    ,add = "mean"
    ,rug = TRUE
    ,palette = c("#00AFBB", "#E7B800")
  )
g3

# but it's better to think in terms of facets
t2 <- ds %>% 
  dplyr::filter(graphNum %in% c(12, 24, 40 ))
g4 <-  t2 %>% 
  ggpubr::ggdensity(
    x = "val"
    ,color = "group"
    ,fill  = "group"
    ,add = "mean"
    ,rug = TRUE
    ,palette = c("#00AFBB", "#E7B800")
  )+
  facet_grid(.~graphNum)
g4

# or spreads
g5 <-  ds %>% 
  ggpubr::ggdensity(
    x = "val"
    ,color = "group"
    ,fill  = "group"
    ,add = "mean"
    ,rug = TRUE
    ,palette = c("#00AFBB", "#E7B800")
  )+
  facet_wrap(~graphNum)
g5

# ---- sample-of-graphs-1 --------------------

# conditions <- c(2, 12, 22, 32)
conditions <- c(5, 15, 25, 35)
d1 <- ds %>% 
  dplyr::filter(graphNum %in% conditions) 

# density
g6 <-  d1 %>% 
  dplyr::filter(graphNum %in% conditions) %>% 
  ggpubr::ggdensity(
    x = "val"
    ,color = "group"
    ,fill  = "group"
    ,add = "mean"
    ,rug = TRUE
    ,palette = c("#00AFBB", "#E7B800")
  )+
  facet_wrap(~graphNum)
g6

# boxplot
g7 <- d1 %>% 
  ggpubr::ggboxplot(
    x         = "group"
    , y       = "val"
    , color   = "group"
    , palette = c("#00AFBB", "#E7B800")
    , add     = "jitter"
    , shape   = "group"
  )+
  facet_wrap(~graphNum)
g7 %>% print()

# histogram
g8 <- d1 %>% 
ggpubr::gghistogram(
  x = "val"
  ,color = "group"
  ,fill  = "group"
  ,add = "mean"
  ,rug = TRUE
  ,palette = c("#00AFBB", "#E7B800")
  ,alpha = .2
)+
  facet_wrap(~graphNum)
g8

ds_pars %>% filter(graphNum %in% c(1,11,21,31))



  
# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/0-greeter/0-greeter.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
  )

