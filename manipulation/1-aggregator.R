# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/1-aggregator.R", output = "./stitched-output/manipulation/1-aggregator.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# import custom functions and scripts
base::source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr)          # pipes
library(ggplot2)           # graphing
# for other packages, please use qualifiers (e.g. dplyr::select() )
requireNamespace("dplyr")    # data wrangling
requireNamespace("ggpubr")   # documents
  
# ---- declare-globals ---------------------------------------------------------
# path to the data source
path_file_input_dto <- "./data-unshared/derived/0-dto.rds"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_file_input_dto)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
ds          <- dto$sentence
ds_codebook <- dto$codebook

# ----- custom-functions --------------------------------------
get_a_sample <- function(
  d,
  varname            # unique of these
  ,sample_size
  ,show_all = FALSE
){
  # varname = "offense_arrest_cd"
  sample_pool <- ds %>% 
    dplyr::distinct_(.dots = varname) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  if(show_all){ sample_size = length(sample_pool)}
  selected_sample <- sample_pool %>% sample(size = sample_size, replace = FALSE )
  
  return(selected_sample)
}  
# How to use
# ds %>% get_a_sample("person_id",  5)
# ds %>% get_a_sample("offense_arrest_cd",  5, show_all = T) 
# set.seed(42)
# target_sample <- ds %>% 
#   dplyr::filter(n_offenses > 1L) %>% 
#   get_a_sample("person_id", 500)

# ---- tweak-data ---------------------------------------------------------------
# select only the variables needed for aggregation
ds <- ds %>% 
  dplyr::mutate(
    offense_group = substr(offense_arrest_cd,1,1)  # less granular category 
    ,begin_date   = lubridate::as_date(begin_date) # ensure proper data format
  ) %>% 
  dplyr::select(c(
    "person_id"          # idnumbercomb, manually checked to represent a unique person 
    ,"begin_date"        # date the person began serving the aggregate sentence
    ,"offense_arrest_cd" # code for the offense committed   
    ,"offense_group"     # one-letter code (we need "C" - drug-related fellonies)
    ,"offense_count"     # count of offenses in inmate's sentence
    ,"offense_arrest"    # standardized description of the offense committed
  ))
ds %>% dplyr::glimpse(100)

# view the contents of the codebook for these variables
ds_codebook %>% 
  dplyr::filter(field_name %in% c(
    "begin_date"
    ,"offense_arrest_cd"
    ,"offense_arrest"
  )) %>% 
  # neat()
  knitr::kable(format = "pandoc")

# ----- agg-0 ---------------------------------------------
# We will distinguish the following levels of aggregation :

# (4) person_id                                  # person     level
# (3) person_id +              offense_group     # person     level with groups
# (2) person_id + begin_date                     # sentence   level
# (1) person_id + begin_date + offense_group     # conviction level # less granualr
# (0) person_id + begin_date + offense_arrest_cd # conviction level #  more granular

# NOTE: one may have multiple convictions on the same date

# compute variables at the lowest level of aggregation (0)
ds0 <- ds %>% 
  # dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% # for testing
  dplyr::arrange(person_id, begin_date, offense_arrest_cd) %>% 
  dplyr::group_by(person_id) %>% 
  dplyr::mutate(
    conviction_order     = dplyr::row_number() # sequential order of convictions
    ,total_n_convictions = dplyr::n() # total number of convictions a person has
  ) %>%
  # additional measures, computed within a person 
  # note that we are NOT collapsing/aggregating
  dplyr::mutate( # `mutate` NOT `summarize`, same N of rows 
    drug_related         = ifelse(offense_group == "C", TRUE, FALSE)     
    ,drug_order          = cumsum(!is.na(drug_related))               
    ,drug_order          = ifelse(is.na(drug_related), NA, drug_order)
    ,after_1996          = ifelse(begin_date > "1996-01-01", TRUE, FALSE)
    ,days_since_previous = begin_date - dplyr::lag(begin_date,1)      
  ) %>% 
  dplyr::ungroup()
# let us examine the data for three individuals
ds0 %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat(caption = "Grouped by : (PERSON) - (DATE) - (OFFENSE ARREST CODE)")

# ----- agg-function ---------------------------------------------
# this function will be applied at every aggregation level
aggregate_by_groups <- function(d){
  d_aggregate <- d %>% 
    dplyr::summarize(
      n_offense_counts           = sum(offense_count)
      ,n_convictions             = dplyr::n()
      ,n_drug_related            = sum(na.omit(drug_related))
      ,n_after_1996              = sum(na.omit(after_1996))
      ,n_drug_related_after_1996 = sum(na.omit(drug_related*after_1996))
    ) %>% 
    dplyr::ungroup()
  return(d_aggregate)
}

# ----- agg-1 ---------------------------------------------
# create aggregate at the (PERSON)-(DATE)-(OFFENSE GROUP) level
ds1 <- ds0 %>% 
  dplyr::group_by(person_id, begin_date, offense_group) %>% 
  aggregate_by_groups()

ds1 %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat(caption = "Grouped by : (PERSON) - (DATE) - (OFFENSE GROUP)")

# ----- agg-2 ---------------------------------------------
# create aggregate at the (PERSON)-(DATE) level (2)
ds2 <- ds0 %>%
  dplyr::group_by(person_id, begin_date) %>%
  aggregate_by_groups()

ds2 %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat(caption = "Grouped by : (PERSON) - (DATE)")

# ----- agg-3 ---------------------------------------------
# create aggregate at the (PERSON)-(OFFENSE GROUP) level (3)
ds3 <- ds0 %>% 
  dplyr::group_by(person_id, offense_group) %>% 
  aggregate_by_groups()

ds3 %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat(caption = "Grouped by : (PERSON) - (OFFENSE GROUP)")

# ----- agg-4 ---------------------------------------------
# create aggregate at the (PERSON) level (4)
ds4 <- ds0 %>% 
  dplyr::group_by(person_id) %>% 
  aggregate_by_groups()

ds4 %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat(caption = "Grouped by : (PERSON)")

# ---- create-sample-set ------------------------------------------------
# would would like to have a smaller number of cases to work with
# to keep values representative we sample through strata

# how many conviction does each person have?
ds4 %>% 
  dplyr::distinct(person_id, n_convictions) %>% 
  head() %>% 
  neat()

# how many people have the same numer of convictions?
ds4 %>% 
  dplyr::distinct(person_id, n_convictions) %>% 
  dplyr::group_by(n_convictions) %>% # for each number of convictions
  dplyr::summarize( # count how many people had this many convictions
    n_people = length(unique(person_id))
  ) %>% 
  neat()

# define a function to sample from each strata
sample_from_n_conviction <- function(
  d             # source data set
  ,sample_size  # get records for this many persons
  ,n_conv       # person must have this many convictions to be included
){
  # d <- ds0
  # sample_size = 6
  # n_conv = 2
  
  persons_with_n_convictions <- d %>% 
    dplyr::group_by(person_id) %>% 
    aggregate_by_groups() %>% 
    dplyr::filter(n_convictions == n_conv) %>% 
    dplyr::distinct(person_id) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  
  sample_of_persons_with_n_convictions <- persons_with_n_convictions %>% 
    sample(size = sample_size, replace = FALSE )
  
  return(sample_of_persons_with_n_convictions)
}

set.seed(42)
sample_id_2 <- ds0 %>% sample_from_n_conviction(sample_size = 6,n_conv = 2)
sample_id_3 <- ds0 %>% sample_from_n_conviction(sample_size = 3,n_conv = 3)
sample_id_4 <- ds0 %>% sample_from_n_conviction(sample_size = 4,n_conv = 4)
sample_id_6 <- ds0 %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)
sample_id_7 <- ds0 %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)
# combine stratified samples into a single data frame 
ds_sample <- ds %>% 
  dplyr::filter(person_id %in% c(
    sample_id_2
    ,sample_id_3
    ,sample_id_4
    ,sample_id_6
    ,sample_id_7
  )) %>% 
  dplyr::arrange(person_id, begin_date) %>% 
  dplyr::select(
    person_id
    ,begin_date
    ,offense_arrest_cd
    ,offense_count
    ,offense_arrest
    ,offense_group
  )

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------
dto <- list(
  "person"                  = ds4
  ,"person_offense"         = ds3 
  ,"person_date"            = ds2
  ,"person_date_offense"    = ds1
  ,"person_date_offense_cd" = ds0
)
dto %>% pryr::object_size()
dto %>% saveRDS("./data-unshared/derived/1-dto.rds") # for piping further


# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/1-aggregator/1-aggregator.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



