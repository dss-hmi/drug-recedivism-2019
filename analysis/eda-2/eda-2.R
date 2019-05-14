# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
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
# adjust column names
ds <- ds %>% 
  dplyr::select(c(
    "person_id"          # idnumbercomb, manually checked to represent a unique person 
    ,"begin_date"        # date the person began serving the aggregate sentence
    ,"offense_count"     # count of offenses in inmate's sentence
    ,"offense_arrest_cd" # code for the offense committed   
    ,"offense_arrest"    # standardized description of the offense committed
    # the rest will be computed
    # ,"conviction_id"     # person_id + offense_arrest_cd
    # ,"year"              # calendar year in which offense tool place
    # ,"offense_group"     # one-letter code (we need "C" - drug-related fellonies)
  ))
ds %>% dplyr::glimpse(50)

ds <- ds %>% 
  # tweak existing
  dplyr::mutate(
    begin_date         = lubridate::as_date(begin_date)
    # ,offense_begin_date  = lubridate::as_date(as.integer(offense_begin_date), origin = "1900-01-01" )
  ) %>% 
  # create new
  dplyr::mutate(
    # to discern multiple convictions on the same date, compute conviction_id
    conviction_id       = paste0(person_id,"-",offense_arrest_cd)  
    # add auxillary variable for grouping and printing
    ,year               = lubridate::year(begin_date)
    ,month              = lubridate::month(begin_date)
    ,offense_group      = substr(offense_arrest_cd,1,1)
  ) 

# view the contents of the codebook for these variables
ds_codebook %>% 
  dplyr::filter(field_name %in% c(
    "begin_date"
    # ,"offense_begin_date" # a lot of missing values
    ,"offense_arrest_cd"
    ,"offense_arrest"
  )) %>% 
  # neat()
  knitr::kable(format = "pandoc")

# ---- create-sample-set ------------------------------------------------
# would would like to have a smaller number of cases to work with
# to keep values representative we sample through strata

# how many conviction does each person have?
a <- ds %>% 
  dplyr::group_by(person_id) %>% # for each person
  dplyr::summarize(
    n_convictions = length(unique(conviction_id)) # count how many convictions s/he has
  ) %>% 
  dplyr::ungroup() 
a %>% head()
# how many people have the same numer of convictions?
b <- a %>% 
  dplyr::group_by(n_convictions) %>% # for each number of convictions
  dplyr::summarize(
    n_people = length(unique(person_id)) # count how many people had this many convictions
  )
b %>% neat()

# define a function to sample from each strata
sample_from_n_conviction <- function(
  d             # source data set
  ,sample_size  # get records for this many persons
  ,n_conv       # person must have this many convictions to be included
){
  
  a <- d %>% 
    dplyr::group_by(person_id) %>% # for each person
    dplyr::summarize(
      n_convictions = length(unique(conviction_id)) # count how many convictions s/he has
    ) %>% 
    dplyr::ungroup() 
  
  b <- a %>% 
    dplyr::group_by(n_convictions) %>% # for each number of convictions
    dplyr::summarize(
      n_people = length(unique(person_id)) # count how many people had this many convictions
    )
  
  persons_with_n_convictions <- a %>% 
    dplyr::filter(n_convictions == n_conv) %>% 
    dplyr::distinct(person_id) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  
  sample_of_persons_with_n_convictions <- persons_with_n_convictions %>% 
    sample(size = sample_size, replace = FALSE )
}

set.seed(42)
sample_id_2 <- ds %>% sample_from_n_conviction(sample_size = 6,n_conv = 2)
sample_id_3 <- ds %>% sample_from_n_conviction(sample_size = 3,n_conv = 3)
sample_id_4 <- ds %>% sample_from_n_conviction(sample_size = 4,n_conv = 4)
sample_id_6 <- ds %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)
sample_id_7 <- ds %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)
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
    ,conviction_id
    ,offense_arrest
    ,offense_group
    # ,month
    # ,year
  )
# ----- compute-variables -------------------------------
# total and cumulative number of convictions
# duration between convictions
# drug offense after 1996


d1 <- ds_sample %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  dplyr::arrange(person_id, begin_date, offense_arrest_cd) %>% 
  dplyr::group_by(person_id) %>% 
  dplyr::mutate(
    conviction_order = row_number()
  ) %>% 
  dplyr::mutate(
    drug_related = ifelse(offense_group == "C", TRUE, NA)
    ,drug_order = cumsum(!is.na(drug_related))
    ,drug_order = ifelse(is.na(drug_related), NA, drug_order)
    ,after_1996   = ifelse(begin_date > "1996-01-01", TRUE, NA)
    ,days_bw_conv = begin_date - dplyr::lag(begin_date,1)
  )



# ----- compute-recedivism ---------------------------
# get_a_sample <- function(d,varname,sample_size, show_all = FALSE){
#   # varname = "offense_arrest_cd"
#   
#   sample_pool <- ds %>% 
#     dplyr::distinct_(.dots = varname) %>% na.omit() %>% 
#     as.list() %>% unlist() %>% as.vector() 
#   if(show_all){ sample_size = length(sample_pool)}
#   selected_sample <- sample_pool %>% sample(size = sample_size, replace = FALSE )
#   
#   return(selected_sample)
# }  
#   # ds %>% get_a_sample("person_id",  5)
#   # ds %>% get_a_sample("offense_arrest_cd",  5, show_all = T) 
# set.seed(42)
# target_sample <- ds %>% 
#   dplyr::filter(n_offenses > 1L) %>% 
#   get_a_sample("person_id", 500)


ds %>% 
  dplyr::filter(person_id %in% target_sample) %>% 
  ggplot(aes(x = begin_date, y = offense_jail_time_days))+
  geom_point()+
  

# subet to people with repeat offenses
ds1 <- ds %>%
  dplyr::group_by(person_id) %>%
  dplyr::summarize( n_offenses = length(unique( begin_date )) ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_offenses > 1L)



ds1 %>% dplyr::glimpse(80)
# a sample of recidivists to work with
sample1 <- sample(ds1$person_id, 10)

ds2 <- ds %>%
  dplyr::filter(person_id %in% get_a_sample(ds1,"person_id",5))
# ---- basic-graph -------------------------------------------------------


# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------


# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/eda-2/eda-2.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



