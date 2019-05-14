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
    # ,"offense_begin_date"# date the person began serving the sentence for an individual offense
    ,"offense_count"     # count of offenses in inmate's sentence
    ,"offense_arrest_cd" # code for the offense committed   
    ,"offense_arrest"    # standardized description of the offense committed
    # the rest were computed
    ,"conviction_id"     # person_id + offense_arrest_cd
    ,"year"              # calendar year in which offense tool place
    ,"offense_group"     # one-letter code (we need "C" - drug-related fellonies)
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
  neat()
  # knitr::kable(format = "pandoc")

# ---- explore-1 ------------------------------------------------

# compute the number of convictions and offenses for each person
ds <- ds %>% 
  dplyr::group_by(person_id) %>% 
  dplyr::mutate(
    n_convictions = length(unique(begin_date)) # convictions, unique date
    ,n_offenses   = n()                        # offenses, counts of
  ) %>% 
  dplyr::ungroup()
ds %>% dplyr::glimpse(50)



# frequency distribution of recedivism / how many people for each level?
# anything > 1 = recedivism
t2 <- ds %>% 
  dplyr::group_by(n_convictions ) %>% 
  dplyr::summarize(
    n_people   = length(unique(person_id)) # number of people with that many convictions
  )

t2 %>% 
  ggplot2::ggplot(aes(x = n_convictions, y = n_people))+
  geom_bar(stat = "identity", fill = 'salmon', alha = .5, color = "black")+
  theme_minimal()
# obsevation: most people are first time offenders

# ---- explore-2 ------------------------------------------------
# Q. Distribution of offenses for people with a single conviction:
# For those who have a single convictions, how many people have 1,2,3,... offenses? 
ds %>% 
  dplyr::filter(n_convictions == 1) %>% 
  dplyr::group_by(n_offenses) %>% 
  dplyr::summarise(
    n_people = n()
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(aes(x = n_offenses, y = n_people))+
  geom_bar(stat = "identity",fill = 'aquamarine3', alpha = .5, color = "black")+
  theme_minimal()

# ---- explore-3 ------------------------------------------------

# Q.  Among the people with a single conviction
# For each offense group, how many unique persons were registered
# For each offense group, how many distinct convictions were registered
t3 <- ds %>% 
  dplyr::filter(n_convictions == 1) %>% 
  dplyr::group_by(offense_group) %>% 
  dplyr::summarize(
    # WARNING: people will be double counted because 
    # a single conviction may contain multiple offenses from various offense groups
    n_people = length(unique(person_id)) # ! WARNING 
    ,n_offenses = n()
  ) %>% 
  dplyr::ungroup()


t3 %>% 
  ggplot2::ggplot(aes(x = offense_group  ) ) +
  geom_bar(aes(y = n_offenses),fill = "yellow",color = "black", stat = "identity", alpha = .3)+
  geom_bar(aes(y = n_people),  fill = "blue", stat = "identity", alpha = .6)+
  theme_minimal()

# ---- explore-4 ------------------------------------------------

# Q.  For every offense group, 
# how many individuals had at least one conviction with at leasT one offence in this offense group
t4 <- ds %>% 
  dplyr::group_by(offense_group, year) %>% 
  dplyr::summarize(
    n_people = length(unique(person_id))
  )  
  
t4 %>% 
  dplyr::filter(year  %in% 1970:2014) %>% 
  dplyr::filter(offense_group %in% c("A","B","C","D")) %>% 
  ggplot2::ggplot(aes(x = year, y = n_people, color = offense_group))+
  geom_point()+
  geom_line(aes(group = offense_group))+
  geom_vline(xintercept = 1996)+
  theme_minimal() +
  labs(title = "Number of people with at least one conviction")


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
  input = "./analysis/eda-1/eda-1.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



