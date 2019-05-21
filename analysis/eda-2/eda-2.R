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
path_file_input_dto <- "./data-unshared/derived/1-dto.rds"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_file_input_dto)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
ds          <- dto$person_date_offense
ds %>% dplyr::glimpse(100)
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
# How to use
# ds %>% 
#   dplyr::group_by(person_id, offense_arrest_cd) %>% 
#   aggregate_by_groups()

# ---- tweak-1 ---------------------------------------------------------------
ds <- ds %>% 
  # dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% # for testing
  dplyr::group_by(person_id) %>% 
  dplyr::mutate(
    drug_ever         = ifelse( sum(n_drug_related) > 0L, TRUE, FALSE )
    ,drug_after_1996  = ifelse( sum(n_drug_related_after_1996) > 0L, TRUE, FALSE )
    ,drug_before_1996 = ifelse( (drug_ever==TRUE & drug_after_1996==FALSE), TRUE, FALSE )
  )

ds %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
  neat() 

# ---- tweak-2 ---------------------------------------------------------------
# let us aggregate ove (YEAR) and (OFFENSE GROUP)
ds_pivot <- dto$person_date_offense_cd %>% 
  # dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% # for testing
  dplyr::mutate(
    year = lubridate::year(begin_date)
  ) %>% 
  # agg level 1 : person   - date - offense group
  dplyr::group_by(person_id, year, offense_group) %>% 
  aggregate_by_groups() %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(person_id) %>%
  dplyr::mutate(
    drug_ever         = ifelse( sum(n_drug_related) > 0L, TRUE, FALSE )
    ,drug_after_1996  = ifelse( sum(n_drug_related_after_1996) > 0L, TRUE, FALSE )
    ,drug_before_1996 = ifelse( (drug_ever==TRUE & drug_after_1996==FALSE), TRUE, FALSE )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year, offense_group) %>% 
  dplyr::summarize(
    n_offense_counts           = sum(n_offense_counts)
    ,n_convictions             = sum(n_convictions)
    ,n_drug_related            = sum(na.omit(n_drug_related))
    ,n_after_1996              = sum(na.omit(n_after_1996))
    ,n_drug_related_after_1996 = sum(na.omit(n_drug_related_after_1996))
  ) %>% 
  dplyr::ungroup()

ds_pivot %>% 
  neat() 
# ---- basic-pivot -------------------------------------------------------

ds_pivot %>% 
  rpivotTable::rpivotTable(
    # rows = c("", "county")
    # , cols = c("year")
    # , vals = "n_trained"
    # , aggregatorName = "Integer Sum"
    # , rendererName = "Heatmap"
    # , width="100%"
    # , height="400px"
  )

# ---- publish ---------------------------------

path_report_1 <- "./analysis/eda-2/eda-2.Rmd"
path_report_2 <- "./analysis/eda-2/eda-2-pivot.Rmd"


# allReports <- c(path_report_1,path_report_2)
allReports <- c(path_report_1)
# allReports <- c(path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}


# ---- basic-graph -------------------------------------------------------

# ds1 <- dto$person_date_offense_cd %>% 
#   dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% # for testing
#   dplyr::mutate(
#     year = lubridate::year(begin_date)
#   ) %>% 
#   # agg level 1 : person   - date - offense group
#   dplyr::group_by(person_id, year, offense_group) %>% 
#   aggregate_by_groups() %>% 
#   dplyr::ungroup() %>%
#   dplyr::group_by(person_id) %>%
#   dplyr::mutate(
#     drug_ever         = ifelse( sum(n_drug_related) > 0L, TRUE, FALSE )
#     ,drug_after_1996  = ifelse( sum(n_drug_related_after_1996) > 0L, TRUE, FALSE )
#     ,drug_before_1996 = ifelse( (drug_ever==TRUE & drug_after_1996==FALSE), TRUE, FALSE )
#   ) %>% 
#   dplyr::ungroup() 
#  
# ds1 %>% 
#   dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
#   neat(caption = "Grouped by : (PERSON) - (DATE) - (OFFENSE GROUP)")
# 
# small_sample <- ds1 %>% get_a_sample("person_id",500)
# 
# g1 <- ds1 %>% 
#   dplyr::filter(year  %in% 1970:2014) %>% 
#   dplyr::filter(offense_group %in% c("A","B","C","D")) %>% 
#   ggplot2::ggplot(aes(x = year, y = n_people, color = offense_group))+
#   geom_point()+
#   geom_line(aes(group = offense_group))+
#   geom_vline(xintercept = 1996)+
#   theme_minimal() +
#   labs(title = "Number of people with at least 1 conviction")
# 
# t1 %>% 
#   dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% 
#   neat() 

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



