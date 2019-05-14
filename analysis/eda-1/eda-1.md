---
title: "EDA 1: Basic Questions"
output:
  html_document:
    always_allow_html: yes
    df_print: kable
    highlight: tango
    keep_md: yes
    theme: spacelab
    toc: yes
    toc_float: yes
  pdf_document: 
    highlight: pygments
    toc: yes
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->



# I. Exposition 

This report begins analytic inquiry of the data stored in the `data transfer object` (`dto`) prepared by the [`./manipulation/0-greeter.R`][greeter_html_path] script. 

[greeter_html_path]:https://raw.githack.com/dss-hmi/drug-recedivism-2019/master/analysis/0-greeter/0-greeter.html

<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 

```r
# Attach these packages so their functions don't need to be qualified
library(magrittr)          # pipes
library(ggplot2)           # graphing
# for other packages, please use qualifiers (e.g. dplyr::select() )
requireNamespace("dplyr")    # data wrangling
requireNamespace("ggpubr")   # documents
```


<!-- Load the sources.  Suppress the output when loading sources. --> 

```r
# import custom functions and scripts
base::source("./scripts/common-functions.R")
```


<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
# path to the data source
path_file_input_dto <- "./data-unshared/derived/0-dto.rds"
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


## I. Data
<!-- Load the datasets.   -->

```r
dto <- readRDS(path_file_input_dto)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()
```

```
82.4 MB
```

```
[1] "list"
```

```
[1] "sentence" "codebook" "inmate"  
```

```r
# assign aliases for this report
ds          <- dto$sentence
ds_codebook <- dto$codebook
```

<!-- Inspect the datasets.   -->


<!-- Tweak the datasets.   -->

```r
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
```

```
Observations: 120,381
Variables: 8
$ person_id         <dbl> 44473, 44473, 54131...
$ begin_date        <dttm> 2000-05-09, 1993-0...
$ offense_count     <dbl> 1, 1, 1, 1, 1, 1, 2...
$ offense_arrest_cd <chr> "C31", "D21", "B26"...
$ offense_arrest    <chr> "MANU/DIST/DEL/DISP...
$ conviction_id     <chr> "44473-C31", "44473...
$ year              <dbl> 2000, 1993, 2000, 2...
$ offense_group     <chr> "C", "D", "B", "C",...
```

```r
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
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> field_name </th>
   <th style="text-align:left;"> description </th>
   <th style="text-align:left;"> sample_data </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> offense_arrest_cd </td>
   <td style="text-align:left;"> code for the offense committed </td>
   <td style="text-align:left;"> E01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> offense_arrest </td>
   <td style="text-align:left;"> standardized description of the offense committed </td>
   <td style="text-align:left;"> FORGERY 1ST DEGREE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> begin_date </td>
   <td style="text-align:left;"> date the person began serving the  aggregate sentence </td>
   <td style="text-align:left;"> 27296 </td>
  </tr>
</tbody>
</table>

```r
  # knitr::kable(format = "pandoc")
```



<!-- Basic table view.   -->


<!-- Basic graph view.   -->






# II. Development

## 1

```r
# compute the number of convictions and offenses for each person
ds <- ds %>% 
  dplyr::group_by(person_id) %>% 
  dplyr::mutate(
    n_convictions = length(unique(begin_date)) # convictions, unique date
    ,n_offenses   = n()                        # offenses, counts of
  ) %>% 
  dplyr::ungroup()
ds %>% dplyr::glimpse(50)
```

```
Observations: 120,381
Variables: 11
$ person_id         <dbl> 44473, 44473, 54131...
$ begin_date        <date> 2000-05-09, 1993-0...
$ offense_count     <dbl> 1, 1, 1, 1, 1, 1, 2...
$ offense_arrest_cd <chr> "C31", "D21", "B26"...
$ offense_arrest    <chr> "MANU/DIST/DEL/DISP...
$ conviction_id     <chr> "44473-C31", "44473...
$ year              <dbl> 2000, 1993, 2000, 2...
$ offense_group     <chr> "C", "D", "B", "C",...
$ month             <dbl> 5, 5, 8, 8, 1, 3, 3...
$ n_convictions     <int> 2, 2, 1, 1, 1, 1, 1...
$ n_offenses        <int> 2, 2, 1, 1, 1, 3, 3...
```

```r
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
```

```
Warning: Ignoring unknown parameters: alha
```

<img src="figure_rmd/explore-1-1.png" width="900px" />

```r
# obsevation: most people are first time offenders
```

## 2


```r
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
```

<img src="figure_rmd/explore-2-1.png" width="900px" />

## 3

```r
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
```

<img src="figure_rmd/explore-3-1.png" width="900px" />

## 4

```r
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
```

<img src="figure_rmd/explore-4-1.png" width="900px" />



Session Information {#session-info}
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.5.2 (2018-12-20)
 os       Windows >= 8 x64            
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/Los_Angeles         
 date     2019-05-14                  

- Packages -----------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.5.3)
 backports     1.1.4   2019-04-10 [1] CRAN (R 3.5.3)
 callr         3.2.0   2019-03-15 [1] CRAN (R 3.5.3)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.3)
 cli           1.1.0   2019-03-19 [1] CRAN (R 3.5.3)
 codetools     0.2-15  2016-10-05 [2] CRAN (R 3.5.2)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.5.3)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.3)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.3)
 devtools      2.0.2   2019-04-08 [1] CRAN (R 3.5.3)
 digest        0.6.18  2018-10-10 [1] CRAN (R 3.5.3)
 dplyr         0.8.0.1 2019-02-15 [1] CRAN (R 3.5.3)
 evaluate      0.13    2019-02-12 [1] CRAN (R 3.5.3)
 fansi         0.4.0   2018-10-05 [1] CRAN (R 3.5.3)
 fs            1.3.1   2019-05-06 [1] CRAN (R 3.5.3)
 ggplot2     * 3.1.1   2019-04-07 [1] CRAN (R 3.5.3)
 ggpubr        0.2     2018-11-15 [1] CRAN (R 3.5.3)
 glue          1.3.1   2019-03-12 [1] CRAN (R 3.5.3)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.5.3)
 highr         0.8     2019-03-20 [1] CRAN (R 3.5.3)
 hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.3)
 htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.3)
 httr          1.4.0   2018-12-11 [1] CRAN (R 3.5.3)
 kableExtra    1.1.0   2019-03-16 [1] CRAN (R 3.5.3)
 knitr       * 1.22    2019-03-08 [1] CRAN (R 3.5.3)
 labeling      0.3     2014-08-23 [1] CRAN (R 3.5.2)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.5.3)
 lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.5.3)
 magrittr    * 1.5     2014-11-22 [1] CRAN (R 3.5.3)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.3)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.3)
 pillar        1.3.1   2018-12-15 [1] CRAN (R 3.5.3)
 pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.5.3)
 pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.3)
 pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.3)
 plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.3)
 prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.3)
 processx      3.3.1   2019-05-08 [1] CRAN (R 3.5.2)
 pryr          0.1.4   2018-02-18 [1] CRAN (R 3.5.3)
 ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.3)
 purrr         0.3.2   2019-03-15 [1] CRAN (R 3.5.3)
 R6            2.4.0   2019-02-14 [1] CRAN (R 3.5.3)
 Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.5.3)
 readr         1.3.1   2018-12-21 [1] CRAN (R 3.5.3)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 3.5.3)
 remotes       2.0.4   2019-04-10 [1] CRAN (R 3.5.3)
 rlang         0.3.4   2019-04-07 [1] CRAN (R 3.5.3)
 rmarkdown     1.12    2019-03-14 [1] CRAN (R 3.5.3)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.3)
 rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.5.3)
 rvest         0.3.3   2019-04-11 [1] CRAN (R 3.5.3)
 scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)
 stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 3.5.3)
 tibble        2.1.1   2019-03-16 [1] CRAN (R 3.5.3)
 tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.3)
 usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.5.3)
 viridisLite   0.3.0   2018-02-01 [1] CRAN (R 3.5.3)
 webshot       0.5.1   2018-09-28 [1] CRAN (R 3.5.3)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.3)
 xfun          0.6     2019-04-02 [1] CRAN (R 3.5.3)
 xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.3)
 yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)

[1] C:/Users/an499583/Documents/R/win-library/3.5
[2] C:/Program Files/R/R-3.5.2/library
```
</details>



Report rendered by an499583 at 2019-05-14, 10:36 -0700 in 9 seconds.


