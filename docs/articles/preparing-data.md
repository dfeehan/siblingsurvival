# Preparing DHS data

``` r
library(siblingsurvival)
library(tidyverse)
#> Warning: package 'tibble' was built under R version 4.4.3
#> Warning: package 'purrr' was built under R version 4.4.3
#> Warning: package 'dplyr' was built under R version 4.4.3
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.0     ✔ readr     2.1.5
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.0     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# this is helfpul for timing
library(tictoc)
```

### Overview

This vignette shows how to prepare a DHS-style dataset for analysis with
the `siblingsurvival` model.

First, we’ll open up the model DHS dataset, which is available
[here](https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZIR62DT.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs);
see also [this
webpage](https://dhsprogram.com/data/Download-Model-Datasets.cfm). We’re
going to be using the **Individual Recode**, and, generally, it’s
easiest to start using the Stata-formatted version (neither an R
version, nor an easy-to-use plain text format like .csv is provided).

We’ll assume that we’ve read the individual recode version of the
dataset into R using
[`haven::read_dta`](https://haven.tidyverse.org/reference/read_dta.html),
and that we’re calling it `model_dhs_dat`.

``` r
data(model_dhs_dat)
```

Next, we’ll open up the `varmap`, which maps column names in the raw DHS
data to more user-friendly names.

An up-to-date list of DHS versions and recodes is available
[here](https://www.dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm).

``` r
data(sibhist_varmap_dhs6)
```

Using the `varmap`, get a vector with the names of the respondent (ego)
variables

``` r
## ego (respondent) variables to grab
tmp <- subset(sibhist_varmap_dhs6, sibvar==0)
resp.attrib <- tmp$orig.varname
names(resp.attrib) <- tmp$new.varname
resp.attrib
#>           caseid           survey          cluster            hhnum 
#>         "caseid"           "v000"           "v001"           "v002" 
#>            hrnum             wwgt             year              doi 
#>           "v003"           "v005"           "v007"           "v008" 
#>              age             age5              psu stratum_analysis 
#>           "v012"           "v013"           "v021"           "v022" 
#>   stratum_design           region       ruralurban             educ 
#>           "v023"           "v024"           "v025"           "v106" 
#>            relig        ethnicity           hhsize      educ_recode 
#>           "v130"           "v131"           "v136"           "v149" 
#>         literacy    mm_occurences     mm_preceding 
#>           "v155"           "mmc1"           "mmc2"
```

Using the `varmap`, get a vector with the names of the sibling variables

``` r
## alter (sibling) variables to grab
tmp <- subset(sibhist_varmap_dhs6, sibvar==1)
sib.attrib <- tmp$orig.varname
names(sib.attrib) <- tmp$new.varname
sib.attrib
#>                sibindex                 sib.sex               sib.alive 
#>                 "mmidx"                   "mm1"                   "mm2" 
#>                 sib.age                 sib.dob      sib.marital.status 
#>                   "mm3"                   "mm4"                   "mm5" 
#>        sib.death.yrsago           sib.death.age          sib.death.date 
#>                   "mm6"                   "mm7"                   "mm8" 
#>       sib.died.pregnant   sib.died.bc.pregnancy         sib.death.cause 
#>                   "mm9"                  "mm10"                  "mm11" 
#> sib.time.delivery.death         sib.place.death        sib.num.children 
#>                  "mm12"                  "mm13"                  "mm14" 
#>          sib.death.year       sib.died.accident 
#>                  "mm15"                  "mm16"
```

We’ll need to prepare two different datasets: the first dataset has
information about the women who responded to the survey; we’ll call this
the respondent or ego dataset. The second dataset has information about
the reported siblings; we’ll call this the sibling dataset.

### Prepare the respondent (ego) dataset

``` r
ex.ego <- model_dhs_dat %>% 
  # use information from the varmap to rename ego variables
  rename(!!!resp.attrib) %>%
  mutate(
    ## typically, only women are asked sibling histories in DHS surveys
    sex='f',
    ## these breaks should agree with the ones
    ## used in the age categories for siblings
    ## (see below)
    age.cat=fct_drop(cut(age,
      breaks=c(0, seq(from=15,to=50,by=5),95),
      include.lowest=TRUE, right=FALSE)),
    ## we'll rescale the weights, dividing them by 1,000,000 
    ## (so that their average is 1); see DHS documentation
    wwgt=wwgt/1e6
  ) 

ex.ego %>% head()
#> # A tibble: 6 × 4,277
#>   caseid    survey cluster hhnum hrnum  v004  wwgt  v006  year   doi  v009  v010
#>   <chr>     <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 "       … ZZ6          1     1     2     1  1.06     6  2015  1386     6  1985
#> 2 "       … ZZ6          1     3     2     1  1.06     6  2015  1386     4  1993
#> 3 "       … ZZ6          1     4     2     1  1.06     6  2015  1386     4  1973
#> 4 "       … ZZ6          1     4     3     1  1.06     6  2015  1386     7  1989
#> 5 "       … ZZ6          1     5     1     1  1.06     6  2015  1386     2  1990
#> 6 "       … ZZ6          1     6     2     1  1.06     6  2015  1386     2  1978
#> # ℹ 4,265 more variables: v011 <dbl>, age <dbl>, age5 <dbl>, v014 <dbl>,
#> #   v015 <dbl>, v016 <dbl>, v017 <dbl>, v018 <dbl>, v019 <dbl>, v019a <dbl>,
#> #   v020 <dbl>, psu <dbl>, stratum_analysis <dbl>, stratum_design <dbl>,
#> #   region <dbl>, ruralurban <dbl>, v026 <dbl>, v027 <dbl>, v028 <dbl>,
#> #   v029 <dbl>, v030 <dbl>, v031 <dbl>, v032 <dbl>, v034 <dbl>, v040 <dbl>,
#> #   v042 <dbl>, v044 <dbl>, v101 <dbl>, v102 <dbl>, v103 <dbl>, v104 <dbl>,
#> #   v105 <dbl>, educ <dbl>, v107 <dbl>, v113 <dbl>, v115 <dbl>, v116 <dbl>, …
```

### Prepare the sibling reports dataset

Next, we’ll prepare the dataset of sibling reports.

We’ll start by using `attributes.to.long` to convert the wide-form
sibling data into long format for analysis:

``` r
sibdata <- siblingsurvival::attributes.to.long(ex.ego,
                                               attribute.prefix=sib.attrib,
                                               ego.vars=c('caseid', 'wwgt', 
                                                          'psu', 'doi'),
                                               idvar="caseid")
glimpse(sibdata)
#> Rows: 35,082
#> Columns: 22
#> $ .tmpid                  <chr> "        1  1  2", "        1  3  2", "       …
#> $ caseid                  <chr> "        1  1  2", "        1  3  2", "       …
#> $ wwgt                    <dbl> 1.057703, 1.057703, 1.057703, 1.057703, 1.0577…
#> $ psu                     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ doi                     <dbl> 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386…
#> $ sibindex                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ sib.sex                 <dbl> 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1…
#> $ sib.alive               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1…
#> $ sib.age                 <dbl> 42, 27, 46, 33, 40, 49, 22, 30, 49, 50, 35, NA…
#> $ sib.dob                 <dbl> 876, 1056, 828, 984, 900, 792, 1116, 1020, 792…
#> $ sib.marital.status      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.death.yrsago        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3,…
#> $ sib.death.age           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 42…
#> $ sib.death.date          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 13…
#> $ sib.died.pregnant       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.died.bc.pregnancy   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.death.cause         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.time.delivery.death <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.place.death         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.num.children        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sib.death.year          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ alternum                <chr> "01", "01", "01", "01", "01", "01", "01", "01"…
```

Now we’ll do some recoding.

Turn the `sib.sex` variable into more readable values:

``` r
sibdata.coded <- sibdata %>%
  mutate(sib.sex = ifelse(sib.sex == 2, 'f', 'm'))
```

Next, we need to create a new variable, `sib.endobs`, which has the date
(as a century month code, CMC) when we stopped observing each sibling.
Generally, this date is either the time that the sibling died, or the
date of the interview.

``` r
## make the assumption that
##  (1) sibs who died lived all the way through
##      the month in which they are reported to have died
##  (2) the interview took place on the first of the month
## these assumptions are necessary to get these tables to line up
## with the DHS reports
sibdata.coded$sib.endobs <- pmin(sibdata.coded$doi,
                                 sibdata.coded$sib.death.date + 1,
                                 na.rm=TRUE)
```

For siblings who are currently alive, we’ll recode their date of death
to -1; this ensures that we count their exposure, but not their deaths
(since they are alive).

``` r
## siblings who haven't died get their death dates
## recoded to -1 so we don't lose the exposures they
## contribute...
sibdata.coded$sib.death.date[ is.na(sibdata.coded$sib.death.date) ] <- -1
```

We’ll also create a unique id for each reported sibling.

``` r
sibdata.coded$sibid <- 1:nrow(sibdata)
```

Now we’ll do some quality checks, removing some sibling reports when
there is not enough information to make use of them.

First, we’ll figure out how many siblings have unknown survival status,
and we’ll drop these.

``` r

## CALCULATE % of siblings with unknown survival status;
## we are taking them out of the analysis here
sibdata.coded %>% 
  group_by(sib.alive) %>% 
  summarise(freq=n()) %>% 
  mutate(frac=freq/sum(freq))
#> # A tibble: 4 × 3
#>   sib.alive  freq     frac
#>       <dbl> <int>    <dbl>
#> 1         0  5314 0.151   
#> 2         1 29130 0.830   
#> 3         8     9 0.000257
#> 4        NA   629 0.0179

pre.n <- nrow(sibdata.coded)
sibdata.coded <- sibdata.coded %>% filter(sib.alive %in% c(0,1))
post.n <- nrow(sibdata.coded)

cat(paste0("Removed ", pre.n-post.n, " out of ", pre.n, " (", round(100*(pre.n-post.n)/pre.n,2), "%)",
           " reports about sibs with unknown survival status.\n"))
#> Removed 638 out of 35082 (1.82%) reports about sibs with unknown survival status.
```

Next, we’ll figure out how many siblings have unknown sex, and we’ll
drop them, too.

``` r

## CALCULATE % of siblings with unknown sex
sibdata.coded %>% 
  group_by(sib.sex) %>% 
  summarise(freq=n()) %>% 
  mutate(frac=freq/sum(freq))
#> # A tibble: 3 × 3
#>   sib.sex  freq     frac
#>   <chr>   <int>    <dbl>
#> 1 f       17221 0.500   
#> 2 m       17219 0.500   
#> 3 NA          4 0.000116

## we're removing them from the analysis here
pre.n <- nrow(sibdata.coded)
sibdata.coded <- sibdata.coded %>% filter(sib.sex %in% c('f', 'm'))
post.n <- nrow(sibdata.coded)

cat(paste0("Removed ", pre.n-post.n, " out of ", pre.n, " (", round(100*(pre.n-post.n)/pre.n,2), "%)",
           " reports about sibs with unknown sex.\n"))
#> Removed 4 out of 34444 (0.01%) reports about sibs with unknown sex.

ex.sib <- sibdata.coded
```

We’ll only keep a small subset of the ego variables, to keep things
manageable

``` r
ex.ego <- ex.ego %>% select(!!!c(names(resp.attrib), 'sex', 'age.cat'))
```

### Done!

Now our two datasets, `ex.ego` and `ex.sib` are prepped and ready to go!

## Using `prep_dhs_sib_histories`

The package includes a function, `prep_dhs_sib_histories`, that can be
used to load the sibling data, following the steps we just described. It
works like this:

``` r
data(model_dhs_dat)
data(sibhist_varmap_dhs6)

prepped <- prep_dhs_sib_histories(model_dhs_dat,
                                  varmap = sibhist_varmap_dhs6,
                                  keep_missing = FALSE)
#> 
#> No information on respondent sex given; assuming all respondents are female.
#> 
#> Found wwgt column; assuming we have a DHS survey and scaling weights.
#> 638 out of 35082 (1.82%) reports about sibs have unknown survival status.
#> 602 out of 35082 (1.72%) reports about sibs have unknown sex.
#> Removing reported sibs missing survival status or sex.
#> ... this removes  642  out of  35082  ( 1.83 %)  sibling reports.

str(prepped, 1)
#> List of 4
#>  $ survey : chr "ZZ6"
#>  $ ego.dat: tibble [8,348 × 4,278] (S3: tbl_df/tbl/data.frame)
#>  $ sib.dat:'data.frame': 34440 obs. of  25 variables:
#>  $ summ   : tibble [1 × 11] (S3: tbl_df/tbl/data.frame)
```
