Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2025/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages).

``` r
# Install packages if they are not already installed
packages <- c("data.table", "dtplyr", "dplyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

# Load the required libraries
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

2.  Load the met data from
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>,
    and also the station data. For the latter, you can use the code we
    used during lecture to pre-process the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data
met <- data.table::fread("met_all.gz")
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
```

## Question 1: Representative station for the US

Across all weather stations, what stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# medians across all stations and times
medians <- met[, .(
  temp_50 = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = .5, na.rm = TRUE)
)]

medians
```

    ##    temp_50 wind.sp_50 atm.press_50
    ##      <num>      <num>        <num>
    ## 1:    23.5        2.1       1014.1

2)  

``` r
# medians by station (keep state)
station_med <- met[, .(
  temp = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press = quantile(atm.press, probs = .5, na.rm = TRUE)
), by = .(USAFID, STATE)]
```

3)  

``` r
# Find the stations that are the closest to the overall medians

# Median temperature stations
station_med[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_med[temp_dist == 0]
median_temp_station
```

    ##    USAFID  STATE  temp wind.sp atm.press temp_dist
    ##     <int> <char> <num>   <num>     <num>     <num>
    ## 1: 720501     VA  23.5     1.5        NA         0
    ## 2: 722031     AL  23.5     0.0        NA         0
    ## 3: 722148     NC  23.5     0.0        NA         0
    ## 4: 723055     NC  23.5     0.0        NA         0
    ## 5: 723067     NC  23.5     1.5        NA         0
    ## 6: 723177     NC  23.5     0.0        NA         0
    ## 7: 725564     NE  23.5     2.6        NA         0

``` r
# Median wind speed stations
station_med[, wind.sp_dist := abs(wind.sp - medians$wind.sp_50)]
median_wind.sp_station <- station_med[wind.sp_dist == 0]
median_wind.sp_station
```

    ##      USAFID  STATE  temp wind.sp atm.press temp_dist wind.sp_dist
    ##       <int> <char> <num>   <num>     <num>     <num>        <num>
    ##   1: 720110     TX  31.0     2.1        NA       7.5            0
    ##   2: 720258     MN  17.0     2.1        NA       6.5            0
    ##   3: 720266     IN  21.0     2.1        NA       2.5            0
    ##   4: 720272     WA  18.0     2.1        NA       5.5            0
    ##   5: 720273     TX  28.6     2.1        NA       5.1            0
    ##  ---                                                             
    ## 339: 726583     MN  21.0     2.1        NA       2.5            0
    ## 340: 726589     MN  20.0     2.1        NA       3.5            0
    ## 341: 726603     MN  20.7     2.1        NA       2.8            0
    ## 342: 726626     WI  16.6     2.1        NA       6.9            0
    ## 343: 726813     ID  22.8     2.1   1011.75       0.7            0

``` r
# Median atmospheric pressure stations
station_med[, atm.press_dist := abs(atm.press - medians$atm.press_50)]
median_atm.press_station <- station_med[atm.press_dist == 0]
median_atm.press_station
```

    ##     USAFID  STATE  temp wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##      <int> <char> <num>   <num>     <num>     <num>        <num>          <num>
    ##  1: 722420     TX  30.0     4.6    1014.1       6.5          2.5              0
    ##  2: 723830     CA  23.3     5.1    1014.1       0.2          3.0              0
    ##  3: 724885     NV  24.7     2.6    1014.1       1.2          0.5              0
    ##  4: 724940     CA  18.9     5.1    1014.1       4.6          3.0              0
    ##  5: 725376     MI  22.8     3.1    1014.1       0.7          1.0              0
    ##  6: 725975     OR  16.1     2.1    1014.1       7.4          0.0              0
    ##  7: 726183     ME  18.9     0.0    1014.1       4.6          2.1              0
    ##  8: 726375     MI  21.1     3.1    1014.1       2.4          1.0              0
    ##  9: 726579     MN  20.0     3.1    1014.1       3.5          1.0              0
    ## 10: 726584     MN  20.0     3.1    1014.1       3.5          1.0              0
    ## 11: 726590     SD  20.0     3.1    1014.1       3.5          1.0              0

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all ~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.
