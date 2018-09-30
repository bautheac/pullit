[![Travis-CI Build Status](https://travis-ci.org/bautheac/pullit.svg?branch=master)](https://travis-ci.org/bautheac/pullit)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/pullit?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/pullit)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)


# pullit!

Pulling financial data from Bloomberg in R made easy.


## Installation

pullit! velongs to the [finRes](https://bautheac.github.io/finRes/) suite, install the development version from [github](https://github.com/bautheac/finRes/) with `devtools::install_github(repo = "finRes", username = "bautheac")` or install pullit! individually with `devtools::install_github(repo = "pullit", username = "bautheac")`.



## Example

Futures term structure historical market data from Bloomberg:

``` r
library(finRes)

term_structure <- BBG_futures_market(type = 'term structure', 
  active_contract_tickers = c("W A Comdty", "KWA Comdty"), 
  start = "2017-01-01", end = as.character(Sys.Date()), 
  TS_positions = 1L:5L, 
  roll_type = "A", roll_days = 0L,  roll_months = 0L, roll_adjustment = "N")

get_active_contract_tickers(term_structure)
get_term_structure_tickers(term_structure)
get_data(term_structure)
get_call(term_structure)

plot_term_structure(object = term_structure, ticker = "W A Comdty", 
  frame = 100L)


```

