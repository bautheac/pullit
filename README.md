pullit\!
================

<style> body {text-align: justify} </style>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Although self-contained with consumption value on its own,
[pullit](https://bautheac.github.io/pullit/) is one of the workhorses
for data ETL (extract, transform, load) work in the
[finRes](https://bautheac.github.io/finRes/) suite where it plays the
very important role of interface to Bloomberg. Using carefully selected
Bloomberg datafields from
[BBGsymbols](https://bautheac.github.io/BBGsymbols/) in tandem with
Armstrong, Eddelbuettel, and Laing (2021)’s Bloomberg interface it
facilitates Bloomberg data queries for the R user.  
Install the development version from github with
`devtools::install_github("bautheac/pullit")`.

## finRes

### BBGsymbols

[pullit](https://bautheac.github.io/pullit/) queries Bloomberg for
historical as well as contemporaneous data for multiple types of
financial instruments using fields provided by
[BBGsymbols](https://bautheac.github.io/BBGsymbols/) and returns the
retrieved data in formats that are easy to work with.

### plotit

[finRes](https://bautheac.github.io/finRes/) provides a set of accessors
and summary methods for these objects while
[plotit](https://bautheac.github.io/plotit/) provides bespoke
visualization tools.

### storethat

Financial data retrieved using
[pullit](https://bautheac.github.io/pullit/) can be stored using the
[storethat](https://bautheac.github.io/storethat/) package for
subsequent access with no active Bloomberg connection needed. Retrieving
previously stored data from a
[storethat](https://bautheac.github.io/storethat/) database using
[pullit](https://bautheac.github.io/pullit/) is fairly similar to
retrieving it from Bloomberg. The corresponding functions only differ in
name with their prefix referring to the data source (storethat
vs. Bloomberg); the parameters are equal.

## References

<div id="refs" class="references">

<div id="ref-Armstrong_Rblpapi">

Armstrong, Whit, Dirk Eddelbuettel, and John Laing. 2021. *Rblpapi: R
Interface to ’Bloomberg’*. <https://CRAN.R-project.org/package=Rblpapi>.

</div>

</div>
