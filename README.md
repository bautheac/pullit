pullit\!
================

<style> body {text-align: justify} </style>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Although self-contained with consumption value on its own, pullit is one
of the workhorses of the [finRes](https://bautheac.github.io/finRes/)
suite where it plays the very important role of interface to Bloomberg.
Using carefully selected Bloomberg datafields from
[BBGsymbols](https://bautheac.github.io/BBGsymbols/) in tandem with
Armstrong, Eddelbuettel, and Laing (2018)’s Bloomberg interface it makes
pulling financial data from Bloomberg rather easy for the R user.
Install the development version from github with
`devtools::install_github("bautheac/pullit")`.

## finRes

### BBGsymbols

pullit queries Bloomberg for historical as well as contemporaneous data
for multiple types of financial instruments using fields provided by
BBGsymbols and returns the retrieved data in formats that are easy to
work with.

### plotit

finRes provides a set of accessors and summary methods for these objects
while [plotit](https://bautheac.github.io/plotit/) provides bespoke
visualization tools.

### storethat

Financial data retrieved using pullit can be stored using
[storethat](https://bautheac.github.io/storethat/) for subsequent access
with no active Bloomberg connection needed. storethat uses a bespoke
database design for easy and fast access where the data can be stored
locally or remotely for global access, in a cloud for example.
Retrieving previously stored data from a storethat database using pullit
is fairly similar to retrieving it from Bloomberg. The corresponding
functions only differ in name with their prefix referring to the data
source (storethat vs. Bloomberg); the parameters are equal.

## References

<div id="refs" class="references">

<div id="ref-Armstrong_Rblpapi_2018">

Armstrong, Whit, Dirk Eddelbuettel, and John Laing. 2018. *Rblpapi: R
Interface to ’Bloomberg’*. <https://CRAN.R-project.org/package=Rblpapi>.

</div>

</div>
