
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mripr

<!-- badges: start -->
<!-- badges: end -->

The goal of mripr is to provide a convenient set of tools for
downloading and using
[MRIP](https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads)
data. The package will provide the following functions:

-   [ ] Download MRIP data from the web
-   [ ] Load the downloaded data, which also checks [this
    page](https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/SAS/)
    to see if any data has been modified since it was last downloaded,
    in which case the updated version is downloaded before proceeding.
-   [ ] Calculate catch and directed trip estimates, comparable to those
    frin the [web
    query](https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
-   [ ] Calculate custom catch estimates using custom survey domains

## Installation

You can install the development version of mripr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zandergordan/mripr")
```

## Example

This call to the `calculate_catch()` function uses the 2018 `.csv` files
which are bundled with the package to replicate the output from the MRIP
catch time series query tool with the following options: - `2018` to
`2018` - `by wave` - `West Florida` - `GAG` - `all modes combined` -
`all areas combined` - `total catch`

``` r
library(mripr)

a <- calculate_catch(intdir = system.file("extdata", package = "mripr"),
                     common = 'GAG',
                     st = 12,
                     styr = 2018,
                     endyr = 2018,
                     dom = list(mode_fx = list(c(1,2,3,4,5,7)),
                                area_x = list(c(1,2,3,4,5)),
                                sub_reg = list(c(7)))
)
a[c("Domain", "total.catch")]
#>                                         Domain total.catch
#> 2  year2018wave1st12sub_reg1mode_fx1area_x1gag    263833.7
#> 4  year2018wave2st12sub_reg1mode_fx1area_x1gag    451789.1
#> 6  year2018wave3st12sub_reg1mode_fx1area_x1gag    738269.2
#> 8  year2018wave4st12sub_reg1mode_fx1area_x1gag    422932.9
#> 10 year2018wave5st12sub_reg1mode_fx1area_x1gag    181018.9
#> 12 year2018wave6st12sub_reg1mode_fx1area_x1gag    512648.0
```
