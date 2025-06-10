
# Purpose

- The URL to the GitHub (i.e. the source code) is:
  <https://github.com/brebrown6101/UWBiost561>
- The URL to the PkgDown webpage is:
  <file:///Users/breannabrown/Desktop/BIOST561/UWBiost561/docs/index.html>

# How to install

This package is called UWBiost561. To install, run the following code
(in R):

``` r
library(devtools)
devtools::install_github("brebrown6101/UWBiost561")
```

Upon completion, you can run the following code (in R):

``` r
library(UWBiost561)
```

# Session Info

This package was developed in the following environment

``` r
devtools::session_info()
─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.4.1 (2024-06-14)
 os       macOS 15.2
 system   aarch64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       America/Los_Angeles
 date     2025-06-09
 rstudio  2024.12.0+467 Kousa Dogwood (desktop)
 pandoc   3.2 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/ (via rmarkdown)
 quarto   1.5.57 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto

─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────
 ! package     * version    date (UTC) lib source
   bookdown      0.42       2025-01-07 [1] CRAN (R 4.4.1)
   brio          1.1.5      2024-04-24 [1] CRAN (R 4.4.1)
   bslib         0.8.0      2024-07-29 [1] CRAN (R 4.4.0)
   cachem        1.1.0      2024-05-16 [1] CRAN (R 4.4.0)
   callr         3.7.6      2024-03-25 [1] CRAN (R 4.4.0)
   cli           3.6.3      2024-06-21 [1] CRAN (R 4.4.0)
   commonmark    1.9.2      2024-10-04 [1] CRAN (R 4.4.1)
   curl          6.2.2      2025-03-24 [1] CRAN (R 4.4.1)
   desc          1.4.3      2023-12-10 [1] CRAN (R 4.4.1)
   devtools      2.4.5      2022-10-11 [1] CRAN (R 4.4.0)
   digest        0.6.37     2024-08-19 [1] CRAN (R 4.4.1)
   ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.4.1)
   evaluate      1.0.3      2025-01-10 [1] CRAN (R 4.4.1)
   fastmap       1.2.0      2024-05-15 [1] CRAN (R 4.4.0)
   fs            1.6.5      2024-10-30 [1] CRAN (R 4.4.1)
   glue          1.8.0      2024-09-30 [1] CRAN (R 4.4.1)
   htmltools     0.5.8.1    2024-04-04 [1] CRAN (R 4.4.0)
   htmlwidgets   1.6.4      2023-12-06 [1] CRAN (R 4.4.0)
   httpuv        1.6.15     2024-03-26 [1] CRAN (R 4.4.0)
   jquerylib     0.1.4      2021-04-26 [1] CRAN (R 4.4.0)
   jsonlite      1.8.9      2024-09-20 [1] CRAN (R 4.4.1)
   knitr       * 1.49       2024-11-08 [1] CRAN (R 4.4.1)
   later         1.4.1      2024-11-27 [1] CRAN (R 4.4.1)
   lifecycle     1.0.4      2023-11-07 [1] CRAN (R 4.4.0)
   magrittr      2.0.3      2022-03-30 [1] CRAN (R 4.4.0)
   memoise       2.0.1      2021-11-26 [1] CRAN (R 4.4.0)
   mime          0.12       2021-09-28 [1] CRAN (R 4.4.0)
   miniUI        0.1.1.1    2018-05-18 [1] CRAN (R 4.4.0)
   pillar        1.10.1     2025-01-07 [1] CRAN (R 4.4.1)
   pkgbuild      1.4.7      2025-03-24 [1] CRAN (R 4.4.1)
   pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.4.0)
   pkgload       1.4.0      2024-06-28 [1] CRAN (R 4.4.0)
   png           0.1-8      2022-11-29 [1] CRAN (R 4.4.1)
   prettyunits   1.2.0      2023-09-24 [1] CRAN (R 4.4.0)
   processx      3.8.5      2025-01-08 [1] CRAN (R 4.4.1)
   profvis       0.4.0      2024-09-20 [1] CRAN (R 4.4.1)
   promises      1.3.2      2024-11-28 [1] CRAN (R 4.4.1)
   ps            1.8.1      2024-10-28 [1] CRAN (R 4.4.1)
   purrr         1.0.2      2023-08-10 [1] CRAN (R 4.4.0)
   R6            2.5.1      2021-08-19 [1] CRAN (R 4.4.0)
   rcmdcheck     1.4.0      2021-09-27 [1] CRAN (R 4.4.0)
   Rcpp          1.0.14     2025-01-12 [1] CRAN (R 4.4.1)
   remotes       2.5.0      2024-03-17 [1] CRAN (R 4.4.0)
   rlang         1.1.4      2024-06-04 [1] CRAN (R 4.4.0)
   rmarkdown     2.29       2024-11-04 [1] CRAN (R 4.4.1)
   roxygen2      7.3.2      2024-06-28 [1] CRAN (R 4.4.0)
   rprojroot     2.0.4      2023-11-05 [1] CRAN (R 4.4.1)
   rsconnect     1.3.4      2025-01-22 [1] CRAN (R 4.4.1)
   rstudioapi    0.17.1     2024-10-22 [1] CRAN (R 4.4.1)
   sass          0.4.9      2024-03-15 [1] CRAN (R 4.4.0)
   sessioninfo   1.2.3      2025-02-05 [1] CRAN (R 4.4.1)
   shiny         1.10.0     2024-12-14 [1] CRAN (R 4.4.1)
   stringi       1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
   stringr       1.5.1      2023-11-14 [1] CRAN (R 4.4.0)
   testthat    * 3.2.3      2025-01-13 [1] CRAN (R 4.4.1)
   tibble        3.2.1      2023-03-20 [1] CRAN (R 4.4.0)
   urlchecker    1.0.1      2021-11-30 [1] CRAN (R 4.4.1)
   usethis       3.1.0      2024-11-26 [1] CRAN (R 4.4.1)
 P UWBiost561  * 0.0.0.9000 2025-06-06 [?] load_all()
   vctrs         0.6.5      2023-12-01 [1] CRAN (R 4.4.0)
   waldo         0.6.1      2024-11-07 [1] CRAN (R 4.4.1)
   withr         3.0.2      2024-10-28 [1] CRAN (R 4.4.1)
   xfun          0.50       2025-01-07 [1] CRAN (R 4.4.1)
   xml2          1.3.6      2023-12-04 [1] CRAN (R 4.4.0)
   xopen         1.0.1      2024-04-25 [1] CRAN (R 4.4.0)
   xtable        1.8-4      2019-04-21 [1] CRAN (R 4.4.1)
   yaml          2.3.10     2024-07-26 [1] CRAN (R 4.4.0)

 [1] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
```
