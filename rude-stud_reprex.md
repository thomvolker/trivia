``` r
library(MASS)
library(blavaan)
#> Loading required package: lavaan
#> This is lavaan 0.6-8
#> lavaan is FREE software! Please report any bugs.
#> Loading required package: Rcpp
#> Loading required package: RcppParallel
#> 
#> Attaching package: 'RcppParallel'
#> The following object is masked from 'package:Rcpp':
#> 
#>     LdFlags
#> This is blavaan 0.3-17
#> On multicore systems, we suggest use of future::plan("multiprocess") for faster post-MCMC computations.
library(furrr)
#> Loading required package: future
library(tictoc)

tic()

set.seed(123)

dat <- mvrnorm(n = 1000, 
               mu = c(0,0,0), 
               Sigma = matrix(c(1, 0.2, 0.2,
                                0.2, 1, 0.2, 
                                0.2, 0.2, 1),
                              nrow = 3))

dat <- as.data.frame(dat)

model <- list(H1 = "V1 ~~ V2; V1 ~~ V3",
              H0 = "V1 ~~ a*V2; V1 ~~ a*V3")

plan(multisession)

mod_out <- future_map(model, ~bsem(model = .x, data = dat),
                      .options = furrr_options(seed = as.integer(123)))
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.002217 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 22.17 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 8.64203 seconds (Warm-up)
#> Chain 1:                15.7488 seconds (Sampling)
#> Chain 1:                24.3908 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.001687 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 16.87 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 8.14083 seconds (Warm-up)
#> Chain 2:                15.4894 seconds (Sampling)
#> Chain 2:                23.6302 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 0.001677 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 16.77 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 3: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 3: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 3: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 3: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 3: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 3: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 3: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 3: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 3: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 3: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 3: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 8.29754 seconds (Warm-up)
#> Chain 3:                16.2411 seconds (Sampling)
#> Chain 3:                24.5386 seconds (Total)
#> Chain 3: 
#> Computing posterior predictives...
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.002566 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 25.66 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 7.73224 seconds (Warm-up)
#> Chain 1:                15.162 seconds (Sampling)
#> Chain 1:                22.8942 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.002782 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 27.82 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 7.50776 seconds (Warm-up)
#> Chain 2:                15.7878 seconds (Sampling)
#> Chain 2:                23.2956 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'stanmarg' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 0.001712 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 17.12 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 3: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 3: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 3: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 3: Iteration:  501 / 1500 [ 33%]  (Sampling)
#> Chain 3: Iteration:  650 / 1500 [ 43%]  (Sampling)
#> Chain 3: Iteration:  800 / 1500 [ 53%]  (Sampling)
#> Chain 3: Iteration:  950 / 1500 [ 63%]  (Sampling)
#> Chain 3: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 3: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 3: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 3: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 7.52968 seconds (Warm-up)
#> Chain 3:                14.7636 seconds (Sampling)
#> Chain 3:                22.2933 seconds (Total)
#> Chain 3: 
#> Computing posterior predictives...

blavCompare(mod_out$H1, mod_out$H0)
#> 
#> WAIC estimates: 
#>  object1:  8431.322 
#>  object2:  8430.39 
#> 
#> WAIC difference & SE: 
#>    -0.466    1.057 
#> 
#> LOO estimates: 
#>  object1:  8431.343 
#>  object2:  8430.407 
#> 
#> LOO difference & SE: 
#>    -0.468    1.057 
#> 
#> Laplace approximation to the log-Bayes factor
#> (experimental; positive values favor object1):   -1.490

toc()
#> 1752.949 sec elapsed
```

<sup>Created on 2021-08-27 by the [reprex package](https://reprex.tidyverse.org) (v2.0.0)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.1.0 (2021-05-18)
#>  os       macOS Big Sur 10.16         
#>  system   x86_64, darwin17.0          
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Europe/Amsterdam            
#>  date     2021-08-27                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version    date       lib source                             
#>  assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.1.0)                     
#>  bayesplot      1.8.1      2021-06-14 [1] CRAN (R 4.1.0)                     
#>  blavaan      * 0.3-17     2021-07-19 [1] CRAN (R 4.1.0)                     
#>  callr          3.7.0      2021-04-20 [1] CRAN (R 4.1.0)                     
#>  cli            3.0.0      2021-06-30 [1] CRAN (R 4.1.0)                     
#>  coda           0.19-4     2020-09-30 [1] CRAN (R 4.1.0)                     
#>  codetools      0.2-18     2020-11-04 [1] CRAN (R 4.1.0)                     
#>  colorspace     2.0-2      2021-06-24 [1] CRAN (R 4.1.0)                     
#>  CompQuadForm   1.4.3      2017-04-12 [1] CRAN (R 4.1.0)                     
#>  conquer        1.0.2      2020-08-27 [1] CRAN (R 4.1.0)                     
#>  crayon         1.4.1      2021-02-08 [1] CRAN (R 4.1.0)                     
#>  curl           4.3.2      2021-06-23 [1] CRAN (R 4.1.0)                     
#>  DBI            1.1.1      2021-01-15 [1] CRAN (R 4.1.0)                     
#>  digest         0.6.27     2020-10-24 [1] CRAN (R 4.1.0)                     
#>  dplyr          1.0.7      2021-06-18 [1] CRAN (R 4.1.0)                     
#>  ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.1.0)                     
#>  evaluate       0.14       2019-05-28 [1] CRAN (R 4.1.0)                     
#>  fansi          0.5.0      2021-05-25 [1] CRAN (R 4.1.0)                     
#>  fs             1.5.0      2020-07-31 [1] CRAN (R 4.1.0)                     
#>  furrr        * 0.2.3.9000 2021-07-02 [1] Github (DavisVaughan/furrr@4068c95)
#>  future       * 1.21.0     2020-12-10 [1] CRAN (R 4.1.0)                     
#>  future.apply   1.8.1      2021-08-10 [1] CRAN (R 4.1.0)                     
#>  generics       0.1.0      2020-10-31 [1] CRAN (R 4.1.0)                     
#>  ggplot2        3.3.5      2021-06-25 [1] CRAN (R 4.1.0)                     
#>  ggridges       0.5.3      2021-01-08 [1] CRAN (R 4.1.0)                     
#>  globals        0.14.0     2020-11-22 [1] CRAN (R 4.1.0)                     
#>  glue           1.4.2      2020-08-27 [1] CRAN (R 4.1.0)                     
#>  gridExtra      2.3        2017-09-09 [1] CRAN (R 4.1.0)                     
#>  gtable         0.3.0      2019-03-25 [1] CRAN (R 4.1.0)                     
#>  highr          0.9        2021-04-16 [1] CRAN (R 4.1.0)                     
#>  htmltools      0.5.1.1    2021-01-22 [1] CRAN (R 4.1.0)                     
#>  inline         0.3.19     2021-05-31 [1] CRAN (R 4.1.0)                     
#>  jsonlite       1.7.2      2020-12-09 [1] CRAN (R 4.1.0)                     
#>  knitr          1.33       2021-04-24 [1] CRAN (R 4.1.0)                     
#>  lattice        0.20-44    2021-05-02 [1] CRAN (R 4.1.0)                     
#>  lavaan       * 0.6-8      2021-03-10 [1] CRAN (R 4.1.0)                     
#>  lifecycle      1.0.0      2021-02-15 [1] CRAN (R 4.1.0)                     
#>  listenv        0.8.0      2019-12-05 [1] CRAN (R 4.1.0)                     
#>  loo            2.4.1      2020-12-09 [1] CRAN (R 4.1.0)                     
#>  magrittr       2.0.1      2020-11-17 [1] CRAN (R 4.1.0)                     
#>  MASS         * 7.3-54     2021-05-03 [1] CRAN (R 4.1.0)                     
#>  Matrix         1.3-3      2021-05-04 [1] CRAN (R 4.1.0)                     
#>  MatrixModels   0.5-0      2021-03-02 [1] CRAN (R 4.1.0)                     
#>  matrixStats    0.60.0     2021-07-26 [1] CRAN (R 4.1.0)                     
#>  mcmc           0.9-7      2020-03-21 [1] CRAN (R 4.1.0)                     
#>  MCMCpack       1.5-0      2021-01-20 [1] CRAN (R 4.1.0)                     
#>  mnormt         2.0.2      2020-09-01 [1] CRAN (R 4.1.0)                     
#>  munsell        0.5.0      2018-06-12 [1] CRAN (R 4.1.0)                     
#>  mvtnorm        1.1-2      2021-06-07 [1] CRAN (R 4.1.0)                     
#>  nonnest2       0.5-5      2020-07-05 [1] CRAN (R 4.1.0)                     
#>  parallelly     1.26.1     2021-06-30 [1] CRAN (R 4.1.0)                     
#>  pbivnorm       0.6.0      2015-01-23 [1] CRAN (R 4.1.0)                     
#>  pillar         1.6.1      2021-05-16 [1] CRAN (R 4.1.0)                     
#>  pkgbuild       1.2.0      2020-12-15 [1] CRAN (R 4.1.0)                     
#>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.1.0)                     
#>  plyr           1.8.6      2020-03-03 [1] CRAN (R 4.1.0)                     
#>  prettyunits    1.1.1      2020-01-24 [1] CRAN (R 4.1.0)                     
#>  processx       3.5.2      2021-04-30 [1] CRAN (R 4.1.0)                     
#>  ps             1.6.0      2021-02-28 [1] CRAN (R 4.1.0)                     
#>  purrr          0.3.4      2020-04-17 [1] CRAN (R 4.1.0)                     
#>  quantreg       5.86       2021-06-06 [1] CRAN (R 4.1.0)                     
#>  R6             2.5.0      2020-10-28 [1] CRAN (R 4.1.0)                     
#>  Rcpp         * 1.0.7      2021-07-07 [1] CRAN (R 4.1.0)                     
#>  RcppParallel * 5.1.4      2021-05-04 [1] CRAN (R 4.1.0)                     
#>  reprex         2.0.0      2021-04-02 [1] CRAN (R 4.1.0)                     
#>  rlang          0.4.11     2021-04-30 [1] CRAN (R 4.1.0)                     
#>  rmarkdown      2.9        2021-06-15 [1] CRAN (R 4.1.0)                     
#>  rstan          2.21.2     2020-07-27 [1] CRAN (R 4.1.0)                     
#>  rstantools     2.1.1      2020-07-06 [1] CRAN (R 4.1.0)                     
#>  rstudioapi     0.13       2020-11-12 [1] CRAN (R 4.1.0)                     
#>  sandwich       3.0-1      2021-05-18 [1] CRAN (R 4.1.0)                     
#>  scales         1.1.1      2020-05-11 [1] CRAN (R 4.1.0)                     
#>  sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 4.1.0)                     
#>  SparseM        1.81       2021-02-18 [1] CRAN (R 4.1.0)                     
#>  StanHeaders    2.21.0-7   2020-12-17 [1] CRAN (R 4.1.0)                     
#>  stringi        1.6.2      2021-05-17 [1] CRAN (R 4.1.0)                     
#>  stringr        1.4.0      2019-02-10 [1] CRAN (R 4.1.0)                     
#>  tibble         3.1.2      2021-05-16 [1] CRAN (R 4.1.0)                     
#>  tictoc       * 1.0.1      2021-04-19 [1] CRAN (R 4.1.0)                     
#>  tidyselect     1.1.1      2021-04-30 [1] CRAN (R 4.1.0)                     
#>  tmvnsim        1.0-2      2016-12-15 [1] CRAN (R 4.1.0)                     
#>  utf8           1.2.1      2021-03-12 [1] CRAN (R 4.1.0)                     
#>  V8             3.4.2      2021-05-01 [1] CRAN (R 4.1.0)                     
#>  vctrs          0.3.8      2021-04-29 [1] CRAN (R 4.1.0)                     
#>  withr          2.4.2      2021-04-18 [1] CRAN (R 4.1.0)                     
#>  xfun           0.24       2021-06-15 [1] CRAN (R 4.1.0)                     
#>  yaml           2.2.1      2020-02-01 [1] CRAN (R 4.1.0)                     
#>  zoo            1.8-9      2021-03-09 [1] CRAN (R 4.1.0)                     
#> 
#> [1] /Library/Frameworks/R.framework/Versions/4.1/Resources/library
```

</details>
