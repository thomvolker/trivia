#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#' ---

library(MASS)
library(blavaan)
library(furrr)
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

blavCompare(mod_out$H1, mod_out$H0)

toc()
