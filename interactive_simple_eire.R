library(tidyverse)
library(Matrix)
library(nleqslv)
library(MASS)
library(Rcpp)
library(gEcon)




eire_object <- make_model('simple_rbc_eire_model.gcn')



eire_ss <- steady_state(eire_object)
eire_ss <- solve_pert(eire_ss)



get_ss_values(eire_ss)
get_par_values(eire_ss)
re_solved(eire_ss)



path <- matrix(
  c(0.05),
  nrow = 1,
  ncol = 1
)

eire_sim <- simulate_model(
  eire_ss,
  variables  = c('K_s', 'L_s', 'C', 'r', 'W'),
  shocks     = 'epsilon_A',
  shock_path = path,
  sim_length = 100
)

plot_simulation(eire_sim)



random_eire_sim <- random_path(
  eire_ss,
  variables = c('W', 'r', 'C', 'L_s', 'A'),
  sim_length = 300
)

plot_simulation(random_eire_sim)



