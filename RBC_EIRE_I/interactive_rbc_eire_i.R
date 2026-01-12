library(tidyverse)
library(Matrix)
library(nleqslv)
library(MASS)
library(Rcpp)
library(gEcon)




eire <- make_model('rbc_eire_i_model.gcn')


eire <- initval_var(eire, c(
  Z_D = 1, Z_M = 1, Z_IM = 1, Z_G = 1,
  r_world = 0.02, r_B = 0.02, r_D = 0.04, r_M = 0.08,
  p_m = 1, p_X = 1, w = 1, tau_L = 0.193,
  c = 0.8, c_D = 0.5, c_m = 0.3, s = 0.8,
  L_s = 0.33, L_Dd = 0.2, L_M = 0.13,
  k_Ds = 5, k_Dd = 5, k_M = 10,
  i_D = 0.15, i_M = 0.35,
  y_D = 1.2, y_M = 2, y_GDP = 3, y_GNI_star = 2, y_hat_star = 0,
  g = 0.4, d = 0.6, b = 0, pi_D = 0.1, adj_cost = 0,
  U = -50, lambda__REPRESENTATIVE_HOUSEHOLD_2 = 1, PI_D = 0.1
))


eire <- initval_calibr_par(eire, c(y_GNI_star_bar = 2, d_bar_star = 0.3))


eire <- steady_state(eire)
eire <- solve_pert(eire)


sim <- random_path(eire, sim_length = 200)
plot_simulation(sim)
