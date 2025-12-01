# Generated on 2025-12-01 19:27:06 by gEcon ver. 1.2.3 (2025-04-13)
# http://gecon.r-forge.r-project.org/

# Model name: simple_rbc_eire_model

# info
info__ <- c("simple_rbc_eire_model", "/Users/dcsmac/Desktop/RBC_EIRE_I/simple_rbc_eire_model.gcn", "2025-12-01 19:27:06", "false")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("lambda_c",
                 "pi",
                 "r",
                 "A",
                 "C",
                 "I",
                 "K_s",
                 "K_d",
                 "L_s",
                 "L_d",
                 "PI",
                 "U",
                 "W",
                 "Y")

variables_tex__ <- c("\\lambda^{\\mathrm{c}}",
                     "\\pi",
                     "r",
                     "A",
                     "C",
                     "I",
                     "K^{\\mathrm{s}}",
                     "K^{\\mathrm{d}}",
                     "L^{\\mathrm{s}}",
                     "L^{\\mathrm{d}}",
                     "\\Pi",
                     "U",
                     "W",
                     "Y")

# shocks
shocks__ <- c("epsilon_A")

shocks_tex__ <- c("\\epsilon^{\\mathrm{A}}")

# parameters
parameters__ <- c("alpha",
                  "beta",
                  "delta",
                  "eta",
                  "phi",
                  "rho",
                  "sigma")

parameters_tex__ <- c("\\alpha",
                     "\\beta",
                     "\\delta",
                     "\\eta",
                     "\\phi",
                     "\\rho",
                     "\\sigma")

# free parameters
parameters_free__ <- c("alpha",
                       "beta",
                       "delta",
                       "eta",
                       "phi",
                       "rho",
                       "sigma")

# free parameters' values
parameters_free_val__ <- c(0.33,
                           0.99,
                           0.025,
                           1,
                           0.15,
                           0.979,
                           0.2)

# equations
equations__ <- c("K_s[-1] - K_d[] = 0",
                 "-lambda_c[] + beta * ((1 - delta) * E[][lambda_c[1]] + E[][lambda_c[1] * r[1]]) = 0",
                 "-lambda_c[] + C[]^(-sigma) = 0",
                 "-pi[] + PI[] = 0",
                 "-r[] + alpha * A[] * K_d[]^(-1 + alpha) * L_d[]^(1 - alpha) = 0",
                 "-A[] + exp(epsilon_A[] + rho * log(A[-1])) = 0",
                 "L_s[] - L_d[] = 0",
                 "-W[] + A[] * (1 - alpha) * K_d[]^alpha * L_d[]^(-alpha) = 0",
                 "-Y[] + A[] * K_d[]^alpha * L_d[]^(1 - alpha) = 0",
                 "lambda_c[] * W[] - phi * L_s[]^eta = 0",
                 "I[] - K_s[] + K_s[-1] * (1 - delta) = 0",
                 "pi[] - Y[] + r[] * K_d[] + L_d[] * W[] = 0",
                 "U[] - beta * E[][U[1]] - (-1 + C[]^(1 - sigma)) * (1 - sigma)^-1 + phi * (1 + eta)^-1 * L_s[]^(1 + eta) = 0",
                 "pi[] - C[] - I[] + K_s[-1] * r[] + L_s[] * W[] = 0")

# calibrating equations
calibr_equations__ <- character(0)

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                 5, 5, 6, 7, 7, 8, 8, 8, 8, 9,
                                 9, 9, 9, 10, 10, 10, 11, 11, 12, 12,
                                 12, 12, 12, 12, 13, 13, 13, 14, 14, 14,
                                 14, 14, 14, 14),
                           j = c(7, 8, 1, 3, 1, 5, 2, 11, 3, 4,
                                 8, 10, 4, 9, 10, 4, 8, 10, 13, 4,
                                 8, 10, 14, 1, 9, 13, 6, 7, 2, 3,
                                 8, 10, 13, 14, 5, 9, 12, 2, 3, 5,
                                 6, 7, 9, 13),
                           x = c(1, 2, 6, 4, 2, 2, 2, 2, 2, 2,
                                 2, 2, 3, 2, 2, 2, 2, 2, 2, 2,
                                 2, 2, 2, 2, 2, 2, 2, 3, 2, 2,
                                 2, 2, 2, 2, 2, 2, 6, 2, 2, 2,
                                 2, 1, 2, 2),
                           dims = c(14, 14))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 14))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(14, 0))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 0))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(2, 2, 3, 5, 6, 8, 9, 10, 10, 11,
                                     13, 13, 13, 13),
                               j = c(2, 3, 7, 1, 6, 1, 1, 4, 5, 3,
                                     2, 4, 5, 7),
                               x = rep(1, 14), dims = c(14, 7))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 7))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(6),
                             j = c(1),
                             x = rep(1, 1), dims = c(14, 1))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(14)
    r[1] = v[7] - v[8]
    r[2] = -v[1] + pf[2] * (v[1] * v[3] + v[1] * (1 - pf[3]))
    r[3] = -v[1] + v[5]^(-pf[7])
    r[4] = -v[2] + v[11]
    r[5] = -v[3] + pf[1] * v[4] * v[8]^(-1 + pf[1]) * v[10]^(1 - pf[1])
    r[6] = -v[4] + exp(pf[6] * log(v[4]))
    r[7] = v[9] - v[10]
    r[8] = -v[13] + v[4] * (1 - pf[1]) * v[8]^pf[1] * v[10]^(-pf[1])
    r[9] = -v[14] + v[4] * v[8]^pf[1] * v[10]^(1 - pf[1])
    r[10] = v[1] * v[13] - pf[5] * v[9]^pf[4]
    r[11] = v[6] - v[7] + v[7] * (1 - pf[3])
    r[12] = v[2] - v[14] + v[3] * v[8] + v[10] * v[13]
    r[13] = v[12] - pf[2] * v[12] - (-1 + v[5]^(1 - pf[7])) * (1 - pf[7])^-1 + pf[5] * (1 + pf[4])^-1 * v[9]^(1 + pf[4])
    r[14] = v[2] - v[5] - v[6] + v[3] * v[7] + v[9] * v[13]

    return(r)
}

# calibrating equations
calibr_eq__ <- function(v, pc, pf)
{
    r <- numeric(0)

    return(r)
}

# steady state and calibrating equations Jacobian
ss_calibr_eq_jacob__ <- function(v, pc, pf)
{
    r <- numeric(0)
    jac <- numeric(44)
    jac[1] = 1
    jac[2] = -1
    jac[3] = -1 + pf[2] * (1 - pf[3] + v[3])
    jac[4] = pf[2] * v[1]
    jac[5] = -1
    jac[6] = -pf[7] * v[5]^(-1 - pf[7])
    jac[7] = -1
    jac[8] = 1
    jac[9] = -1
    jac[10] = pf[1] * v[8]^(-1 + pf[1]) * v[10]^(1 - pf[1])
    jac[11] = pf[1] * v[4] * (-1 + pf[1]) * v[8]^(-2 + pf[1]) * v[10]^(1 - pf[1])
    jac[12] = pf[1] * v[4] * (1 - pf[1]) * v[8]^(-1 + pf[1]) * v[10]^(-pf[1])
    jac[13] = -1 + pf[6] * v[4]^-1 * exp(pf[6] * log(v[4]))
    jac[14] = 1
    jac[15] = -1
    jac[16] = (1 - pf[1]) * v[8]^pf[1] * v[10]^(-pf[1])
    jac[17] = pf[1] * v[4] * (1 - pf[1]) * v[8]^(-1 + pf[1]) * v[10]^(-pf[1])
    jac[18] = -pf[1] * v[4] * (1 - pf[1]) * v[8]^pf[1] * v[10]^(-1 - pf[1])
    jac[19] = -1
    jac[20] = v[8]^pf[1] * v[10]^(1 - pf[1])
    jac[21] = pf[1] * v[4] * v[8]^(-1 + pf[1]) * v[10]^(1 - pf[1])
    jac[22] = v[4] * (1 - pf[1]) * v[8]^pf[1] * v[10]^(-pf[1])
    jac[23] = -1
    jac[24] = v[13]
    jac[25] = -pf[4] * pf[5] * v[9]^(-1 + pf[4])
    jac[26] = v[1]
    jac[27] = 1
    jac[28] = -pf[3]
    jac[29] = 1
    jac[30] = v[8]
    jac[31] = v[3]
    jac[32] = v[13]
    jac[33] = v[10]
    jac[34] = -1
    jac[35] = -v[5]^(-pf[7])
    jac[36] = pf[5] * v[9]^pf[4]
    jac[37] = 1 - pf[2]
    jac[38] = 1
    jac[39] = v[7]
    jac[40] = -1
    jac[41] = -1
    jac[42] = v[3]
    jac[43] = v[13]
    jac[44] = v[9]
    jacob <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                5, 5, 6, 7, 7, 8, 8, 8, 8, 9,
                                9, 9, 9, 10, 10, 10, 11, 11, 12, 12,
                                12, 12, 12, 12, 13, 13, 13, 14, 14, 14,
                                14, 14, 14, 14),
                          j = c(7, 8, 1, 3, 1, 5, 2, 11, 3, 4,
                                8, 10, 4, 9, 10, 4, 8, 10, 13, 4,
                                8, 10, 14, 1, 9, 13, 6, 7, 2, 3,
                                8, 10, 13, 14, 5, 9, 12, 2, 3, 5,
                                6, 7, 9, 13),
                          x = jac, dims = c(14, 14))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(4)
    Atm1x[1] = 1
    Atm1x[2] = pf[6] * v[4]^-1 * exp(pf[6] * log(v[4]))
    Atm1x[3] = 1 - pf[3]
    Atm1x[4] = v[3]
    Atm1 <- sparseMatrix(i = c(1, 6, 11, 14),
                         j = c(7, 4, 7, 7),
                         x = Atm1x, dims = c(14, 14))

    Atx <- numeric(41)
    Atx[1] = -1
    Atx[2] = -1
    Atx[3] = -1
    Atx[4] = -pf[7] * v[5]^(-1 - pf[7])
    Atx[5] = -1
    Atx[6] = 1
    Atx[7] = -1
    Atx[8] = pf[1] * v[8]^(-1 + pf[1]) * v[10]^(1 - pf[1])
    Atx[9] = pf[1] * v[4] * (-1 + pf[1]) * v[8]^(-2 + pf[1]) * v[10]^(1 - pf[1])
    Atx[10] = pf[1] * v[4] * (1 - pf[1]) * v[8]^(-1 + pf[1]) * v[10]^(-pf[1])
    Atx[11] = -1
    Atx[12] = 1
    Atx[13] = -1
    Atx[14] = (1 - pf[1]) * v[8]^pf[1] * v[10]^(-pf[1])
    Atx[15] = pf[1] * v[4] * (1 - pf[1]) * v[8]^(-1 + pf[1]) * v[10]^(-pf[1])
    Atx[16] = -pf[1] * v[4] * (1 - pf[1]) * v[8]^pf[1] * v[10]^(-1 - pf[1])
    Atx[17] = -1
    Atx[18] = v[8]^pf[1] * v[10]^(1 - pf[1])
    Atx[19] = pf[1] * v[4] * v[8]^(-1 + pf[1]) * v[10]^(1 - pf[1])
    Atx[20] = v[4] * (1 - pf[1]) * v[8]^pf[1] * v[10]^(-pf[1])
    Atx[21] = -1
    Atx[22] = v[13]
    Atx[23] = -pf[4] * pf[5] * v[9]^(-1 + pf[4])
    Atx[24] = v[1]
    Atx[25] = 1
    Atx[26] = -1
    Atx[27] = 1
    Atx[28] = v[8]
    Atx[29] = v[3]
    Atx[30] = v[13]
    Atx[31] = v[10]
    Atx[32] = -1
    Atx[33] = -v[5]^(-pf[7])
    Atx[34] = pf[5] * v[9]^pf[4]
    Atx[35] = 1
    Atx[36] = 1
    Atx[37] = v[7]
    Atx[38] = -1
    Atx[39] = -1
    Atx[40] = v[13]
    Atx[41] = v[9]
    At <- sparseMatrix(i = c(1, 2, 3, 3, 4, 4, 5, 5, 5, 5,
                             6, 7, 7, 8, 8, 8, 8, 9, 9, 9,
                             9, 10, 10, 10, 11, 11, 12, 12, 12, 12,
                             12, 12, 13, 13, 13, 14, 14, 14, 14, 14,
                             14),
                       j = c(8, 1, 1, 5, 2, 11, 3, 4, 8, 10,
                             4, 9, 10, 4, 8, 10, 13, 4, 8, 10,
                             14, 1, 9, 13, 6, 7, 2, 3, 8, 10,
                             13, 14, 5, 9, 12, 2, 3, 5, 6, 9,
                             13),
                         x = Atx, dims = c(14, 14))

    Atp1x <- numeric(3)
    Atp1x[1] = pf[2] * (1 - pf[3] + v[3])
    Atp1x[2] = pf[2] * v[1]
    Atp1x[3] = -pf[2]
    Atp1 <- sparseMatrix(i = c(2, 2, 13),
                         j = c(1, 3, 12),
                         x = Atp1x, dims = c(14, 14))

    Aepsx <- numeric(1)
    Aepsx[1] = exp(pf[6] * log(v[4]))
    Aeps <- sparseMatrix(i = c(6),
                         j = c(1),
                         x = Aepsx, dims = c(14, 1))

    return(list(Atm1, At, Atp1, Aeps))
}

ext__ <- list()

# create model object
gecon_model(model_info = info__,
            index_sets = index_sets__,
            variables = variables__,
            variables_tex = variables_tex__,
            shocks = shocks__,
            shocks_tex = shocks_tex__,
            parameters = parameters__,
            parameters_tex = parameters_tex__,
            parameters_free = parameters_free__,
            parameters_free_val = parameters_free_val__,
            equations = equations__,
            calibr_equations = calibr_equations__,
            var_eq_map = vareqmap__,
            shock_eq_map = shockeqmap__,
            var_ceq_map = varcalibreqmap__,
            cpar_eq_map = calibrpareqmap__,
            cpar_ceq_map = calibrparcalibreqmap__,
            fpar_eq_map = freepareqmap__,
            fpar_ceq_map = freeparcalibreqmap__,
            ss_function = ss_eq__,
            calibr_function = calibr_eq__,
            ss_calibr_jac_function = ss_calibr_eq_jacob__,
            pert = pert1__,
            ext = ext__)
