
# Initial Conditions
initial(PX) <- PX_init
PX_init <- user(0)
initial(PY) <- PY_init
PY_init <- user(0)
initial(PZ) <- PZ_init
PZ_init <- user(0)
initial(X) <- X_init
X_init <- user(0)
initial(Y) <- Y_init
Y_init <- user(20)
initial(Z) <- Z_init
Z_init <- user(0)
# Differential equations
deriv(PX) <- 0 + 1 * k_tl * X - 1 * kd_prot * PX
deriv(PY) <- 0 + 1 * k_tl * Y - 1 * kd_prot * PY
deriv(PZ) <- 0 + 1 * k_tl * Z - 1 * kd_prot * PZ
deriv(X) <- 0 - 1 * kd_mRNA * X + 1 * a0_tr + a_tr * (KM)^(n) / ((KM)^(n) + (PZ)^(n))
deriv(Y) <- 0 - 1 * kd_mRNA * Y + 1 * a0_tr + a_tr * (KM)^(n) / ((KM)^(n) + (PX)^(n))
deriv(Z) <- 0 - 1 * kd_mRNA * Z + 1 * a0_tr + a_tr * (KM)^(n) / ((KM)^(n) + (PY)^(n))
# Parameters
beta <- tau_mRNA / tau_prot
alpha0 <- a0_tr * eff * tau_prot / (log(2) * KM)
alpha <- a_tr * eff * tau_prot / (log(2) * KM)
eff <- user(20)
n <- user(2)
KM <- user(40)
tau_mRNA <- user(2)
tau_prot <- user(10)
t_ave <- tau_mRNA / log(2)
kd_mRNA <- log(2) / tau_mRNA
kd_prot <- log(2) / tau_prot
k_tl <- eff / t_ave
a_tr <- (ps_a - ps_0) * 60
ps_a <- user(0.5)
ps_0 <- user(5e-04)
a0_tr <- ps_0 * 60
# Events
# Compartments
cell <- 1