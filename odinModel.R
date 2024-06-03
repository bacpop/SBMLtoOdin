
initial(r5) <- r5_init
r5_init <- user(0)
initial(R5) <- R5_init
R5_init <- user(0)
initial(r7) <- r7_init
r7_init <- user(0)
initial(R7) <- R7_init
R7_init <- user(0)
K1 <- 1
ke <- 0.3
kg <- 0.1
kf <- 2.5
kminus1 <- 1
h <- 3
kh <- 0.06
deriv(r5) <- 0 + endosome * K1 - endosome * sig_act_t(r5, ke, t, kg, R5, kf) - endosome * extraction(kminus1, r5) + endosome * sig_act(ke, R5, kg, R7, kf) + endosome * hydrolysis(kh, R5)
deriv(R5) <- 0 + endosome * sig_act_t(r5, ke, t, kg, R5, kf) - endosome * sig_act(ke, R5, kg, R7, kf) - endosome * hydrolysis(kh, R5)
deriv(r7) <- 0 + endosome * K1 - endosome * hill_act(r7, ke, R7, h, kg) - endosome * sig_act(ke, r7, kg, R5, kf) - endosome * extraction(kminus1, r7) + endosome * hydrolysis(kh, R7)
deriv(R7) <- 0 + endosome * hill_act(r7, ke, R7, h, kg) + endosome * sig_act(ke, r7, kg, R5, kf) - endosome * hydrolysis(kh, R7)
endosome <- 1