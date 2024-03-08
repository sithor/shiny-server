tab <- matrix(c(50, 25,
                100,
                200), ncol = 2)
tab
rr <- epitools::epitab(tab, method = "riskratio")
rr$tab
rr$tab[2,"riskratio"]
rr$tab[2,"lower"]
rr$tab[2,"upper"]
0.89/0.66
