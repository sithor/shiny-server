tab <- matrix(c(50, 25,
                100,
                200), ncol = 2)
tab
rr <- epitools::epitab(tab, method = "riskratio")
rr$tab
rr$tab[2,"riskratio"]
rr$tab[2, "p0"]
rr$tab[2,"lower"]
rr$tab[2,"upper"]
0.89/0.66


group <- c("Exposed", "Outcome", "Exposed&Outcome", "All")
n_elements <- c(50, 25, 100)
fit1 <- euler(c("A" = 25, "B" = 5, "C" = 5,
                "A&B" = 5, "A&C" = 5, "B&C" = 3,
                "A&B&C" = 35))


plot(fit1)
twoxtwo <- c(
  Exposed = 225,
  Outcome = 300,
  Unexposed = 150,
  NoOutcome = 75,
  "Exposed&Outcome" = 200,
  "Unexposed&Outcome" = 100,
  "Unexposed&NoOutcome" = 50,
  "Exposed&NoOutcome" = 25
)
fit3 <- euler(twoxtwo, shape = "ellipse")
plot(fit3)

names(n_elements) <- group

library(eulerr)

set.seed(1)
res <- euler(n_elements)

plot(res)
