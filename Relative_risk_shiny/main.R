pacman::p_load(shiny, epitools, ggplot2, shinythemes,
               eulerr, mekko, epiR)

source("./Relative_risk_shiny/functions.R") #inzight plot

## https://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_association/EP713_Association7.html
tab <- matrix(c(89100, 9500,
                900,
                500), ncol = 2)
#tab <- matrix(c( input$no_outcome_unexposed,input$no_outcome_exposed,
       #          input$outcome_unexposed, input$outcome_exposed),
        
#      ncol = 2)
tab 
dimnames(tab) <- list(c("No","Yes"), c("No","Yes"))
names(dimnames(tab)) <- c("Exposure", "Disease")
tab
df <- expand.table(tab)
dat.v01 <- c(13,2163,5,3349); dat.v01
epi.2by2(dat = dat.v01, method = "cross.sectional", digits = 2, 
         conf.level = 0.95, units = 100, interpret = FALSE, outcome = "as.columns")

df |> str()
tab[1,1]
tab[1,2]
tab[2,2]
tab[2,1]
epiR_tab <-  cbind(tab[,2], tab[,1]) |> as.table()
epiR_tab <-  rbind(epiR_tab[2,], epiR_tab[1,]) |> as.table()
x <- epiR::epi.2by2(epiR_tab,
         method="cohort.count",
         digits=2,
         conf.level=0.95, 
         units=100,
         interpret=TRUE,
         outcome="as.columns")
x
x |> str()
x$massoc.detail$PAFRisk.strata.wald |> str()

rr <- epitools::epitab(tab, method = "riskratio")
rr$tab
0.2/1.2

rr$tab[2, "p1"]*(rr$tab[2,"riskratio"] - 1)/(1+rr$tab[2, "p1"]*(rr$tab[2,"riskratio"] - 1))
sum(tab)
rr$tab[, "Yes"]
rr$tab[, "Yes"] |> sum()

Overall_prevalence <-  sum(rr$tab["Yes", ])/sum(tab)
Overall_prevalence

PAR <-((Overall_prevalence*(rr$tab[2,"riskratio"] - 1)) /
         (1 + Overall_prevalence*(rr$tab[2,"riskratio"] - 1))) 
PAR
PAR <-((rr$tab[2, "p0"]*(rr$tab[2,"oddsratio"] - 1))/
         (1 + rr$tab[2, "p0"]*(rr$tab[2,"oddsratio"] - 1))) 

PAR
rr$tab
rr$tab[2,"riskratio"]
rr$tab[2, "p0"]
rr$tab[2,"lower"]
rr$tab[2,"upper"]
0.89/0.66

## Estimate PAR & CI with delta method



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
