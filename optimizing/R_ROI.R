ROI_available_solvers() # check available solvers
ROI_installed_solvers() # check installed solvers
## 
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.qpoases)
library(ROI.plugin.ecos)
library(ROI.plugin.scs)
library(ROI.plugin.alabama)
library(ROI.plugin.lpsolve)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(tictoc)
library("ROI.plugin.quadprog")
library("ROI.plugin.symphony")

library(ompr)
library(ompr.roi)

library("xlsx")

# test for problem

# Problem dimensionen J Lager und k Kunden

k <- 6
j <- 3
## Kostenmatrix von k nach j

C.transport<- rbind(c(3,59,57), c(30,26,24), c(46,10,8),
                    c(49,13,5),c(74,38,20),c(76,20,38))

# Anzahl Lager =3 j=1,2,3

# fixkosten_lager

c.fixkosten= c(6,20,15)

# Kapazitäten

Kap.Lager= c(4,3,4)



model <- MIPModel() %>%
  # Variable O (Boolesche Variable nur 0 oder 1)
  add_variable(O[m], m = 1:j, type = "integer", lb = 0, ub = 1) %>%
  # Variable S (Matrix, bestimmt ob Kunde k durch Lager j beliefert wird)
  add_variable(S[l,m], l = 1:k, m = 1:j, type = "integer", lb = 0,ub=1) %>% 
  
  set_objective(sum_expr(S[l,m]*C.transport[l,m],l = 1:k, m = 1:j)
                + sum_expr(c.fixkosten[m]*O[m], m = 1:j), "min")

# erste NB
# Nur ein Kunde pro Warenhaus
model <- add_constraint(model, sum_expr(S[l,m],  m = 1:j) == 1, l = 1:k)
 # zweite NB
#nur ein Lagerhaus pro Kunde
#model <- add_constraint(model, sum_expr(S[l,m], l=1:k)<= O[m],   m=1:j)
# dritte NB
#Kapazitäten pro Lager beachten
model <- add_constraint(model, sum_expr(S[l,m], l = 1:k) <= Kap.Lager[m]*O[m], m = 1:j)



# solve model with GLPK
result <- solve_model(model, with_ROI(solver = "symphony"))
solution <- get_solution(result, O[m]) 
solution
solution <- get_solution(result, S[l,m]) 
solution

write.xlsx(solution, "Solution.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


## Fallstudie Zwei Ab wann wird Warenhaus 2 unprofitabel?
# ab 43

c.fixkosten= c(6,20,15)

model <- MIPModel() %>%
  # Variable O (Boolesche Variable nur 0 oder 1)
  add_variable(O[m], m = 1:j, type = "integer", lb = 0, ub = 1) %>%
  # Variable S (Matrix, bestimmt ob Kunde k durch Lager j beliefert wird)
  add_variable(S[l,m], l = 1:k, m = 1:j, type = "integer", lb = 0, ub=1) %>% 
  
  set_objective(sum_expr(S[l,m]*C.transport[l,m],l = 1:k, m = 1:j)
                + sum_expr(c.fixkosten[m]*O[m], m = 1:j), "min")


# erste NB
# Nur ein Kunde pro Warenhaus
model <- add_constraint(model, sum_expr(S[l,m],  m = 1:j) == 1, l = 1:k)
# zweite NB
#nur ein Lagerhaus 
#model <- add_constraint(model, sum_expr(S[l,m], l=1:k)<= O[m],   m=1:j)
# dritte NB
#Kapazitäten pro Lager beachten
model <- add_constraint(model, sum_expr(S[l,m], l = 1:k) <= Kap.Lager[m]*O[m], m = 1:j)


# solve model with GLPK
result <- solve_model(model, with_ROI(solver = "glpk",verbose=TRUE))
solution <- get_solution(result, O[m]) 
solution
solution <- get_solution(result, S[l,m]) 
solution

write.xlsx(solution, "Solution.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


## Fallstudie mit Fabrik
# ab 43

i <- 1
j <- 3
k <- 6


## Transport von i nach k
i.transport <-rbind(c(2,3,10))
## Kostenmatrix von k nach j

C.transport<- rbind(c(3,59,57), c(30,26,24), c(46,10,8),
                    c(49,13,5),c(74,38,20),c(76,20,38))

c1.transport<-C.transport+2
c2.transport<-C.transport+3
c3.transport<-C.transport+10

library(abind)
#cik.transport <-array(c(i.transport,C.transport),dim = c(6,3,6))
cik.transport <- abind(c1.transport,c2.transport,c3.transport, along = 3)
# bedarf als verhaeltnis zum Markt
dk.Bedarf <- rbind(c(1/6,1/6,1/6,1/6,1/6,1/6))
# Menge in Fabrik i 
si=10/6

# Anzahl Lager =3 j=1,2,3

# fixkosten_lager

c.fixkosten= c(6,20,15)

# Kapazitäten

Kap.Lager= c(4,3,4)

# Modell
model <- MIPModel() %>%
  # Variable x (transportmenge von i nach k)
  add_variable(x[l,m,n], l = 1:i,m=1:j,n=1:k,  lb = 0) %>%
  # boolesche Variable y (Vektor ob Lagerhaus öffnet)
  add_variable(y[m], m = 1:j, type = "integer", lb = 0, ub=1) %>% 
  
  set_objective(sum_expr(cik.transport(l,m,n)*x[l,m,n],l = 1:i,m=1:j,n=1:k)
                + sum_expr(c.fixkosten[m]*y[m], m = 1:j), "min")


# erste NB
# Nur ein Kunde pro Warenhaus
model <- add_constraint(model, sum_expr(x[l,m,n], l = 1:i,m=1:j,n=1:k) == 1)

model <- add_constraint(model, sum_expr(x[l,m,n], m=1:j,n=1:k) <= si[l], l = 1:i)

model <- add_constraint(model, sum_expr(x[l,m,n], l = 1:i,m=1:j) >= dk.Bedarf[n],n=1:k)

model <- add_constraint(model, sum_expr(x[l,m,n], l=1:i,n=1:k) <= Kap.Lager[m]*y[m], m = 1:j)

model <- add_constraint(model, sum_expr(x[l,m,n], l=1:i,n=1:k) <= si[l]*y[m], l=1:i, m = 1:j)

result <- solve_model(model, with_ROI(solver = "glpk",verbose=TRUE))
solution <- get_solution(result, x[l,m,n]) 
solution 
solution <- get_solution(result, y[m]) 
solution 

write.xlsx(solution, "Solution.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)




# Fallstudie mit Fabrik


i <- 1
j <- 3
k <- 6


## Transport von i nach k
i.transport <-rbind(c(2,3,10))
## Kostenmatrix von k nach j

Citransport= rbind(c(5,32,48,51,76,78),
              c(82,46,13,16,21,41),
              c(67,34,18,15,30,48))

#library(abind)
#cik.transport <-array(c(i.transport,C.transport),dim = c(6,3,6))
#cik.transport <- abind(c1.transport,c2.transport,c3.transport, along = 3)
# bedarf als verhaeltnis zum Markt
dk.Bedarf <- rbind(c(1/6,1/6,1/6,1/6,1/6,1/6))
# Menge in Fabrik i 
si=10/6

# Anzahl Lager =3 j=1,2,3

# fixkosten_lager

c.fixkosten= c(6,20,15)

# Kapazitäten

Kap.Lager= c(4,3,4)

# Modell
model <- MIPModel() %>%
  # Variable x (transportmenge von i nach k)
  add_variable(x[m,n], m=1:j,n=1:k,  lb = 0) %>%
  # boolesche Variable y (Vektor ob Lagerhaus öffnet)
  add_variable(y[m], m = 1:j, type = "integer", lb = 0, ub=1) %>% 
  
  set_objective(sum_expr(Citransport[m,n]*x[m,n] ,m=1:j,n=1:k)
                + sum_expr(c.fixkosten[m]*y[m], m = 1:j), "min")


# erste NB
# Nur ein Kunde pro Warenhaus
model <- add_constraint(model, sum_expr(x[m,n], m=1:j,n=1:k) == 1)

model <- add_constraint(model, sum_expr(x[m,n], m=1:j) >= dk.Bedarf[n],n=1:k)

model <- add_constraint(model, sum_expr(x[m,n], n=1:k) <= Kap.Lager[m]*y[m], m = 1:j)

model <- add_constraint(model, sum_expr(x[m,n], n=1:k) <= si*y[m],  m = 1:j)

result <- solve_model(model, with_ROI(solver = "glpk",verbose=TRUE))
solution <- get_solution(result, x[m,n]) 
solution 
solution <- get_solution(result, y[m]) 
solution 

write.xlsx(solution, "Solution.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


#Problem mit M-Entspannung

M_Const=1

model <- MIPModel() %>%
  # Variable x (transportmenge von i nach k)
  add_variable(x[m,n], m=1:j,n=1:k, type="integer", lb=1/6,ub=1/6) %>%
  # boolesche Variable y (Vektor ob Lagerhaus öffnet)
  add_variable(y[m], m = 1:j, type = "integer", lb = 0, ub=1) %>% 
  
  set_objective(sum_expr(cik.transport(m,n)*x[m,n],m=1:j,n=1:k)
                + sum_expr(c.fixkosten[m]*y[m], m = 1:j), "min")


# erste NB
# Nur ein Kunde pro Warenhaus
model <- add_constraint(model, sum_expr(x[m,n], m=1:j,n=1:k) == 1)

model <- add_constraint(model, sum_expr(x[m,n], m=1:j) >= dk.Bedarf[n],n=1:k)

model <- add_constraint(model, sum_expr(x[m,n], n=1:k) <= Kap.Lager[m]*y[m], m = 1:j)

model <- add_constraint(model, sum_expr(x[m,n], n=1:k) <= si*y[m],  m = 1:j)

model <- add_constraint(model, sum_expr(x[m,n] , n=1:k) + M_Const*(1-y[m]) >= 0,  m=1:j)


result <- solve_model(model, with_ROI(solver = "symphony"))
solution <- get_solution(result, x[m,n]) 
solution 
solution <- get_solution(result, y[m]) 
solution 

write.xlsx(solution, "Solution.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


# beliebiges großes problem

set.seed(1234)
grid_size <- 10000

n <- 10
Kunden <- data.frame(
  id = 1:n,
  x = round(runif(n) * grid_size),
  y = round(runif(n) * grid_size)
)

m <- 20
Warenhaus_Orte<- data.frame(
  id = 1:m,
  x = round(runif(m) * grid_size),
  y = round(runif(m) * grid_size)
)
cf <- round(rnorm(m, mean = grid_size * 10, sd = grid_size * 5))


Distanz <- function(i, j) {
  Kunden <- Kunden[i, ]
  Warenhaus_Orte<- Warenhaus_Orte[j, ]
  round(sqrt((Kunden$x - Warenhaus_Orte$x)^2 + (Kunden$y - Warenhaus_Orte$y)^2))
}


tic("Modell und Lösen")
model <- MIPModel() %>%
  # Ob der Kunde i durch das Warenhaus j beliefert wird
  add_variable(S[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # ob das Warenhaus gebaut wird
  add_variable(O[j], j = 1:m, type = "binary") %>%
  
  # Minimieren
  set_objective(sum_expr(Distanz(i, j) * S[i, j], i = 1:n, j = 1:m) + 
                  sum_expr(cf[j] * O[j], j = 1:m), "min") %>%
  
  # Jeder Kunde hat ein Warenhaus
  add_constraint(sum_expr(S[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
  # Warenhäuser werden gebaut
  add_constraint(S[i,j] <= O[j], i = 1:n, j = 1:m)

# mit zeit


result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
toc()
solution <- get_solution(result, S[m,n]) 
solution 
solution <- get_solution(result, O[m]) 
solution 
