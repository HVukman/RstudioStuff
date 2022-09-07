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
library(ggplot2)
#library(ROI.plugin.gurobi)


# Beispiel
A <- rbind(c(6, 4, 5), c(8, 0, 2), c(9, 1, 7))
milp <- OP(objective = L_objective(c(7, 1, 3), c("x", "y", "z")),
           constraints = L_constraint(L = rbind(c(6, 4, 5), c(8, 0, 2), c(9, 1, 7)),
                                      dir = c("<=", "<=", "<="),
                                      rhs = c(60, 80, 70)),
           types = c("I", "C", "I"), 
           maximum = TRUE)
(sol <- ROI_solve(milp))


# Kapazitätsproblem


# install some new solvers
library("ROI.plugin.quadprog")
library("ROI.plugin.symphony")


# create problem dimension
n <- 10
# create cost vector
c.vec <- rpois(n^2, 100)    # sample costs from Poisson distribution with mean 100
# create empty constraint matrix
L.mat <- matrix(0, ncol = n^2, nrow = 2*n)
# first n rows
L.mat[1:n,] <- do.call(cbind, lapply(1:n, function(x) diag(n)))
# last n rows
L.mat[(n+1):(2*n),] <- t(sapply(1:n, function(x) c(rep(0,(x-1)*n), rep(1,n),rep(0,(n-x)*n) )))

# create optimization problem
copt <- OP(
  objective = L_objective(L = c.vec),
  constraints = L_constraint(L = L.mat, dir = rep("==", 2*n), rhs = rep(1, 2*n)),
  types = rep("B", n^2)
)

copt_sol <- ROI_solve(copt)         # solve the problem
copt_sol$solution                   # obtain solution
copt_sol$objval                     # objective value

# display solution in matrix form
sol.mat <- matrix(copt_sol$solution, ncol = n)
# check first set of constraints
colSums(sol.mat)                    # all column sums equal to 1?
# check second set of constraints
rowSums(sol.mat)                    # all row sums equal to 1?
# check objective
sum(sol.mat * matrix(c.vec, ncol = n))


###



library(ompr)
library(ompr.roi)


n <- 10          
# sample node coordinates
cities <- data.frame(id = 1:n, x = runif(n), y = runif(n))  
# plot nodes
ggplot(cities, aes(x, y)) + geom_point()    

distance <- as.matrix(dist(cities[,c("x","y")]), method = "minkowski")       # calculate distances
# create vectorized function for accessing distance values
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}
# compare results: retain distance of arcs (1,2), (2,3), and (3,4)
distance[c(1,2,3), c(2,3,4)]
dist_fun(c(1,2,3), c(2,3,4))

  
  
  model <- MIPModel() %>%
  # create decision variable that is 1 iff we travel from node i to j
  add_variable(x[i, j], i = 1:n, j = 1:n, type = "integer", lb = 0, ub = 1) %>%
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  # minimize travel distance
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%
  # leave each node
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  # visit each node
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  # ensure no subtours (arc constraints)
  add_constraint(u[1] == 1) %>% 
  add_constraint(u[j] >= u[i] + 1 - n * (1 - x[i, j]), i = 1:n, j = 2:n) %>%
  # exclude self-cycles
  set_bounds(x[i, i], ub = 0, i = 1:n)

# solve model with GLPK
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

# obtain solution
solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0) 
solution



# create problem dimension
n <- 10
# create cost vector
c.vec <- rpois(n^2, 100)    # sample costs from Poisson distribution with mean 100
# create empty constraint matrix
L.mat <- matrix(0, ncol = n^2, nrow = 2*n)
# first n rows
L.mat[1:n,] <- do.call(cbind, lapply(1:n, function(x) diag(n)))
# last n rows
L.mat[(n+1):(2*n),] <- t(sapply(1:n, function(x) c(rep(0,(x-1)*n), rep(1,n),rep(0,(n-x)*n) )))

# create optimization problem
copt <- OP(
  objective = L_objective(L = c.vec),
  constraints = L_constraint(L = L.mat, dir = rep("==", 2*n), rhs = rep(1, 2*n)),
  types = rep("B", n^2)
)

copt_sol <- ROI_solve(copt)         # solve the problem
copt_sol$solution                   # obtain solution
copt_sol$objval                     # objective value

# display solution in matrix form
sol.mat <- matrix(copt_sol$solution, ncol = n)
# check first set of constraints
colSums(sol.mat)                    # all column sums equal to 1?
# check second set of constraints
rowSums(sol.mat)                    # all row sums equal to 1?
# check objective
sum(sol.mat * matrix(c.vec, ncol = n))




# test for problem

# Problem dimensionen J Lager und k Kunden

k <- 6
j <- 3
## Kostenmatrix von k nach j

C.transport<- rbind(c(3,59,57), c(30,26,24), c(46,10,8),
      c(49,13,5),c(20,38,74),c(76,20,38))

# Anzahl Lager =3 j=1,2,3

# fixkosten_lager

c.fixkosten= c(6,20,15)

# Kapazitäten

Kap.Lager= c(4,3,4)

model <- MIPModel() %>%
 
  # Variable O (boolesche Variable nur 0 oder 1)
  add_variable(O[m], m = 1:j, type = "integer", lb = 0, ub = 1) %>%
  # Variable S (boolesche matrix bestimmt ob kunde k durch lager j beliefert wird)
  add_variable(S[l,m], l = 1:k, m = 1:j, type = "integer", lb = 0, ub = 1) %>% 
  # Minimieren der Funktion
  set_objective(sum_expr(S[l,m]*C.transport[l,m] + 
                           c.fixkosten*O[m] , l = 1:k, m = 1:j), "min") %>%
  # erste NB
  # sum warehouses is equal one
  model <- add_constraint(sum_expr(S[l,m],  m = 1:j) == 1, l = 1:k) 
  # # nur ein lagerhaus pro kunde
  # add_constraint(O[m] >= S[l,m], m = 1:j, l = 1:k) 
  # Kapazität pro Lagerhaus beachten
  model <- add_constraint(model, sum_expr(S[l,m], l = 1:k) <= Kap.Lager[m]*O[j], m = 1:j)



# solve model with GLPK
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
solution <- get_solution(result, O[m]) 
solution
solution <- get_solution(result, S[l,m]) 
solution
