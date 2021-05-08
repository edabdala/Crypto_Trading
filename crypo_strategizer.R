library(lpSolve)

r_fee  = .005
investment <- 472.01
curr_price <- .525350


cost <- c(1) 

A <- matrix(c(curr_price*(1+r_fee)), nrow=1, byrow = TRUE) # final cost of investment is currency price*fee

B <- c(investment) # money put in must not exceed our desired amount

constraint_dir <- c("<=") # all constraints in ">=" form

model <- lp(direction = "max", # maximize shares bought
            objective.in = cost, # costs to buy a share
            const.mat = A, # constraint matrix
            const.dir = constraint_dir, # all less than
            const.rhs = B, # bounds
            all.int = FALSE) # we cant buy fractions of currency

if (model$status == 0) {
  print("Solution Found!")
} else {
  print("No Solution")
}

best_sol <- model$solution
names(best_sol) <- c("shares")
print(best_sol)


optimal_shares <- model$solution
curr_price
investment

exit_strategy <- function(curr_price, investment, r_fee = .005, d_return, optimal_shares) {
  paid <- ((optimal_shares*curr_price)*(1+r_fee))
  value <- paid*(1+r_fee+d_return)
  exit <- (value/optimal_shares)
  return(round(exit,4))
}

exit_strategy(curr_price, investment, d_return = .05, optimal_shares = optimal_shares)
