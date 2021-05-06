# SIT 718 Assignment 3

# Question 2

library(lpSolveAPI) 

FactoryModel= make.lp(0,9)

lp.control(FactoryModel, sense="maximize")

set.objfn(FactoryModel, c(25, 10, 5, 21, 6, 1, 25, 10, 5))

# Cotton_Spring
add.constraint(FactoryModel, c(-0.45, 0.55, 0.55, 0, 0, 0, 0, 0, 0), "<=", 0)

# Wool_Spring
add.constraint(FactoryModel, c(0.3, -0.7, 0.3, 0, 0, 0, 0, 0, 0), "<=", 0)

# Cotton_Autumn
add.constraint(FactoryModel, c(0, 0, 0, -0.55, 0.45, 0.45, 0, 0, 0), "<=", 0)

# Wool_Autumn
add.constraint(FactoryModel, c(0, 0, 0, 0.4, -0.6, 0.4, 0, 0, 0), "<=", 0)

# Cotton_Winter
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, -0.7, 0.3, 0.3), "<=", 0)

# Wool_Winter
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, 0.5, -0.5, 0.5), "<=", 0)

#Demand_Spring
add.constraint(FactoryModel, c(1, 1, 1, 0, 0, 0, 0, 0, 0), "<=", 4800)

#Demand_Autumn
add.constraint(FactoryModel, c(0, 0, 0, 1, 1, 1, 0, 0, 0), "<=", 3000)

#Demand_Winter
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, 1, 1, 1), "<=", 3500)

#None Negative
add.constraint(FactoryModel, c(1, 0, 0, 0, 0, 0, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 1, 0, 0, 0, 0, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 1, 0, 0, 0, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 1, 0, 0, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 0, 1, 0, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 1, 0, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, 1, 0, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, 0, 1, 0), ">=", 0)
add.constraint(FactoryModel, c(0, 0, 0, 0, 0, 0, 0, 0, 1), ">=", 0)
#Setting up names of constraints and variables

constraintNames <- c("Cotton_Spring", "Wool_Spring", "Cotton_Autumn", "Wool_Autumn", "Cotton_Winter", "Wool_Winter", "Demand_Spring", "Demand_Autumn", "Demand_Winter", "1", "2", "3", "4", "5", "6","7", "8", "9")
variableNames <- c("SpringCutton","SpringWool", "SpringSilk", "AutumnCutton","AutumnWool", "AutumnSilk", "WinterCutton","WinterWool", "WinterSilk")
dimnames(FactoryModel) <- list(constraintNames, variableNames)

# Solves the model
solve(FactoryModel)

get.objective(FactoryModel)

get.variables(FactoryModel)

get.constraints(FactoryModel) 

FactoryModel

write.lp(FactoryModel, filename = "FactoryModel.lp")



# Question 3
# Payoff Matrix

Q3_payoff <- matrix(0, nrow = 7, ncol = 5)
Q3_payoff

Helen <- matrix(c(0,6,1,5,2,4,3,3,4,2,5,1,6,0), nrow = 7, ncol = 2, byrow = TRUE)
David <- matrix(c(0,4,1,3,2,2,3,1,4,0), nrow = 5, ncol = 2, byrow = TRUE)

Stratgy_Helen <- c(1:7)
Stratgy_David <- c(1:5)


for (i in Stratgy_Helen){
  for (j in Stratgy_David){ 
    a = Helen[i,1]-David[j,1]
    b = Helen[i,1]-David[j,2]
    c = Helen[i,2]-David[j,1]
    d = Helen[i,2]-David[j,2]
    
    if (a < 0){
      a = -1
    }else if (a > 0){
      a = 1
    }else{
      a = 0
    }
    
    if (b < 0){
      b = -1
    }else if (b > 0){
      b = 1
    }else{
      b = 0
    }

    if (c < 0){
      c = -1
    }else if (c > 0){
      c = 1
    }else{
      c = 0
    }
    
    if (d < 0){
      d = -1
    }else if (d > 0){
      d = 1
    }else{
      d = 0
    }
    
    Q3_payoff[i,j] <- a + b + c + d
    }  
}

colnames(Q3_payoff) <- c("(0,4)", "(1,3)","(2,2)", "(3,1)", "(4,0)")
rownames(Q3_payoff) <- c("(0,6)", "(1,5)", "(2,4)", "(3,3)", "(4,2)", "(5,1)", "(6,0)")
Q3_payoff

# Helen's Game 
library(lpSolveAPI)

HelenG <- make.lp(0, 5)

lp.control(HelenG, sense= "maximize") 

set.objfn(HelenG, c(0, 0, 0, 0, 1))


add.constraint(HelenG, c(-1, -2, -1, 0, 1), "<=", 0)

add.constraint(HelenG, c(0, -1, -2, -2, 1), "<=", 0)

add.constraint(HelenG, c(0, 0, -2, -4, 1), "<=", 0)

add.constraint(HelenG, c(1, 1, 1, 1, 0), "=", 1)

set.bounds(HelenG, lower = c(0, 0, 0, 0,-Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4")

ColNames <- c("x1", "x2", "x3", "x4","v")

dimnames(HelenG) <- list(RowNames, ColNames)


solve(HelenG) 

get.objective(HelenG)

get.variables(HelenG)

get.constraints(HelenG)

HelenG


# David's Game
library(lpSolveAPI)

DavidG <- make.lp(0, 4)

lp.control(DavidG, sense= "minimize") 

set.objfn(DavidG, c(0, 0, 0,  1))

add.constraint(DavidG, c(-1, 0, 0, 1), ">=", 0)

add.constraint(DavidG, c(-2, -1, 0, 1), ">=", 0)

add.constraint(DavidG, c(-1, -2, -2,  1), ">=", 0)

add.constraint(DavidG, c(0, -2, -4,  1), ">=", 0)

add.constraint(DavidG, c(1, 1, 1, 0), "=", 1)

set.bounds(DavidG, lower = c(0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4", "Row5")

ColNames <- c("y1", "y2", "y3","v")

dimnames(DavidG) <- list(RowNames, ColNames)

solve(DavidG) 

get.objective(DavidG)

get.variables(DavidG)

get.constraints(DavidG)

DavidG

    
# Question 4

Q4_payoff_0 <- matrix(0, nrow = 3, ncol = 3)
Q4_payoff_3 <- matrix(0, nrow = 3, ncol = 3)
Q4_payoff_6 <- matrix(0, nrow = 3, ncol = 3)

Contribution <- c(0,3,6)


for (e in 1:3){
  for (f in 1:3){
    Q4_payoff_0[e,f] = (2*(0+Contribution[e]+Contribution[f]))/3
  }
}

for (e in 1:3){
  for (f in 1:3){
    Q4_payoff_3[e,f] = (2*(3+Contribution[e]+Contribution[f]))/3
  }
}

for (e in 1:3){
  for (f in 1:3){
    Q4_payoff_6[e,f] = (2*(6+Contribution[e]+Contribution[f]))/3
  }
}

Q4_payoff_0
Q4_payoff_3
Q4_payoff_6

Q4_profit_0 <- matrix(0, nrow = 3, ncol = 3)
Q4_profit_3 <- matrix(0, nrow = 3, ncol = 3)
Q4_profit_6 <- matrix(0, nrow = 3, ncol = 3)

for (e in 1:3){
  for (f in 1:3){
    Q4_profit_0[e,f] = ((2*(0+Contribution[e]+Contribution[f]))/3)-0
  }
}

for (e in 1:3){
  for (f in 1:3){
    Q4_profit_3[e,f] = ((2*(3+Contribution[e]+Contribution[f]))/3)-3
  }
}

for (e in 1:3){
  for (f in 1:3){
    Q4_profit_6[e,f] = ((2*(6+Contribution[e]+Contribution[f]))/3)-6
  }
}

Q4_profit_0
Q4_profit_3
Q4_profit_6

Q4_profitsum_0 <- matrix(0, nrow = 3, ncol = 9)
Q4_profitsum_3 <- matrix(0, nrow = 3, ncol = 9)
Q4_profitsum_6 <- matrix(0, nrow = 3, ncol = 9)




