



# ==================================================================================
#
# Objective:          Analyze the Solow Growth Model
# Author:             Madhav Kumar
# First version:      2014-03-17
# This draft:         2014-03 -17
# Dependency:         None
# Reference:          http://www.unc.edu/~jbhill/Solow-Growth-Model.pdf
#
# ==================================================================================


# ======================================
# Definitions
# n= population growth
# s= savings rate
# d= depreciation
# a= cobb-douglas parameter
# N0= Initial population
# K0= Initial capital stock
# A= Technology parameter
# ======================================



### Consider three economies:

# Economy 1:
n1= 0.02
s1= 0.25
d1= 0.07
a1= 0.75
N01= K01= A1= 1

# Economy 2:
n2= 0.08
s2= 0.25
d2= 0.07
a2= 0.75
N02= K02= A2= 1

# Economy 3:
n3= 0.02
s3= 0.30
d3= 0.07
a3= 0.75
N03= K03= A3= 1


# ======================
# capital Accumulation 
# ======================

CapitalAcc <- function(n, s, d, a, A, k0, t){
  k <- k0
  for (i in 2:length(t)){
    tmp <- ((1 - d)*k[i-1])/(1 + n) + ((s*A)*((k[i-1])^a))/(1 + n)
    k <- c(k, tmp)
  }
  k
}

# calculate accumulation over time
econ1.cap <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= A1, k0= 1, t= 1:300)
econ2.cap <- CapitalAcc(n= n2, s= s2, d= d2, a= a2, A= A2, k0= 1, t= 1:300)
econ3.cap <- CapitalAcc(n= n3, s= s3, d= d3, a= a3, A= A3, k0= 1, t= 1:300)


# plot capital accumulation
plot(econ3.cap, type= "l", col= "green4", main= "Capital accumulation in the \n Solow growth model", xlab= "Time",
     ylab= "Capital per capita")
lines(econ1.cap, col= "blue")
lines(econ2.cap, col= "red")
legend("topleft", legend= c("Econ 3 (High s)", "Econ 1", "Econ 2 (High n)"), lty= c(1, 1, 1), 
       col= c("green4", "blue", "red"), bty= "n", cex= 0.75)



# ======================
# capital growth rate
# ======================

CapitalGrowth <- function(n, s, d, a, A, k0, t){
  grt <- (CapitalAcc(n, s, d, a, A, k0= k0, t= 2)[2] - k0)/(k0)
  for (i in 3:length(t)){
    tmp <- -((n + d)/(1 + n)) + ((s*A)/(1 + n))*(1/(CapitalAcc(n, s, d, a, A, k0= k0, t= 1:i)[i])^(1 - a))
    grt <- c(grt, tmp)
  }
  grt*100
}

# calculate capital growth rate
econ1.cap.grt <- CapitalGrowth(n= n1, s= s1, d= d1, a= a1, A= A1, k0= 1, t= 1:300)
econ2.cap.grt <- CapitalGrowth(n= n2, s= s2, d= d2, a= a2, A= A2, k0= 1, t= 1:300)
econ3.cap.grt <- CapitalGrowth(n= n3, s= s3, d= d3, a= a3, A= A3, k0= 1, t= 1:300)


# plot capital growth rate
plot(econ3.cap.grt, type= "l", col= "green4", main= "Capital growth rates", xlab= "Time",
     ylab= "Capital growth rate %")
lines(econ1.cap.grt, col= "blue")
lines(econ2.cap.grt, col= "red")
legend("topright", legend= c("Econ 3 (High s)", "Econ 1", "Econ 2 (High n)"), lty= c(1, 1, 1), 
       col= c("green4", "blue", "red"), bty= "n", cex= 0.75)




# ======================
# same economy but 
# different technology
# ======================

econ1.tech1 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.50, k0= 1, t= 1:300)
econ1.tech2 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.25, k0= 1, t= 1:300)
econ1.tech3 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.00, k0= 1, t= 1:300)

plot(econ1.tech1, type= "l", col= "green4", main= "Capital accumulation with \n different technology parameter", 
     xlab= "Time", ylab= "Capital per capita")
lines(econ1.tech2, col= "blue")
lines(econ1.tech3, col= "red")
legend("topleft", legend= c("A = 1.5", "A = 1.25", "A = 1"), lty= c(1, 1, 1), 
       col= c("green4", "blue", "red"), bty= "n", cex= 0.75)





# ======================
# same economy but 
# different initial 
# capital
# ======================

econ1.k01 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.00, k0= 8, t= 1:300)
econ1.k02 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.00, k0= 4, t= 1:300)
econ1.k03 <- CapitalAcc(n= n1, s= s1, d= d1, a= a1, A= 1.00, k0= 1, t= 1:300)

plot(econ1.k01, type= "l", col= "green4", main= "Capital accumulation with \n different technology parameter", 
     xlab= "Time", ylab= "Capital per capita", ylim= c(0, 60))
lines(econ1.k02, col= "blue")
lines(econ1.k03, col= "red")
legend("topleft", legend= c("K0 = 8", "K0 = 4", "K0 = 1"), lty= c(1, 1, 1), 
       col= c("green4", "blue", "red"), bty= "n", cex= 0.75)


## End of file
