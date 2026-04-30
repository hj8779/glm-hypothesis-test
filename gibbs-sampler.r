set.seed(210520269)

# Config
m <- 10000 # nb iteration
b <- 100 # nb chauffe
# Vecteurs de longueur m alloués dans la mémoire
# pour réduire le nombre de hard copy de array
th1Samples <- numeric(m)
th2Samples <- numeric(m)
x1Samples <- numeric(m)

# Init
theta1 <- 0.5
theta2 <- 0.5
x1 <- 4

for (i in 1 : m) {
    # Mettre à jour theta1 : Beta(x1 + 1, 6 - x1)
    theta1 <- rbeta(1, x1 + 1, 6 - x1)
  
    # Mettre à jour theta2 : Beta(8 - x1, x1 - 2)
    theta2 <- rbeta(1, 8 - x1, x1 - 2)
    
    # Mettre à jour x1
    # Cas 3, 4, 5
    p3 <- 10 * theta1^3 * (1 - theta1)^2 * theta2^4
    p4 <- 20 * theta1^4 * (1 - theta1)^1 * theta2^3 * (1 - theta2)^1
    p5 <- 6 * theta1^5 * theta2^2 * (1 - theta2)^2
    
    sumProb = p3 + p4 + p5
    # y~unif(0,1), u / sumProb = y <=> u = y * sumProb
    u <- runif(1) * sumProb

    # P(y < p3 / sumProb) <=> P(y * sumProb < p3) <=> P(u < p3)
    if (u <= p3) x1 <- 3
    else if (u <= (p3 + p4)) x1 <- 4
    else x1 <- 5

    # Stocker les histoires
    th1Samples[i] <- theta1
    th2Samples[i] <- theta2
    x1Samples[i] <- x1
}

# Période de chauffe : b
# Prendre seules les valeurs après l'echauffement
th1Final <- th1Samples[(b + 1) : m]
th2Final <- th2Samples[(b + 1) : m]
x1Final <- x1Samples[(b + 1) : m]

# Estimation des paramètres à posteriori
probTheta <- mean(th1Final > 0.5 & th2Final > 0.5)
probX  <- mean(x1Final == 4)

cat("P(theta1 > 0.5, theta2 > 0.5 | Y=7) =", probTheta, "\n")
cat("P(X1 = 4 | Y=7) =", probX, "\n")