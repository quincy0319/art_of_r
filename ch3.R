# 3.2.3
library(pixmap)
mtrush1 <- read.pnm("mtrush1.pgm")
mtrush1

# adds random noise to img
blurpart <- function(img, rows, cols, q){
        lrows <- length(rows)
        lcols <- length(cols)
        newimg <- img
        randomnoise <- matrix(nrow = lrows, ncol = lcols, runif(lrows * lcols))
        newimg@grey <- (1 - q) * img@grey + q * randomnoise
        return(newimg)
}

# 3.2.5 
makecov <- function(rho, n){
        m <- matrix(nrow = n, ncol = n)
        m <- ifelse(row(m) == col(m), 1, rho)  # 对角线元素为1，其余为rho
        return(m)
}