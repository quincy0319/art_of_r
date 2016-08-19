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

# 3.3.1
z <- matrix(1 : 6, nrow = 3, ncol = 2)
f <- function(x) x / c(2, 8)
y <- apply(z, 1, f)
w <- t(y) # 转置

# copymaj
function(rw, d){
        maj <- sum(rw[1 : d]) / d
        return(if(maj > 0.5) 1 else 0)
}

# 3.4.2 
# returns the minimum value of d[i, j], i != j
mind <- function(d){
        n <- nrow(d)
        # add a column to identify row number for apply()
        dd <- cbind(d, 1:n)
        wmins <- apply(dd[-n, ], 1, imin)
        # wins will be 2xn, 1st row being indices and 2nd being values
        i <- which.min(wmins[2, ])
        j <- wmins[1, i]
        return(c(d[i, j], i, j))
}

# finds the location, value of the minimum in a row x
imin <- function(x){
        lx <- length(x)
        i <- x[lx] # original row number
        j <- which.min(x[(i + 1):(lx - 1)])
        x <- i + j
        return(c(k, x[k]))
}

# if minimum distance between 2 cities is the only one
# solution below is simpler
minda <- function(d){
        smallest <- min(d)
        ij <- which(d == smallest, arr.ind = TRUE)
        return(c(smallest, ij))
}
