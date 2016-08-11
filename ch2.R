# 2.5.1
findruns <- function(x, k){
        n <- length(x)
        runs <- NULL
        for (i in 1 : (n-k+1)){
                if (all(x[i : (i + k - 1)] == 1)) runs <- c(runs,i)
        }
        return(runs)
}

findruns1 <- function(x,k){
        n <- length(x)
        runs <- vector(length = n)
        count <- 0
        for (i in 1 : (n - k + 1)){
                if (all(x[i : (i + k - 1)] == 1)){
                        count <- count + 1
                        runs[count] <- i
                }
        }
        if(count > 0){
                runs <- runs[1 : count]
        }else runs <- NULL
        ruturn(runs)
}

# 2.5.2
preda <- function(x, k){
        n <- length(x)
        k2 <- k / 2
        pred <- vector(length = n - k)
        for (i in 1 : (n - k)){
                if (sum(x[i : (i + (k - 1))]) >= k2) pred[i] <- 1 else pred[i] <- 0
        }
        return(mean(abs(pred - x[(k + 1) : n])))
}