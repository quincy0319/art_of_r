# 2.4.2 select all the elements except the last one
z <- c(5, 12, 13)
z[1:(length(z) - 1)]
z[- length(z)] # simpler version

# 2.5.1 寻找连续出现1的游程
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
        runs <- vector(length = n) # 提前分配内存
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

# 2.5.2 预测离散值时间序列
preda <- function(x, k){
        n <- length(x)
        k2 <- k / 2
        pred <- vector(length = n - k)
        for (i in 1 : (n - k)){
                if (sum(x[i : (i + (k - 1))]) >= k2) pred[i] <- 1 else pred[i] <- 0
        }
        return(mean(abs(pred - x[(k + 1) : n])))
}

# 2.9.1 
# findud() converts vector v to 1s, 0s, representing an element
# increasing or not, relative to the previous one; output length is 1
# less than input

findud <- function(v){
        vud <- v[-1] - v[-length(v)]
        return(ifelse(vud > 0, 1, -1))
}
udcorr <- function(x, y){
        ud <- lapply(list(x, y), findud)
        return(mean(ud[[1]] == ud[[2]]))
}

diff()
sign()

# 2.9.2
ifelse(g == "M", 1, ifelse(g == "F", 2, 3))
ab[, 1] <- ifelse(ab[, 1] == "M", 1, ifelse(ab[, 1] == "F", 2, 3))
m <- which(g == "M")
f <- which(g == "F")
i <- which(g == "I")

grps <- list()
for (gen in c("M", "F", "I")) grps[[gen]] <- whic(g == gen)

