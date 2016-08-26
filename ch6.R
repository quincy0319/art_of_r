# 6.2.1
ages <- c(25, 26, 55, 37, 21, 42)
affils <- c("R", "D", "D", "R", "U", "D")
tapply(ages, affils, mean)

d <- data.frame(list(gender = c("M", "M", "F", "M", "F", "F"),
                     ages = c(47, 59, 21, 32, 33, 24), income = c(55000, 88000, 
                        32450, 76500, 123000, 45650)))
d$over25 <- ifelse(d$ages > 25, 1, 0)
tapply(d$income, list(d$gender, d$over25), mean)

# 6.2.2
split(d$income, list(d$gender, d$over25))

findworks <- function(tf){
        # read in the words from the files, into a vector of mode character
        txt <- scan(tf, "")
        words <- split(1:length(txt), txt)
        return(words)
}

# 6.2.3
aba <- read.csv("abalone.data", header = TRUE)
by(aba, aba$Gender, function(m) lm(m[, 2] ~ m[, 3]))

# 6.3 
u <- c(22, 8, 33, 6, 8, 29, -2)
fl <- list(c(5, 12, 13, 12, 13, 5, 13), c("a", "bc", "a", "a", "bc", "a", "a"))
tapply(u, fl, length)

# 6.3.2 subtable()
subtable <- function(tbl, subnames){
        # get array of cell counts in tbl
        tblarray <- unclass(tbl)
        # we'll get the subarray of cell counts corresponding to subnames by
        # calling do.call() on the "[" function; we need to build up a list
        # of arguments first
        dcargs <- list(tblarray)
        ndims <- length(sbunames) # number of dimensions
        for (i in 1:ndims){
                dcargs[[i+1]] <- subnames[[i]]
        }
        subarray <- do.call("[", dcargs)
        # now we'll build the new table, consisting of the subarray, the 
        # numbers of levels in each dimension, and the dimnames() value, plus
        # the "table" class attribute
        dims <- lapply(subnames, length)
        subtbl <- array(subarray, dims, dimnames = subnames)
        class(subtbl) <- "table"
        return(subtbl)
}

#6.3.3
# finds the cells in table tbl with the k highest frequencies; handling
# of ties is unrefined
tabdom <- function(tbl, k){
        # create a data frame representation of tbl, adding a Freq column
        tbldf <- as.data.frame(tbl)
        
}
