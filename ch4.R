# 4.2.4
findwords <- function(tf){
        # read in the words from the file, into a vector of mode character
        txt <- scan(tf, "")
        wl <- list()
        for (i in 1:length(txt)){
                wrd <- txt[i] # ith word in input file
                wl[[wrd]] <- c(wl[[wrd]], i)
        }
        return(wl)
} 

# 4.2.4
# sorts wrdlst, the output of findwords() alphabetically by word
# 按字母顺序排列
alphawl <- function(wrdlst){
        nms <- names(wrdlst) # the words
        sn <- sort(nms) # same words in alpha order
        return(wrdlst[n]) # return rearranged version
}

# orders the output of findwords() by frequency
# 按词频排序
freqwl <- function(wrdlst){
        freqs <- sapply(wrdlst, length) # get word frequencies
        return(wrdlst[order(freqs)])
}

