# 5.2.5
all2006 <- read.csv("2006.csv", header = TRUE, as.is = TRUE)
all2006 <- all2006[all2006$Wage_Per == "Year", ] # exclude hourly-wagers
all2006 <- all2006[all2006$Wage_Offered_From > 20000, ] # exclude weird cases
all2006 <- all2006[all2006$Prevailing_Wage_Amount > 200, ] # exclude hrly prv wg
all2006$rat <- all2006$Wage_Offered_From / all2006$Prevailing_Wage_Amount

medrat <- function(dataframe){
        return(median(dataframe$rat, na.rm = TRUE))
}

se2006 <- all2006[grep("Software Engineer", all2006), ]
prg2006 <- all2006[grep("Programmer", all2006), ]
ee2006 <- all2006[grep("Electronics Engineer", all2006), ]

makecorp <- function(corpname){
        t <- all2006[all2006$Employer_Name == corpname, ]
        return(t)
}

corplist <- c("MICROSOFT CORPORATION", "ms", "INTEL CORPORATION", "intel", 
              "SUN MICROSYSTEMS, INC.", "sun", "GOOGLE INC.", "google")

for (i in 1:(length(corplist) / 2)){
        corp <- corplist[2 * i - 1]
        newdtf <- paste(corplist[2 * i], "2006", sep = "")
        assign(newdtf, makecorp(corp), pos = .GlobalEnv)
}

# 5.3
count.fields("DA", sep = ",")
all(count.fields("DA", sep = ",") >= 5)
table(count.fields("DA", sep = ","))
da <- read.csv("DA", header = TRUE, stringsAsFactors = FALSE)
db <- read.csv("DB", header = FALSE, stringsAsFactors = FALSE)
for (col in 1:6)
        print(unique(sort(da[, col])))
mrg <- merge(da, db, by.x = 1, by.y = 1)

# 5.4.2 
aba <- read.csv("abalone.data", header = TRUE)
abamf <- aba[aba$Gender != "I", ] # exclude infants from the analysis
lftn <- function(clmn){
        glm(abamf$Gender ~ clmn, family = binomial)$coef
}
loall <- sapply(abamf[, -1], lftn)

# 5.4.3
# merges data frames for 2 fangyans
merge2fy <- function(fy1, fy2){
        outdf <- merge(fy1, fy2)
        #seperate tone from sound, and create new columns
        for (fy in list(fy1, fy2)){
                # saplout will be a matrix, init cons in row 1, remainders in row
                # 2, and tones in row 3
                saplout <- sapply((fy[[2]]), sepsoundtone)
                # convert it to a data frame
                tmpdf <- data.frame(fy[, 1], t(saplout), row.names = NULL,
                                    stringsAsFactors = F)
                #add names to the columns
                consname <- paste(names(fy)[[2]], " cons", sep = "")
                restname <- paste(names(fy)[[2]], " sounds", sep = "")
                tonename <- paste(names(fy)[[2]], " tone", sep = "")
                names(tmpdf) <- c("Ch char", consname, restname, tonename)
                # need to use merge(), not cbind(), due to possibly different
                # ordering of fy, outdf
                outdf <- merge(outdf, tmpdf)
        }
        return(outdf)
}

# separates romanized pronunciation pronun into initial consonant, if any, 
# the remainder of the sound, and the tone, if any
sepsoundtone <- function(pronun){
        nchr <- nchar(pronun)
        vowels <- c("a", "e", "i", "o", "u")
        # how many initial consonants?
        numcons <- 0
        for (i in 1:nchr){
                ltr <- substr(pronun, i, i)
                if (!ltr %in% vowels) numcons <- numcons + 1 else break
        }
        cons <- if (numcons > 0) substr(pronun, 1, numcons) else NA
        tone <- substr(pronun, nchr, nchr)
        numtones <- tone %in% letters # T is 1, F is 0
        if (numtones == 1) tone <- NA
        therest <- substr(pronun, numcons + 1, nchr - numtones)
        return(c(cons, therest, tone))
}

mapsound <- function(df, fromcol, tocol, sourceval){
        base <- which(df[[fromcol]] == sourceval)
        basedf <- df[base, ]
        #determine which rows of basedf correspond to the various mapped values
        sp <- split(basedf, basedf[[tocol]])
        retval <- list()
        retval$counts <- sapply(sp, nrow)
        retval$images <- sp
        return(retval)
}