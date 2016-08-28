# 7.1.2
u <- matrix(c(1, 2, 3, 1, 2, 4), nrow = 3, ncol = 2)
v <- matrix(c(8, 12, 20, 15, 10, 2), nrow = 3, ncol = 2)
for (m in c("u", "v")){
        z <- get(m)
        print(lm(z[, 2] ~ z[, 1]))
}

# 7.5
g1 <- function(x) return(sin(x))
g2 <- function(x) return(sqrt(x ^ 2 + 1))
g3 <- function(x) return(2 * x - 1)
plot(c(0, 1), c(-1, 1.5)) # prepare the graph, specifying X and Y ranges
for (f in c(g1, g2, g3)) plot(f, 0, 1, add = T) # add plot to existing graph

g <- function(h, a, b) h(a, b)
body(g) <- quote(2 * x + 3)
g

# 7.6.5
f <- function(){
        a <- 1
        return(g(a) + a)
}
g <- function(aa){
        b <- 2
        aab <- h(aa + b)
        return(aab)
}
h <- function(aaa){
        c <- 3
        return(aaa + c)
}

# shows the values of the local variables (including arguments) of the 
# frame upn frames above the one from which showframe() is called; if
# upn < 0, the globals are shown; function objects are not shown
showframe <- function(upn){
        # determine the proper environment
        if (upn <0){
                env <- .GlobalEnv
        } else {
                env <- parent.frame(n = upn + 1)
        }
        # get the list of variable names
        vars <- ls(envir = env)
        # for each variable name, print its value
        for (vr in vars){
                vrg <- get(vr, envir = env)
                if (!is.function(vrg)){
                        cat(vr, ":\n", sep = "")
                        print(vrg)
                }
        }
}

# 7.8.3
# DES.R:  R routines for discrete-event simulation (DES)

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype, a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm,evntty,appin=NULL) {
        rw <- c(list(evnttime=evnttm,evnttype=evntty),appin)
        return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm,evntty,appin=NULL) {
        newevnt <- evntrow(evnttm,evntty,appin)
        # if the event list is empty, set it to consist of evnt and return
        if (is.null(sim$evnts)) {
                sim$evnts <<- newevnt
                return()
        }
        # otherwise, find insertion point
        inspt <- binsearch((sim$evnts)$evnttime,evnttm) 
        # now "insert," by reconstructing the data frame; we find what
        # portion of the current matrix should come before the new event and
        # what portion should come after it, then string everything together
        before <- 
                if (inspt == 1) NULL else sim$evnts[1:(inspt-1),]
        nr <- nrow(sim$evnts)
        after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
        sim$evnts <<- rbind(before,newevnt,after)
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x,y) {
        n <- length(x)
        lo <- 1
        hi <- n
        while(lo+1 < hi) {
                mid <- floor((lo+hi)/2)
                if (y == x[mid]) return(mid)
                if (y < x[mid]) hi <- mid else lo <- mid
        }
        if (y <= x[lo]) return(lo)
        if (y < x[hi]) return(hi)
        return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
        head <- sim$evnts[1,]
        # delete head
        if (nrow(sim$evnts) == 1) {
                sim$evnts <<- NULL
        } else sim$evnts <<- sim$evnts[-1,]
        return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,
                  dbg=FALSE) {
        sim <<- list()
        sim$currtime <<- 0.0  # current simulated time
        sim$evnts <<- NULL  # events data frame
        sim$dbg <<- dbg
        initglbls(apppars)
        while(sim$currtime < maxsimtime) {  
                head <- getnextevnt()
                sim$currtime <<- head$evnttime  # update current simulated time
                reactevnt(head)  # process this event 
                if (dbg) print(sim)
        }
        prntrslts()
}

# DES application:  M/M/1 queue, arrival rate 0.5, service rate 1.0

# the call 
# dosim(mm1initglbls,mm1reactevnt,mm1prntrslts,10000.0,
#    list(arrvrate=0.5,srvrate=1.0))
# should return a value of about 2 (may take a while)

# initializes global variables specific to this app
mm1initglbls <- function(apppars) {
        mm1glbls <<- list()
        # simulation parameters
        mm1glbls$arrvrate <<- apppars$arrvrate
        mm1glbls$srvrate <<- apppars$srvrate
        # server queue, consisting of arrival times of queued jobs
        mm1glbls$srvq <<- vector(length=0) 
        # statistics
        mm1glbls$njobsdone <<- 0  # jobs done so far
        mm1glbls$totwait <<- 0.0  # total wait time so far
        # set up first event, an arrival; the application-specific data for
        # each event will consist of its arrival time, which we need to
        # record in order to later calculate the job's residence time in the
        # system
        arrvtime <- rexp(1,mm1glbls$arrvrate)
        schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
}

# application-specific event processing function called by dosim()
# in the general DES library 
mm1reactevnt <- function(head) {
        if (head$evnttype == "arrv") {  # arrival
                # if server free, start service, else add to queue (added to queue
                # even if empty, for convenience)
                if (length(mm1glbls$srvq) == 0) {
                        mm1glbls$srvq <<- head$arrvtime
                        srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
                        schedevnt(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
                } else mm1glbls$srvq <<- c(mm1glbls$srvq,head$arrvtime)
                # generate next arrival
                arrvtime <- sim$currtime + rexp(1,mm1glbls$arrvrate)
                schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
        } else {  # service done
                # process job that just finished
                # do accounting
                mm1glbls$njobsdone <<- mm1glbls$njobsdone + 1
                mm1glbls$totwait <<- 
                        mm1glbls$totwait + sim$currtime - head$arrvtime
                # remove from queue
                mm1glbls$srvq <<- mm1glbls$srvq[-1]
                # more still in the queue?
                if (length(mm1glbls$srvq) > 0) {
                        # schedule new service
                        srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
                        schedevnt(srvdonetime,"srvdone",list(arrvtime=mm1glbls$srvq[1]))
                }
        }
}

mm1prntrslts <- function() {
        print("mean wait:")
        print(mm1glbls$totwait/mm1glbls$njobsdone)
}

# 7.9.1
qs <- function(x){
        if (length(x) <= 1) return(x)
        pivot <- x[1]
        therest <- x[-1]
        sv1 <- therest[therest < pivot]
        sv2 <- therest[therest >= pivot]
        sv1 <- qs(sv1)
        sv2 <- qs(sv2)
        return(c(sv1, pivot, sv2))
}


# 7.9.2
# routines to create trees and insert items into them are included
# below; a deletion routine is left to the reader as an exercise

# storage is in a matrix, say m, one row per node of the tree; if row 
# i contains (u,v,w), then node i stores the value w, and has left and
# right links to rows u and v; null links have the value NA 

# the tree is represented as a list (mat,nxt,inc), where mat is the
# matrix, nxt is the next empty row to be used, and inc is the number of
# rows of expansion to be allocated whenever the matrix becomes full

# print sorted tree via in-order traversal
printtree <- function(hdidx,tr) {  
        left <- tr$mat[hdidx,1]
        if (!is.na(left)) printtree(left,tr)  
        print(tr$mat[hdidx,3])  # print root
        right <- tr$mat[hdidx,2]
        if (!is.na(right)) printtree(right,tr)  
}

# initializes a storage matrix, with initial stored value firstval
newtree <- function(firstval,inc) {
        m <- matrix(rep(NA,inc*3),nrow=inc,ncol=3)
        m[1,3] <- firstval
        return(list(mat=m,nxt=2,inc=inc))
}

# inserts newval into the subtree of tr, with the subtree's root being
# at index hdidx; note that return value must be reassigned to tr by the
# caller (including ins() itself, due to recursion)
ins <- function(hdidx,tr,newval) {  
        # which direction will this new node go, left or right?
        dir <- if (newval <= tr$mat[hdidx,3]) 1 else 2
        # if null link in that direction, place the new node here, otherwise
        # recurse
        if (is.na(tr$mat[hdidx,dir])) {  
                newidx <- tr$nxt  # where new node goes
                # check for room to add a new element
                if (tr$nxt == nrow(tr$mat) + 1) {  
                        tr$mat <- 
                                rbind(tr$mat, matrix(rep(NA,tr$inc*3),nrow=tr$inc,ncol=3))
                }
                # insert new tree node
                tr$mat[newidx,3] <- newval
                # link to the new node
                tr$mat[hdidx,dir] <- newidx
                tr$nxt <- tr$nxt + 1  # ready for next insert
                return(tr)
        } else tr <- ins(tr$mat[hdidx,dir],tr,newval)
}