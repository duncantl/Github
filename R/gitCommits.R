# This should use the git API, not system2 calls.

gitLog =
function(dir = ".", ..., .args = unlist(list(...)))
{
    if(dir != ".") {
        cur = getwd()
        on.exit(setwd(cur))
        setwd(dir)
    }

    #    system("git log", intern = TRUE)
    system2("git", args = c("log", .args), stdout = TRUE)
}

gitLogDates =
function(log = gitLog(...), ...)
{
    d = trimws(gsub("^Date:", "", grep("^Date:", log, value = TRUE)))
    as.POSIXct(strptime(d, "%a %b %d %H:%M:%S %Y")) # , tz = "PDT"
}

gitUser =
function(log = gitLog(...), ...)
{
   trimws(gsub("^Author:", "", grep("^Author:", log, value = TRUE)))
}

density.POSIXct =
function(x, ...)    
{
    ans = density(as.numeric(x))
    class(ans) = c("POSIXctDensity", class(ans))
    attr(ans, "range") = range(x)
    ans
}

plot.POSIXctDensity =
function(x, y, ylab = "density", ...)
{
    r = attr(x, "range")
    h = seq(r[1], r[2], length = length(x$y))
    plot(h, x$y, type = "l", ylab = ylab, ...)
}



gitChanges =
function(log = gitLog(..., .args = c("--numstat", "--oneline")), ...)
{
    w = grepl("\\t", log)
    com = split(log, cumsum(!w))
    ans = do.call(rbind, lapply(com, mkCommitDf))
    names(ans) = c("additions", "deletions", "file", "hash")
    ans
}

mkCommitDf =
function(x)    
{
    if(length(x) == 1)
        return(data.frame(additions = integer(), deletions = integer(), file = character(), hash = character()))
    
    d = read.table( textConnection( x[-1] ) )
    hash = gsub(" .*", "", x[1])
    d$hash = hash
    d
}
