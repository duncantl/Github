gitLog =
function(dir = ".", ...)    
{
    if(dir != ".") {
        cur = getwd()
        on.exit(setwd(cur))
        setwd(dir)
    }

    args = unlist(list(...))
    
    #    system("git log", intern = TRUE)
    system2("git", args = c("log", args), stdout = TRUE)
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
function(log = gitLog(..., "--numstat", "--oneline"), ...)
{
    
}
