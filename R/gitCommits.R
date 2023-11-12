gitLog =
function()    
{
    system("git log", intern = TRUE)
}

gitLogDates =
function(log = gitLog())
{
    d = trimws(gsub("^Date:", "", grep("^Date:", log, value = TRUE)))
    as.POSIXct(strptime(d, "%a %b %d %H:%M:%S %Y")) # , tz = "PDT"
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

