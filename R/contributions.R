if(FALSE) {

    d = c("2020-1-1", "2021-1-1", "2022-1-1", "2023-1-1", format(Sys.Date(), "%Y-%m-%d"))
    z = mapply(function(a, b)
                 contributions("duncantl", a, b),
               d[ - length(d) ], d[-1], SIMPLIFY = FALSE)

    z2 = do.call(rbind, z)
    with(z2[order(z2$date),], plot(date, count, type = "l"))

}


contributions =
    # https://github.com/users/duncantl/contributions?from=2022-11-30&to=2022-12-30
function(user, from = Sys.Date() - 365, to = Sys.Date())
{
    #
    # Handle more than one year.
    #  e.g., from  5/21 to now
    #        from 2022 to now
    #        from 2021 to 2021
    #        from 2021 to 5/2022
    #        from 5/2021 to 5/2022
    #

    if(is.character(from))
        from = as.Date(from, "%Y-%m-%d")

    if(is.character(to))
        to = as.Date(to, "%Y-%m-%d")        

    dates = mkDateSeq(from, to)

    if(length(dates) > 2) {
        ans = do.call(rbind, mapply(function(a, b) getContributions(user, a, b),
                                    dates[-length(dates)],
                                    # have go from start to end of that year, not 1st day of next year
                                    # so subtract 1 day.  But for the last date, need to leave it as is.
                                    dates[-1] - c(rep(1, length(dates) - 2), 0),
                                    SIMPLIFY = FALSE))
        ans[ ans$date >= from & ans$date <= to, ]
    } else
        getContributions(user, from, to)
}


getContributions =
function(user, from, to)        
{
    range = format(c(from, to), "%Y-%m-%d")
    
    html = getForm(sprintf("https://github.com/users/%s/contributions", user), from = range[1], to = range[2])

    doc = htmlParse(html)
    ans = parseContributions(doc)

    ans[ ans$date >= from & ans$date <= to, ]
}

mkDateSeq =
    # Given a start and end date, create a sequence
    # of mid points that are 1 year apart.
function(a, b)
{
    if(format(a, "%m/%d") != "01/01") 
        a = as.Date( paste0(format(a, "%Y"), "/1/1"), "%Y/%m/%d")
    
    sq = seq(a, b, by = "year") # 365)
    if(sq[ length(sq) ] < b)
        sq = c(sq, b)    

    sq
    
#  if(b - a
#  yr = format(c(a, b), "%Y%")
#  browser()
#  yr = unique(yr)
#  if(length(yr)

}


parseContributions =
function(doc)
{
    td = getNodeSet(doc, "//table//td[@data-date]")

    tmp = gsub(" .*", "", sapply(td, xmlValue))

    # count = as.integer(tmp) 
    # count[is.na(count)] = 0
    # gives a warning about NAs
    # Want to avoid this.
    
    count = rep(0, length(td))
    w = (tmp != "No")
    count[w] = as.integer(tmp[w])

    dates = sapply(td, xmlGetAttr, "data-date")
    ans = data.frame(date = as.Date(dates, "%Y-%m-%d"),
                     count = count)

    ans[order(ans$date), ]
}

