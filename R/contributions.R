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
    
    range = format(c(from, to), "%Y-%m-%d")
    html = getForm(sprintf("https://github.com/users/%s/contributions", user), from = range[1], to = range[2])

    doc = htmlParse(html)
    ans = parseContributions(doc)

    ans[ ans$date <= to, ]
}

parseContributions =
function(doc)
{
    td = getNodeSet(doc, "//table//td[@data-date]")

    tmp = gsub(" .*", "", sapply(td, xmlValue))
#browser()
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

