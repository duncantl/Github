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
    range = format(c(from, to), "%Y-%m-%d")
    html = getForm(sprintf("https://github.com/users/%s/contributions", user), from = range[1], to = range[2]) # , .opts = list(verbose = TRUE))
    doc = htmlParse(html)
    parseContributions(doc)
    
#    tb = readHTMLTable(doc, which = 1)
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

    data.frame(date = as.Date(sapply(td, xmlGetAttr, "data-date"), "%Y-%m-%d"),
               count = count)
}

