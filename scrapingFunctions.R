findrating <- function(shopid) {
    fileURL <- paste("http://www.ipeen.com.tw/shop/", shopid, "/comments?sortway=d&so=shop_default", sep = "")
    download.file(fileURL, destfile = "defaultpage")
    doc <- htmlTreeParse("defaultpage", useInternal = TRUE)
    messydata <- xpathSApply(doc, "//ul/li[3]/a/@data-label")
    commentcount <- as.numeric(gsub("[^0-9]", "", unlist(messydata[11])))
    
    pagecount <- ceiling(commentcount/5)
    commentpaths <- c()
    allratings <- c()
    
    for (i in 1:pagecount) {
        fileURL <- paste("http://www.ipeen.com.tw/shop/", shopid, "/comments?p=", i, "&sortway=d&so=shop_default", sep = "")
        download.file(fileURL, destfile = "commentlist")
        doc <- htmlTreeParse("commentlist", useInternal = TRUE)
        commentpaths <- append(commentpaths, xpathSApply(doc, "//h2/a/@href"))
    }
    
    commentpaths <- commentpaths[!(commentpaths == "/")]
    
    for (i in 1:length(commentpaths)) {
        fileURL <- paste("http://www.ipeen.com.tw", commentpaths[i], sep = "")
        download.file(fileURL, destfile = "commentpage")
        doc <- htmlTreeParse("commentpage", useInternal = TRUE)
        ratings <- xpathSApply(doc, "//i/@class")
        toprating <- ratings[1]
        rating <- paste(substr(toprating, 3, 3), ".", substr(toprating, 4, 4), sep = "")
        allratings <- append(allratings, rating)
    }
    
    allratings <- as.numeric(allratings)
    ratingstore <<- allratings
    return(mean(allratings))

}



findhomecity <- function(shopid) {
    fileURL <- paste("http://www.ipeen.com.tw/shop/", shopid, "/comments?sortway=d&so=shop_default", sep = "")
    download.file(fileURL, destfile = "defaultpage")
    doc <- htmlTreeParse("defaultpage", useInternal = TRUE)
    messydata <- xpathSApply(doc, "//ul/li[3]/a/@data-label")
    commentcount <- as.numeric(gsub("[^0-9]", "", unlist(messydata[11])))
    
    pagecount <- ceiling(commentcount/5)
    commenterpaths <- c()
    cities <- c()
    
    for (i in 1:pagecount) {
        fileURL <- paste("http://www.ipeen.com.tw/shop/", shopid, "/comments?p=", i, "&sortway=d&so=shop_default", sep = "")
        download.file(fileURL, destfile = "commentlist")
        doc <- htmlTreeParse("commentlist", useInternal = TRUE)
        commenterpaths <- append(commenterpaths, xpathSApply(doc, "//address/p/a/@href"))
    }
    
    for (i in 1:length(commenterpaths)) {
        fileURL <- paste("http://www.ipeen.com.tw", commenterpaths[i], sep = "")
        download.file(fileURL, destfile = "commenterpage")
        doc <- htmlTreeParse("commenterpage", useInternal = TRUE)
        cities <- append(cities, xpathSApply(doc, "//ul/li[1]/span/span", xmlValue))
    }
    
return(cities)
}
