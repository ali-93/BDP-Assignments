library(XML)

fileURL <-"http://olx.com.pk/lahore/mobile-phones/?page=1"
for (i in 1:500) {
  if (i > 1) {
    fileURL <-"http://olx.com.pk/lahore/mobile-phones/?page="
    fileURL <- paste(fileURL, i, sep = "")
  }
  doc <- htmlTreeParse(fileURL, useInternal = TRUE)
  title <- xpathSApply(doc, "//h3[@class='large lheight20 margintop10']/a/span", xmlValue)
  hrefs <- xpathSApply(doc, "//h3[@class='large lheight20 margintop10']/a", xmlGetAttr, 'href')
  
  price <- xpathSApply(doc, "//p[@class='price x-large margintop10']/strong", xmlValue)

  category <- xpathSApply(doc, "//small[@class='breadcrumb small']", xmlValue)

  dateOfAdd <- xpathSApply(doc, "//td[@valign='bottom']/p", xmlValue)

  
  dfrm <- data.frame(Product_Title = title, Price = price, Category = category,
                     Date_Of_Add = dateOfAdd, Details_Link = hrefs)
  
  if (i == 1) {
    write.table(dfrm, file = "detail_links.csv", sep = ",", row.names = FALSE)
  }
  else {
    write.table(dfrm, file = "detail_links.csv", append = TRUE, sep = ",", row.names = FALSE, col.names=FALSE)
  }
  
}