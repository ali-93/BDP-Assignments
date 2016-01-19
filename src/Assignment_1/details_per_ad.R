library(XML)
library(RCurl)
library(curl)
library(stringr)

  

olx <- read.csv(file = "detail_links.csv", header = TRUE, sep = ",")

#olx is the dataset of 500 pages, import it first
for (i in 1:20580) {
  print(i)
  
  detailDoc <- htmlTreeParse(olx$Details_Link[i], useInternal = TRUE)
  
  city <- xpathSApply(detailDoc, "//span[@class='show-map-link link gray cpointer']/
                      strong[@class='c2b small']", xmlValue)
  
  adID <- xpathSApply(detailDoc, "//span[@class='pdingleft10 brlefte5']/
                      span[@class='nowrap']/span[@class='rel inlblk']", xmlValue)
  
  username <- xpathSApply(detailDoc, "//p[@class='userdetails fleft marginleft15']/
                          span[@class='block color-5 brkword xx-large']", xmlValue)
  
  brand <- xpathSApply(detailDoc, "//div[@class='pding5_10']/strong[@class='block']/a", xmlValue)

  views <- xpathSApply(detailDoc, "//div[@class='pdingtop10']/strong", xmlValue)
  
  description <- xpathSApply(detailDoc, "//div[@id='textContent']/
                             p[@class='pding10 lheight20 large']", xmlValue)
  
  phone <- xpathSApply(detailDoc, "//strong[@class='large lheight20 fnormal  ']", xmlValue)
  
  isVerified <- xpathSApply(detailDoc, "//div[@class='fleft']/span[@class='icon vmiddle 
                            inlblk verified-num-pc-itempage']", xmlValue)

 
  picture_1 <- xpathSApply(detailDoc, "//a[@class='block br5 {nr:1}']", xmlGetAttr, "href")
  picture_2 <- xpathSApply(detailDoc, "//a[@class='block br5 {nr:2}']", xmlGetAttr, "href")
  
  if (length(picture_1) != 0) {
    img_1 <- paste("D:/GitRepo~/Assignment_2/images/", i, "_1.jpg")
    download.file(picture_1, destfile=img_1, quiet = TRUE, method="libcurl")
  } else { picture_1 <- "NA"}
  if (length(picture_2) != 0) {
    img_2 <- paste("D:/GitRepo~/Assignment_2/images/", i, "_2.jpg")
    download.file(picture_2, destfile=img_2, quiet = TRUE, method="libcurl")
  }
  else { picture_2 <- "NA"}
  
  if (is.null(views)) {
    views <- NA
  }
  if (length(picture_1) == 0)
    picture_1 <- "NA"
  if (length(picture_2) == 0)
    picture_2 <- "NA"
    

  if (length(phone) == 0) { 
    phone <- NA
  }
  if (is.null(username)) {
    username <- NA
  }
  
  if(is.null(isVerified))
    isVerified <- NA
  else isVerified <- "YES"
  
  if (is.null(brand)) {
    brand <- NA
  }

  if (is.null(adID))
    adID <- NA
  if (is.null(city))
    city <- NA
  
  if (is.null(description))
    description <- NA
  
  
  dfrm <- data.frame(City = city, Ad_ID = adID, Username = username, Brand = brand, 
                     Views = views, Description = description, Phone = phone, Verified = isVerified, 
                     Picture_1 = picture_1, Picture_2 = picture_2)
  if (i == 1) {
    write.table(dfrm, file = "ad_details.csv" ,sep = ",", row.names = FALSE)  
  } else {
    write.table(dfrm, file = "ad_details.csv", append = TRUE,sep = ",", row.names = FALSE, col.names=FALSE)
  }
}
