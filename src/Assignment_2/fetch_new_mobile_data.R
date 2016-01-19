library(XML)

scrap_mobile_prices <- function (brand) {
    brand_name = brand
    brand_name <- paste(brand_name, "_Mobiles_Prices", sep = "")
    file_URL <- "http://www.whatmobile.com.pk/"
    file_URL <- paste(file_URL, brand_name, sep = "")
    doc <- htmlTreeParse(file_URL, useInternal = TRUE)
    
    title <- xpathSApply(doc, "//a[@class='BiggerText']", xmlValue)
    price <- xpathSApply(doc, "//td[@class='BiggerText']
                         /span[@class='PriceFont']", xmlValue)
    
    
    dfrm <- data.frame(Title = trimws(title), Price = price, Brand = brand)
    if (brand == "Nokia")
      write.table(dfrm, file = "new_mobile_prices.csv", sep = ",",
                row.names = FALSE, col.names = TRUE)
    else write.table(dfrm, file = "new_mobile_prices.csv", append = TRUE, sep = ",",
                     row.names = FALSE, col.names = FALSE)
    
}
scrap_mobile_prices("Nokia")

scrap_mobile_prices("Samsung")

scrap_mobile_prices("Voice")

scrap_mobile_prices("Microsoft")

scrap_mobile_prices("Huawei")

scrap_mobile_prices("HTC")

scrap_mobile_prices("Qmobile")

scrap_mobile_prices("Rivo")

scrap_mobile_prices("Mobilink")

scrap_mobile_prices("GRight")

scrap_mobile_prices("Club")

scrap_mobile_prices("Oppo")

scrap_mobile_prices("Calme")

scrap_mobile_prices("Lenovo")

scrap_mobile_prices("OPhone")

scrap_mobile_prices("SonyEricsson")

scrap_mobile_prices("LG")

scrap_mobile_prices("Apple")

scrap_mobile_prices("BlackBerry")

scrap_mobile_prices("Haier")

scrap_mobile_prices("iNew")

scrap_mobile_prices("Motorola")

scrap_mobile_prices("GFive")