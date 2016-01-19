
olx_data <- read.csv(file = "olx_ad_details.csv", header = TRUE, sep = ",")

new_mobile_data <- read.csv(file = "new_mobile_prices.csv", header = TRUE, sep = ",")


olx_titles <- character(length = length(olx_data$Product_Title))
for (i in 1 : length(olx_titles)) {
    olx_titles[i] <- sprintf("%s", olx_data$Product_Title[i])
}

sort(olx_titles, na.last = TRUE)
new_mobile_titles <- character(length = length(new_mobile_data$Title))
for (i in 1 : length(new_mobile_titles)) {
  new_mobile_titles[i] <- sprintf("%s", new_mobile_data$Title[i])
}
sort(new_mobile_titles, na.last = TRUE)
result_length = 0
for (i in 1 : length(new_mobile_titles)) {
    patt <- new_mobile_titles[i]
    results <- grep(patt, olx_titles,
                    value = TRUE, ignore.case = TRUE)
    if (length(results) > result_length) {
      for (j in  1 : length(olx_data$Product_Title)) {
          if (results == olx_data$Product_Title[j]) {
              old_price = trimws(olx_data$Price[j])
              ad_id = trimws(olx_data$Ad_ID[j])
              username = trimws(olx_data$Username[j])
              city = trimws(olx_data$City[j])
              views = trimws(olx_data$Views[j])
              phone = olx_data$Phone[j]
          }
      }
      for (j in 1 : length(new_mobile_data$Title)) {
          if (patt == new_mobile_data$Title[j]) {
              new_price = new_mobile_data$Price[j]
              brand = new_mobile_data$Brand[j]
          }
      }
      dfrm <- data.frame(Olx_Title = results, New_Title = patt, Old_Price = old_price, 
                         New_Price = new_price, Brand = brand,
                         City = city, Ad_Id = ad_id, Username = username, Phone = phone,
                         Views = views)
      write.table(dfrm, file = "olx_mapped_titles.csv", append = TRUE, 
                         sep = ",", row.names = FALSE, col.names = FALSE)
    }
    result_length = length(results)
}