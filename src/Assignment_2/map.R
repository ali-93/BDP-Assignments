olx_data <- read.csv(file = "olx_ad_details.csv", header = TRUE, sep = ",")

new_mobile_data <- read.csv(file = "new_mobile_phones.csv", header = TRUE, sep = ",")

for (i in 1: length(new_mobile_data$Name)) {
  patterns <- strsplit(as.character(new_mobile_data$Name[i]), " ")[[1]]
  
  for (j in 1: length(patterns)) {
    results <- grep(patterns[j], olx_data$Product_Title, 
                    value = TRUE, ignore.case = TRUE)
  }
}
results <- unique(results)

for (i in 1: length(results)) {
  title <- results[i]
  price <- trimws(get_price(olx_data, results[i]))
  brand <- trimws(get_brand(olx_data, results[i]))
  dfrm <- data.frame(Title = title, Old_Price = price, Brand = brand)
  if (i == 1) {
    write.table(dfrm, file = "mapped.csv", sep = ",", row.names = FALSE)
  } else { write.table(dfrm, file = "mapped.csv", sep = ",", append = TRUE,
                     col.names = FALSE, row.names = FALSE)
    }
}

get_brand <- function(data, title) {
  for (i in 1: length(data$Product_Title)) {
    if (data$Product_Title[i] == title)
      return (data$Brand[i])
  }
}
get_price <- function(data, title){
  for (i in 1: length(data$Product_Title)) {
    if (data$Product_Title[i] == title)
      return (data$Price[i])
  }
}