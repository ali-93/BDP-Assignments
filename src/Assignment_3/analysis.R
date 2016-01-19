
matched_items <- read.csv(file = "olx_mapped_titles.csv", 
                          header = TRUE, sep = ",")

# 1. Getting unique ads within matched items
matched_items <- unique(matched_items)

# 4. Getting Most Viewed and Top Ten Ads
most_viewed <- sort(matched_items$Views, decreasing = TRUE, 
                    na.last = TRUE)
most_viewed <- unique(most_viewed)
print(most_viewed[1:10])

# 5. Getting mobile that was avialable for sale most
hash <- function( keys ) {
  result <- new.env( hash = TRUE, parent = emptyenv(), size = length( keys ) )
  for( key in keys ) {
    result[[ key ]] <- 0
  }
  return( result )
}
read_new_mobile_data <- read.csv(file = "new_mobile_prices.csv",
                                 header = TRUE, sep = ",")
new_mob_hash <- hash(read_new_mobile_data$Title)
populate_occurences(read_new_mobile_data$Title, new_mob_hash)

populate_occurences <- function (some_data, hash) {
  for (key in some_data) {
    hash[[key]] <- hash[[key]] + 1
  }  
  return (hash)
}

get_top_most <- function (some_data, hash) {
  max = 0
  most_available = ""
  for (key in some_data) {
    if (max < hash[[key]]) {
      max = hash[[key]]
      most_available = key
    }
  }
  print(most_available)
}

# 5.  Getting Top Ten Mobiles

get_top_ten <- function (some_data, hash) {
  max = 0
  for (key in some_data) {
    if (max < hash[[key]]) {
      max = hash[[key]]
      most_available = key
      cat(key, max, "")
    }
  }  
}

# 6. Getting user who has post most ads and Top Users
unique_users <- unique(matched_items$Username)
unique_users_hash <- hash(unique_users)
populate_occurences(matched_items$Username, unique_users_hash)
print (get_top_most(matched_items$Username, unique_users_hash))
get_top_ten(matched_items$Username, unique_users_hash)

# 7. Getting the City that included most ads and Ranking them
unique_cities <- unique(matched_items$City)
unique_cities_hash <- hash(unique_cities)
populate_occurences(matched_items$City, unique_cities_hash)

print(typeof(matched_items$New_Price[1]))
# 8. Getting change in Prices [New-Original]

for (i in 1 : length(matched_items$Title)) {
  
    if ((matched_items$New_Price[i]) != 0) {
      change_in_price <- as.numeric(gsub(",","", matched_items$Old_Price[i])) - 
        as.numeric(gsub(",","", matched_items$New_Price[i]))
     }
    else {
      change_in_price <- NA
    }
  dfrm <- data.frame(New_Old = change_in_price)
  if (i == 1) {
  write.table(dfrm, file="change_in_price.csv", sep = ",",
              row.names = FALSE, col.names = TRUE)
  } else write.table(dfrm, file="change_in_price.csv", append = TRUE, sep = ",",
                     row.names = FALSE, col.names = FALSE)
}

# 9. Finding percent change in Prices of all Items  
#[formula: (Now - Original) / Original * 100]

for (i in 1 : length(matched_items$Title)) {
  
  if ((matched_items$New_Old[i]) != 0) {
    percent_change_in_price <- (as.numeric(gsub(",","", matched_items$New_Old[i])) / 
      (as.numeric(gsub(",","", matched_items$New_Price[i])) * 100))
  }
  else {
    percent_change_in_price <- 0
  }
  dfrm <- data.frame(Percent_Change_in_Price = percent_change_in_price)
  if (i == 1) {
    write.table(dfrm, file="change_in_price.csv", sep = ",",
                row.names = FALSE, col.names = TRUE)
  } else write.table(dfrm, file="change_in_price.csv", append = TRUE, sep = ",",
                     row.names = FALSE, col.names = FALSE)
}

# 10. Finding Average %age of all Unique Items
sum_of_all_percentile = 0
for (i in 1 : length(matched_items$Title)) {
    sum_of_all_percentile =  sum_of_all_percentile +
      as.numeric(gsub(",","", matched_items$Percent_Change_in_Price[i]))
}
print(sum_of_all_percentile / length(matched_items$Olx_Title))

# 11. Sort items with respect to percent change in prices [lower to higher]

sort_change_in_price <- sort(matched_items$Percent_Change_in_Price, 
                             decreasing = FALSE, na.last = TRUE)
dfrm <- data.frame(Sort_wrt_Percent_Change_in_Price = sort_change_in_price)
write.table(dfrm, file = "change.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)

# 12.  Finding some relationship between % change in price:
# 1. ad views

max_views <- sort(matched_items$Views, decreasing = TRUE, na.last = TRUE)

for (i in 1 : length(matched_items$Olx_Title)) {
    if (max_views[1] == matched_items$Percent_Change_in_Price[i]) {
        cat(max_views[1], matched_items$Percent_Change_in_Price[i], "\n")
    }
}
# 12. Finding some relationship between % change in price:
# 2. cities

get_mean <- function (field, data) {
  count = 0
  sum = 0
  for (i in 1 : length(matched_items$Olx_Title)) {
    if (!is.na(data[i]) && 
        as.character(data[i]) == field) {
      sum = sum + matched_items$Percent_Change_in_Price[i]
      count = count + 1
    }
  }
  print(count)
  return (sum / count)
}
print(get_mean("Islamabad", matched_items$City))
print(get_mean("Lahore, Punjab", matched_items$City))
print(get_mean("Karachi, Sindh", matched_items$City))

# 12. Finding some relationship between % change in price:
# 3. brands
print(get_mean("Nokia", matched_items$Brand))
print(get_mean("Samsung", matched_items$Brand))
print(get_mean("Voice", matched_items$Brand))
print(get_mean("Microsoft", matched_items$Brand))
print(get_mean("Huawei", matched_items$Brand))
print(get_mean("HTC", matched_items$Brand))
print(get_mean("Qmobile", matched_items$Brand))
print(get_mean("Rivo", matched_items$Brand))
print(get_mean("Mobilink", matched_items$Brand))
print(get_mean("GRight", matched_items$Brand))
print(get_mean("Club", matched_items$Brand))
print(get_mean("Oppo", matched_items$Brand))
print(get_mean("Calme", matched_items$Brand))
print(get_mean("Lenovo", matched_items$Brand))
print(get_mean("OPhone", matched_items$Brand))
print(get_mean("SonyEricsson", matched_items$Brand))
print(get_mean("LG", matched_items$Brand))
print(get_mean("Apple", matched_items$Brand))
print(get_mean("BlackBerry", matched_items$Brand))
print(get_mean("Haier", matched_items$Brand))
print(get_mean("iNew", matched_items$Brand))
print(get_mean("Motorola", matched_items$Brand))
print(get_mean("GFive", matched_items$Brand))

# 12. Finding some relationship between % change in price:
# 4. users

print(get_mean("Essa Hazoor", matched_items$Username))

# 13. Finding average percentage change prices of all unique brands 

print(get_mean("Nokia", matched_items$Brand))
print(get_mean("Samsung", matched_items$Brand))
print(get_mean("Voice", matched_items$Brand))
print(get_mean("Microsoft", matched_items$Brand))
print(get_mean("Huawei", matched_items$Brand))
print(get_mean("HTC", matched_items$Brand))
print(get_mean("Qmobile", matched_items$Brand))
print(get_mean("Rivo", matched_items$Brand))
print(get_mean("Mobilink", matched_items$Brand))
print(get_mean("GRight", matched_items$Brand))
print(get_mean("Club", matched_items$Brand))
print(get_mean("Oppo", matched_items$Brand))
print(get_mean("Calme", matched_items$Brand))
print(get_mean("Lenovo", matched_items$Brand))
print(get_mean("OPhone", matched_items$Brand))
print(get_mean("SonyEricsson", matched_items$Brand))
print(get_mean("LG", matched_items$Brand))
print(get_mean("Apple", matched_items$Brand))
print(get_mean("BlackBerry", matched_items$Brand))
print(get_mean("Haier", matched_items$Brand))
print(get_mean("iNew", matched_items$Brand))
print(get_mean("Motorola", matched_items$Brand))
print(get_mean("GFive", matched_items$Brand))

# 15. Sorting brands with respect to percent change 
# in prices [lower to higher]

brand <- matched_items$Brand
change_in_price <- matched_items$Percent_Change_in_Price

for (i in 1 : length(matched_items$Olx_Title)) {
  for (j in 2 : length(matched_items$Olx_Title)) {
    if (change_in_price[i] <= change_in_price[j]) {
      tmp = change_in_price[j]
      change_in_price[j] = change_in_price[i]
      change_in_price[i] = tmp
      
      tmp_b = as.character(brand[j])
      brand[j] = as.character(brand[i])
      brand[i] = tmp_b
    }
  }
}
print(change_in_price)
print(brand)
dfrm <- data.frame(Sorted_Brands = brand)
write.table(dfrm, file = "sorted_brands.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# 14. Finding some relationship between % change in price of brand: 
# 1. ad views

get_views_sum <- function(brand) {
  views = 0
  for (i in 1 : length(matched_items$Olx_Title)) {
    if (brand == as.character(matched_items$Brand[i])) {
      if (is.na(matched_items$Views[i])) {
        views = views + 0
      } else {
        views = views + matched_items$Views[i]
      }
    }
  }
  return (views)
}
print(get_views_sum("Samsung"))

# 14. Finding some relationship between % change in price of brand: 
# 2. cities

get_city_max_change_price_wrt_brand <- function(city, brand) {
  max = 0.0
  for (i in 1 : length(matched_items$Olx_Title)) {
    if (!is.na(matched_items$City[i]) && city == as.character(matched_items$City[i])) {
      if (brand == as.character(matched_items$Brand[i])) {
        if (max < matched_items$Percent_Change_in_Price[i]) {
          max = matched_items$Percent_Change_in_Price[i]
        }
      }
    }
  }
  return (max)
}

print(get_city_max_change_price_wrt_brand(city = "Lahore, Punjab", brand = "Samsung"))
print(get_city_max_change_price_wrt_brand(city = "Islamabad", brand = "Samsung"))
print(get_city_max_change_price_wrt_brand(city = "Karachi, Sindh", brand = "Samsung"))

# 14. Finding some relationship between % change in price of brand:
# 3. users
get_user_brand_relation <- function (field, data) {
  for (i in 1 : length(matched_items$Olx_Title)) {
    if (!is.na(data[i]) && 
        as.character(data[i]) == field) {
      if (matched_items$Percent_Change_in_Price[i] > 0) {
        print(as.character(matched_items$Brand[i]))
        print(matched_items$Percent_Change_in_Price[i])
      }
    }
  }
}
get_user_brand_relation("Essa Hazoor", matched_items$Username)

# 16. If you want to buy a mobile/tablet today and possibly 
# will sell it exactly 1 year later. Which mobile/tablet
# will you buy keeping in mind that you want to minimise the risk.
min = matched_items$Percent_Change_in_Price[1]
min_index = 0
for (i in 2 : length(matched_items$Olx_Title)) {
  if (matched_items$Percent_Change_in_Price[i] > 0.0)
    if (min >= matched_items$Percent_Change_in_Price[i]) {
        min = matched_items$Percent_Change_in_Price[i]
        min_index = i
    }
}
sort(matched_items$New_Old, decreasing = TRUE, na.last = TRUE)
print(matched_items$Percent_Change_in_Price[min_index])
print(as.character(matched_items$Title[min_index]))
print(as.character(matched_items$Brand[min_index]))