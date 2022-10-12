# Creating retention response variables for 1-year, 3-year, and 5-year retention using test-data
entity <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 7, 7, 7, 7, 7)
year <- c(2016, 2017, 2020, 2021, 2022, 2011, 2012, 2013, 2015, 2015, 2020, 2022, 2010, 2013, 2016, 2007, 2009, 2010, 2011, 2014, 2015, 2022)

test_data <- data.frame(entity, year)

retained <- c()
i <- 1
lastYear <- NA
highestvalue <- 0
for(i in 1:length(test_data$year)) {
  entityID <- test_data$entity[i]
  years <- c()
  # we need a function (probably a loop) that takes in the entity ID, looks for every iteration of that ID, and make a vector of the year results. Then we can do the looping
  i <- i + 1
}
  