# Creating retention response variables for 1-year, 3-year, and 5-year retention using test-data
entity <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 7, 7, 7, 7, 7)
year <- c(2016, 2017, 2020, 2021, 2022, 2011, 2012, 2013, 2015, 2015, 2020, 2022, 2010, 2013, 2016, 2007, 2009, 2010, 2011, 2014, 2015, 2022)

test_data <- data.frame(entity, year)

testing