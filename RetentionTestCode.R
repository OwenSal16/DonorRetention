# Creating retention response variables for 1-year, 3-year, and 5-year retention using test-data
entity <- c(1, 1, 1, 1, 2, 2, 2, 2, 1, 3, 3, 4, 5, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 10, 10, 9, 9, 9, 10, 11, 10, 8, 9, 7, 12, 12)
yeardata <- c(2016, 2020, 2021, 2022, 2011, 2012, 2013, 2015, 2017, 2015, 2020, 2022, 2010, 2013, 2016, 2007, 2009, 2010, 2011, 2014, 2015, 2015, 2022, 2009, 2017, 2022, 2018, 2012, 2013, 2013, 2014, 2015, 2016, 2017, 2015, 2013, 2014, 2019, 2018, 2017, 2019, 2020)

test_data <- data.frame(entity, yeardata)

findYears <- function(e) {
  entityYears <- test_data %>%
    filter(entity == e) %>%
    select(yeardata) %>%
    arrange(yeardata)
  entityYearsVector = as.vector(entityYears[,1])
  entityYearsVector
}

uniqueEntity <- unique(test_data$entity)

RetentionOneYear <- function() {
  retainedOneYear <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(retainedOneYear) <- c('entityID', 'retainedYearly')
  e = 1
  for (e in uniqueEntity) {
    entityYearsVector <- unique(findYears(e))
    yearStart = entityYearsVector[1]
    retainedVal = 0
    possibleRetention = c(0)
    i = 2
    if (length(entityYearsVector) > 1) {
      for (y in entityYearsVector[2:length(entityYearsVector)]) {
        if ((yearStart+1) == y) {
          retainedVal = retainedVal + 1
        }
        else {
          possibleRetention[i] = retainedVal
          retainedVal = 0
          i = i+1
        }
        yearStart = y
      }
      possibleRetention[i] = retainedVal
    }
    retainedVal = max(possibleRetention)
    retainedOneYear[nrow(retainedOneYear) + 1,] = c(e,retainedVal)
  }
  retainedOneYear
}

RetainedOneYear <- RetentionOneYear()

RetainedThreeYears <- function() {
  retainedOneYear <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(retainedOneYear) <- c('entityID', 'retainedYearly')
  e = 1
  for (e in uniqueEntity) {
    entityYearsVector <- unique(findYears(e))
    yearStart = entityYearsVector[1]
    retainedVal = 0
    possibleRetention = c(0)
    i = 2
    if (length(entityYearsVector) > 1) {
      for (y in entityYearsVector[2:length(entityYearsVector)]) {
        if (y - (yearStart+1) <= 2) {
          retainedVal = retainedVal + 1
        }
        else {
          possibleRetention[i] = retainedVal
          retainedVal = 0
          i = i+1
        }
        yearStart = y
      }
      possibleRetention[i] = retainedVal
    }
    retainedVal = max(possibleRetention)
    retainedOneYear[nrow(retainedOneYear) + 1,] = c(e,retainedVal)
  }
  retainedOneYear
}

RetainedThreeYears

RetainedByYear <- function() {
  fulldata <- data.frame(matrix(ncol = 5, nrow = 0))
  for (e in uniqueEntity) {
    entityYearsVector <- unique(findYears(e))
    for (i in 1:length(entityYearsVector)) {
      if (i == 1) {
        OneYear = F
        ThreeYear = F
        FiveYear = F
      }
      else {
        if(entityYearsVector[i] - entityYearsVector[i-1] == 1) {
          OneYear = T
        } else {
          OneYear = F
        }
        
        if (entityYearsVector[i] - entityYearsVector[i-1] <= 3){
          ThreeYear = T
        } else {
          ThreeYear = F
        }
        
        if (entityYearsVector[i] - entityYearsVector[i-1] <= 5){
          FiveYear = T
        } else {
          FiveYear = F
        }
      }
      
      entYear <- data.frame(e, entityYearsVector[i], OneYear,ThreeYear, FiveYear)
      fulldata <- rbind(fulldata, entYear)
    }
  }
  fulldata
}

byYear <- RetainedByYear()
