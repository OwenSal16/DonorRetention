#######################
## Splitting up Donor Retention history to bring down file sizes
## Owen Salciccioli
## October 5th

library(tidyverse)
DonorHistory <- read_csv("/Users/owensalciccioli/Downloads/2_1_Donor_Transaction_History.csv")

DonorHistoryFirstThree <- DonorHistory %>%
  select(1:3)

write_csv(DonorHistoryFirstThree, "DonorHistoryFirstThree")

DonorHistorySecondThree <- DonorHistory %>%
  select(4:6)

write_csv(DonorHistorySecondThree, "DonorHistorySecondThree")

DonorHistoryThirdThree <- DonorHistory %>%
  select(7:9)

write_csv(DonorHistoryThirdThree, "DonorHistoryThirdThree")

DonorHistoryFourthThree <- DonorHistory %>%
  select(10:12)

write_csv(DonorHistoryFourthThree, "DonorHistoryFourthThree")

DonorHistoryFifthThree <- DonorHistory %>%
  select(13:15)

write_csv(DonorHistoryFifthThree, "DonorHistoryFifthThree")

DonorHistorySixthThree <- DonorHistory %>%
  select(16:18)

write_csv(DonorHistorySixthThree, "DonorHistorySixthThree")

DonorHistoryLastTwo <- DonorHistory %>%
  select(19:20)

write_csv(DonorHistoryLastTwo, "DonorHistoryLastTwo")
