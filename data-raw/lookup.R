library(usethis)
library(SlotMachine)

wheel <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)

### generate exhaustive lookup table ###
tmp <- rep(names(wheel), 3) %>%                 # (need 3 copies so that we can get 3 of a kind)
       combn(3) %>%                             # all possible combinations 
       t() %>%
       as.data.frame(stringsAsFactors = FALSE) %>%
       as_tibble() %>%
       unique()                                 # end up with lots of dups - keep only unique combinations
                       
       # this will be our key for the lookup
tmp <- mutate(tmp,
              key = paste(V1, V2, V3),          # add variables and make calculations
              prize = score(tmp, 0)$prize)      # calculate the prize

payoutLookup <- tmp$prize
names(payoutLookup) <- tmp$key


# save in R/sysdata.rda
use_data(wheel, payouts, payoutLookup, internal = TRUE, overwrite = TRUE)
