context("test-score")

# try out some examples from score0
test_that("score0", {
    # we'll use this to verify that score0 works
    wheel <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
    
    # lifted and modified this from lookup.R - includes all possible outcomes
    tmp <- rep(names(wheel), 3) %>%                               # (need 3 copies so that we can get 3 of a kind)
        combn(3) %>%                                              # all possible combinations 
        t() %>%                                                   # transpose to long format
        as.data.frame(stringsAsFactors = FALSE) %>%               # convert to data.frame (no factors)
        unique()                                                  # keep only unique rows
        
    tmp <- mutate(tmp,
                  prize = score(tmp, 0)$prize,                    # prize for outcome
                  p = wheel[V1] * wheel[V2] * wheel[V3],          # probability of outcome
                  e = prize*p)                                    # expected value
    
    # sum of all probabilities should be equal to 1
    expect_equal(sum(tmp$p), 1)
    
    # sum of all expected winnings should be equal to about 0.9344
    expect_equal(round(sum(tmp$e), 4), 0.9344)
    
    #### Add any other scores here that you notice aren't correct
    
    # all bars
    expect_equal(score(data.frame(V1 = 'B',
                                  V2 = 'BB',
                                  V3 = 'BBB'))$prize,
                 5)
    
    # one cherry
    expect_equal(score(data.frame(V1 = '0',
                                  V2 = 'C',
                                  V3 = '7'))$prize,
                 2)
    
    # two cherries and a diamond
    expect_equal(score(data.frame(V1 = 'DD',
                                  V2 = 'C',
                                  V3 = 'C'))$prize,
                 20)
    
})

# test out other score functions to be sure we have them correct
# these tests assume that score0 is correct
test_that("More Score Functions", {
    # lifted this from lookup.R - includes all possible outcomes
    tmp <- rep(c("DD", "7", "BBB", "BB", "B", "C", "0"), 3) %>%   # (need 3 copies so that we can get 3 of a kind)
        combn(3) %>%                                              # all possible combinations 
        t() %>%                                                   # transpose to long format
        as.data.frame(stringsAsFactors = FALSE) %>%               # convert to data.frame (no factors)
        unique()                                                  # keep only unique rows

    expect_true(all(score(tmp, 1)$prize == score(tmp, 0)$prize))
    expect_true(all(score(tmp, 2)$prize == score(tmp, 0)$prize))
    expect_true(all(score(tmp, 3)$prize == score(tmp, 0)$prize))
})
