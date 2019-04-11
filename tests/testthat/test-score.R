context("test-score")

# try out some examples from score0
test_that("score0", {
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
    
    # 
    
    #### Add any other scores here that you notice aren't correct
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
