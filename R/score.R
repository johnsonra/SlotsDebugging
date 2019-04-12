#' @title score
#' @description Score output from play()
#' 
#' @param symbols A dataframe of plays from play()
#' @param version Which version of score() to use
#' 
#' @rdname score
#' 
#' @return A score for an oucome from play()
#' @export score
score <- function(symbols, version = 0)
{
    eval(parse(text = paste0('score', version, '(symbols)')))
}

# socre using if and else
score0 <- function(symbols)
{
    symbols$prize <- 0
    
    for(i in 1:nrow(symbols))
    {
        # tabulate symbols
        tab <- table(unlist(symbols[i,]))
        
        # count diamonds and remove from tab
        diamonds <- sum(tab["DD"], na.rm = TRUE)
        tab['DD'] <- 0
        
        # count cherries
        cherries <- sum(tab["C"], na.rm = TRUE)
        
        # are non-diamonds the same?
        same <- length(tab[tab > 0] == 1)
        
        # are all non-diamonds bars?
        bars <- all(names(tab)[tab > 0] %in% c("B", "BB", "BBB"))
        
        #### assign prize ####
        
        # prize for all diamonds
        if(diamonds == 3){
            
            symbols$prize[i] <- payouts['DD']
            
        # prize for all the same
        }else if(same){
            
            symbols$prize[i] <- payouts[names(tab)[1]]
            
        # prize for all bars
        }else if(all(bars)){
            
            symbols$prize[i] <- 5
            
        # prize for any cherries
        }else if(cherries > 0){
            
            # diamonds count as cherries
            # so long as there is one real cherry
            symbols$prize <- c(2, 5)[cherries + diamonds]
            
        }
        
        # double for each diamond
        symbols$prize[i] <- symbols$prize[i] * 2^diamonds
    }
    
    return(symbols)
}

# another way to score using if and else
score1 <- function(symbols)
{
    for(i in 1:nrow(symbols))
    {
        # tabulate symbols
        syms <- unique(unlist(symbols))
        
        # count diamonds and remove from tab
        diamonds <- sum(symbols == "DD")
        syms <- syms[syms != 'DD']
        
        # count cherries
        cherries <- sum(symbols == "C")
        
        # are non-diamonds the same?
        same <- length(syms) == 1
        
        # are all non-diamonds bars?
        bars <- all(syms %in% c("B", "BB", "BBB"))
        
        #### assign prize ####
        
        # prize for all diamonds
        if(diamonds == 3){
            
            symbols$prize <- payouts['DD']
            
        # prize for all the same
        }else if(same){
            
            symbols$prize <- payouts[syms]
            
        # prize for all bars
        }else if(all(bars)){
            
            symbols$prize <- 5
            
        # prize for any cherries
        }else if(cherries > 0){
            
            # diamonds count as cherries
            # so long as there is one real cherry
            symbols$prize <- c(0, 2, 5)[cherries + diamonds + 1]
            
        }else{
            
            symbols$prize <- 0
            
        }
        
        # double for each diamond
        symbols$prize <- symbols$prize * 2^diamonds
    }
    
    return(symbols)
}

# vectorizing the code from score0 using apply and ifelse functions
score2 <- function(symbols)
{
    # tabulate sumbols from each row
    tab <- apply(symbols, 1, table)

    # count diamonnds
    diamonds <- lapply(tab, `[`, 'DD') %>%
                sapply(sum, na.rm = TRUE)

    # count cherries
    cherries <- lapply(tab, `[`, 'C') %>%
                sapply(sum, na.rm = TRUE)
    
    # are non-diamonds the same?
    same <- sapply(tab, function(.x) length(.x[names(.x) != 'DD']) == 1)

    # are all non-diamonds bars?
    bars <- sapply(tab, function(.x) all(names(.x)[names(.x) != 'DD'] %in% c("C", "BB", "BBB")))

    
    #### assign prize ####
    symbols$prize <- ifelse(diamonds == 3, payouts['DD'],                                             # prize for all diamonds
                     ifelse(same, payouts[sapply(tab, function(.x) names(.x)[names(.x) != 'DD'][1])], # prize for all the same
                     ifelse(bars, 5,                                                                  # prize for all bars
                     ifelse(cherries > 0, c(2, 5)[cherries + diamonds],                               # prize for any cherries
                            0))))                                                                     # otherwise nothing

    # double prize for each diamond
    symbols$prize <- symbols$prize * 2^diamonds
    
    return(symbols)
}

# rewrite of score2 using mutate and map
#' @import dplyr
#' @import purrr
score3 <- function(symbols)
{
    mutate(symbols,
           
           # tabulate sumbols from each row
           slots = apply(cbind(V1, V2, V3), 1, unique),
           
           # count diamonnds
           diamonds = (V1 == 'DD') + (V2 == 'DD') + (V3 == 'DD'),
           
           # count cherries
           cherries = (V1 == 'C') + (V2 == 'C') + (V3 == 'C')
           
           # are non-diamonds the same?
           nonDiamonds = map(slots, ~ grep('DD', .x, invert = TRUE, value = TRUE)),
           same = map_lgl(nonDiamonds, ~ length(.x) == 1),
           
           # are all non-diamonds bars?
           bars = map_lgl(slots, ~ all(.x %in% c("B", "BB", "BBB"))),
           
          
           #### assign prize ####
           prize = case_when(diamonds == 3 ~ payouts['DD'],                            # prize for all diamonds
                             same ~ payouts[nonDiamonds],                              # prize for all the same
                             bars ~ 5,                                                 # prize for all bars
                             cherries > 0 ~ c(0, 2, 5, 10)[cherries + diamonds + 1],   # prize for any cherries
                             TRUE ~ 0)) %>%                                            # otherwise nothing
    
    # double prize for each diamond
    mutate(prize = prize * 2^diamonds) %>%
        
    # keep only variables we want
    select('V1', 'V2', 'V3')
}
