# From Leonard Cohen comes a puzzle at the intersection of 
# language and mathematics:
#
# In Jewish study, "Gematria" is an alphanumeric code where words are 
# assigned numerical values based on their letters. We can do the same 
# in English, assigning 1 to the letter A, 2 to the letter B, and so on, 
# up to 26 for the letter Z. The value of a word is then the sum of the 
# values of its letters. For example, RIDDLER has an alphanumeric value 
# of 70, since R + I + D + D + L + E + R becomes 18 + 9 + 4 + 4 + 12 + 5 + 18 = 70.
#
# But what about the values of different numbers themselves, spelled out as words? 
# The number 1 (ONE) has an alphanumeric value of 15 + 14 + 5 = 34, and 2 (TWO) 
# has an alphanumeric value of 20 + 23 + 15 = 58. Both of these values are bigger 
# than the numbers themselves.
#
# Meanwhile, if we look at larger numbers, 1,417 (ONE THOUSAND FOUR HUNDRED SEVENTEEN) 
# has an alphanumeric value of 379, while 3,140,275 (THREE MILLION ONE HUNDRED FORTY 
# THOUSAND TWO HUNDRED SEVENTY FIVE) has an alphanumeric value of 718. These values 
# are much smaller than the numbers themselves.
#
# If we consider all the whole numbers that are less than their alphanumeric value, #
# what is the largest of these numbers?
#
# Riddler examples to trial to ensure consistency
# 1 = 34 ONE
# 2 = 58 TWO
# 1417 = 379 ONE THOUSAND FOUR HUNDRED SEVENTEEN
# 3140275 = 718 THREE MILLION ONE HUNDRED FORTY THOUSAND TWO HUNDRED SEVENTY FIVE

#Packages that I may use
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(rmarkdown)

# 1.1.1 Translating Numbers to Text: The first step is building a function that produces words from numeric values
# This is not required, but in order to explore a large list of numbers, it is
# definitely useful. Below is a slightly altered version of a program I found 
# written by John Fox quite some time ago (I think 2005).

number_to_text <- function(x) {
  # Function originally by John Fox found here: 
  # http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  # Function I altered was a derivative by AJH found here:
  # https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    n_digits <- length(digits)
    
    if (n_digits == 1) 
      as.vector(ones[digits])
    
    else if (n_digits == 2 && x <= 19) 
      as.vector(teens[digits[1]])
    
    else if (n_digits == 2 && x > 19) 
      trim(
        paste(
          tens[digits[2]],Recall(as.numeric(digits[1]))
        ))
    
    else if (n_digits == 3) 
      trim(
        paste(
          ones[digits[3]], "hundred",Recall(makeNumber(digits[2:1]))
        ))
    
    else {
      n_suffix <- ((n_digits + 2) %/% 3) - 1
      if (n_suffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(
        paste(
          Recall(makeNumber(digits[n_digits:(3*n_suffix + 1)])),
          suffixes[n_suffix],
          Recall(makeNumber(digits[(3*n_suffix):1]))
        ))}
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- trunc(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper))) else helper(x)
}

# 1.1.2 The "AND" Problem: The Riddler challenge gives an example of THREE MILLION ONE HUNDRED FORTY THOUSAND 
# TWO HUNDRED SEVENTY FIVE which does not include "AND" as part of the written
# version. While this is one method of writing numbers, it is often not considered
# grammatically correct. To assess for the effect of including "AND" the below 
# program adjusts. It should be noted that there are probably easier ways of accomplishing
# this without a duplicate program.
number_to_text_and <- function(x) {
  # Function originally by John Fox found here: 
  # http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  # Function I altered was a derivative by AJH found here:
  # https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    n_digits <- length(digits)
    
    if (n_digits == 1) 
      as.vector(ones[digits])
    
    else if (n_digits == 2 && x <= 19) 
      as.vector(teens[digits[1]])
    
    else if (n_digits == 2 && x > 19) 
      trim(
        paste(
          tens[digits[2]],Recall(as.numeric(digits[1]))
        ))
    
    else if (n_digits == 3) 
      trim(
        paste(
          ones[digits[3]], "hundred and",Recall(makeNumber(digits[2:1]))
        ))
    
    else {
      n_suffix <- ((n_digits + 2) %/% 3) - 1
      if (n_suffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(
        paste(
          Recall(makeNumber(digits[n_digits:(3*n_suffix + 1)])),
          suffixes[n_suffix],
          Recall(makeNumber(digits[(3*n_suffix):1]))
        ))}
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- trunc(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper))) else helper(x)
}

# 1.2 Alphanumeric Value: The next step is to build a function that takes the written word
# and calculates an alphanumeric value for it according to the guide 
# on FiveThirtyEight. First I build a table of each letter and value.
# I then use that to create a table with one column for each letter 
# from a number written out as a word is associated in a row with its
# corresponding value. This allows an easy summation over the rows which
# this function produces at the end.
text_to_value <- function(wordy) {
  #Build the crosswalk dataframe
  crosswalk <- tibble(
    letter = letters,
    number = 1:26
  )
  
  #Translate the letters from the words into numbers
  #First remove excess characters, then put letters into column
  textnumber <- str_to_lower(wordy) %>%
    str_remove_all(" ") %>%
    str_remove_all("-") %>%
    str_remove_all(",") %>%
    str_split("",simplify = TRUE) %>%
    as_tibble() %>%
    pivot_longer(cols=contains("v"),names_to = "col",names_prefix = "V")
  
  #Match each letter to a number
  calculating <- left_join(textnumber,crosswalk,by = c("value" = "letter"))
  
  #create summary number
  sum(pull(calculating,number))
}

# 1.3 Database of Numbers and Alphanumeric Values: The second to last step
# was to develop a function to create a database to be used to find a solution.
riddler <- function(x) {
  wordy <- sapply(x,number_to_text)
  num_sum <- sapply(wordy,text_to_value)
  logical <- num_sum > x
  wordy_and <- sapply(x,number_to_text_and)
  num_sum_and <- sapply(wordy_and,text_to_value)
  logical_and <- num_sum_and > x
  tibble(
    number = x,
    value = num_sum,
    success = logical,
    value_and = num_sum_and,
    success_and = logical_and)
}

# Trialing known quantities from the Riddler challenge
riddler(1)
riddler(2)
riddler(1417)
riddler(3140275)

# 1.4 The Non-computer Method: First it's important to recognize that 
# the decimal system is modular in nature. All numbers are going to have a single
# digit value, a tens value, a hundreds value, etc. Within each of these, certain
# patterns of high value will recur. For instance, among the single digits, "SEVEN"
# produces the highest value at 65. Among tens, 70 produces the greatest value at 110.
# We know, then, that 77 will be the highest score <100 and it's the simple addition
# of the two values 65+110=175. The final thing to recognize is that there are diminishing
# returns. "ONE HUNDRED" provides a value of 108 on top of any two-digit value, but 
# "SEVEN HOUNDRED" only adds an additional 31 in alphanumeric value over "ONE HUNDRED"
# while adding 600 additional in numeric value. Similarly, "ONE THOUSAND" adds 136 
# in alphanumeric value while "SEVEN THOUSAND" adds an additional 31 in alphanumeric value
# which is far outstripped by the 6000 additional in numeric value. Recognizing this,
# we can search the space in a modular fashion. We see that the 100-900 range is where
# the numeric value first starts to be greater than the alphanumeric. Then, using our
# knowledge of the highest combo from the tens and singles "SEVENTY SEVEN" we can see that
# no numeric value 300 or greater can be overcome by the combination of it's alphanumeric 
# value and the alphanumeric value of "SEVENTY SEVEN" which is 175. In other words, "TWO 
# HUNDRED SEVENTY SEVEN" has a value of 277 which is less than it's alphanumeric value of 
# 132+175=307. We have to be careful at this step to avoid premature closure, however. 
# The question doesn't ask for the greatest alphanumeric value among numbers where the 
# alphanumeric value is greater than the numeric # value. It requests the greatest numeric 
# value where the alphanumeric is still greater than the numeric. We already about diminishing 
# returns. Reviewing the tens and singles values, we can see that we would need a numeric 
# value of at least 327 to break an alphanumeric value of 300. We can also see that adding 
# "EIGHTY" and "NINETY" only give alphanumeric values of 74 and 87, respectively. Not only 
# are these greater numeric value than alphanumeric on their own, the additional alphanumeric 
# value of 65 for "SEVEN" cannot be used to overcome their negative impacts. Thus, our search 
# space is down to 277, 278 and 279. It turns out that the numeric value 279 has an 
# alphanumeric value of 284 making it the largest numeric value still less than its alphanumeric
# value.


riddler(1:10) %>% arrange(desc(value))
riddler(c(10,20,30,40,50,60,70,80,90)) %>% arrange(desc(value))
riddler(1:100) %>% arrange(desc(value))
riddler(c(100,1000,700,7000)) %>% arrange(desc(value))
riddler(c(100,200,300,400,500,600,700,800,900)) %>% arrange(desc(value))
riddler(c(277,377,477))
riddler(c(307,317,327,337,347,357,367,377,387,397))
riddler(c(300:350)) %>% arrange(desc(value))
riddler(c(287,297)) %>% arrange(desc(number))
riddler(c(277,278,279)) %>% arrange(desc(number))

# While the by hand method (though computer assisted in this case). A more brute force,
# computer assisted method can provide some nice detail. First, we get the same answer of 279.
# Second, we can see that adding "AND" for grammatical purposes opens up 297, 294, 293, 292,
# 287, 284, and 282 which otherwise wouldn't have been options. Third, we can see that the single digit
# level is pretty important. Without it, 270 does not meet the standard of numeric > alphanumeric. 
# Additionally, the graphical view shows the benefits of the tens, but the decreasing yields into the
# hundreds and thousands. We can also see that the alphanumeric per numeric has decreasing value
# as the numeric increases. It appears similar to an exponential decay, though, not smooth. A final
# novelty noted was that there are no numeric values equal to alphanumeric values, but there 
# are two values within one: 219 and 253. 

nums <- 1:9999
solution <- riddler(nums)
filter(solution,success==TRUE) %>%
  arrange(desc(number)) %>%
  top_n(10,number)

filter(solution[1:279,],success==FALSE) %>%
  arrange(desc(number)) %>%
  top_n(10,number)

filter(solution,success_and==TRUE) %>%
  arrange(desc(number)) %>%
  top_n(10,number)

filter(solution,number==value) 
mutate(solution,same=abs(number-value)) %>%
  arrange(same)

ggplot(data=solution) +
  geom_point(mapping = aes(x=number,y=value,color=success)) +
  geom_abline(slope=1,intercept=0) +
  ylab("Alphanumeric Value") +
  xlab("Numeric Value") + 
  theme_classic()

ggplot(data=solution[1:500,]) +
  geom_point(mapping = aes(x=number,y=value,color=success)) +
  geom_abline(slope=1,intercept=0) +
  coord_cartesian(xlim = c(0,500), ylim = c(0,500)) +
  ylab("Alphanumeric Value") +
  xlab("Numeric Value") + 
  theme_classic()

solution2 <- mutate(solution[1:500,],per=value/number)
filter(solution2) %>%
  arrange(desc(per)) %>%
  top_n(10,per)
ggplot(data=solution2) +
  geom_point(mapping = aes(x=number,y=per)) +
  ylab("Alphanumeric Value Per Numeric") +
  xlab("Numeric Value") + 
  theme_classic()
