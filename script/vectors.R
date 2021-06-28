#load package
library(tidyverse)


#==========data types

typeof(1) #double is an approximation with 3 special values -Inf, NaN, Inf

typeof(1L) #integer is exact with one special values NA

typeof(FALSE) #logical can be TRUE or FALSE

typeof("Deus") #character


#==========missing values or vectors

Vec <- tibble(x = 1L:10L, y = letters[1:10], z = rep(2:3, 5))

Vec$z  <- NULL #represents missing vector

Vec <- Vec %>% 
           mutate(w = if_else(x == 1L, "Stata user group", NA_character_))  #NA_character represents missing vector value(s)

#Also NA_integer, NA_real (double), NA (logical) 


#==========conversion or coersion

#explicit coersion
typeof(Vec$x)

Vec$v <- as.double(Vec$x) 

typeof(Vec$v)

#x integer & v double results in double
typeof(Vec$x)
typeof(Vec$v)

Vec <- Vec %>% mutate(u = x*v) 

typeof(Vec$u)


#==========test functions

is_logical(Vec$x)
is_integer(Vec$x)
is_character(Vec$u)
is_double(Vec$u)
is_atomic(Vec$x)
is_list(Vec$x)

#==========vector recycling

New_vec <- tibble(x = 1L:20L, y = letters[1:20]) #tidyeverse will throw erros
New_vec <- data.frame(x = 1L:10L, y = letters[1:20]) #base R will make implicit assumptiom and run


#==========subsetting atomic vector

Vec$w[c(1)] #subset using "[" and reference using positions

#==========recursive vectors (LISTS)

#subsetting list to access values using "[[" 

#initialise list
list_1 <- list(c(2, 3, 4), list(5, 6), list(c(7, 8), list(9, 10)))

#access elements of first list
list_1[[1]]
list_1[[2]]
list_1[[3]]

#access second element of second list
list_1[[2]][[1]]

#access the last element of the third list
list_1[[3]][[1]][2]

#subsetting list to access values using

#initialise list
list_2 <- list(a = c(2, 3, 4), b = list(5, 6), c = list(c(7, 8), list(9, 10)))

#access first member of the list and its elements
list_2$a
list_2$a[1]
list_2$a[3]

#access second member of the list and its elements
list_2$b
list_2$b[[1]]
list_2$b[[2]]

#access second member of the list and its elements
list_2$c
list_2$c[[1]]
list_2$c[[2]][[1]]
list_2$c[[2]][[2]]


#==========augumented vectors

#factors
#dates
#tibble

