library(tidyverse)
library(rlang)

# https://edwinth.github.io/blog/dplyr-recipes/
# http://www.onceupondata.com/2017/08/12/my-first-steps-into-the-world-of-tidyeval/
# http://www.brodrigues.co/blog/2016-07-18-data-frame-columns-as-arguments-to-dplyr-functions/
# http://www.brodrigues.co/blog/2017-06-19-dplyr-0-70-tutorial/
# http://dplyr.tidyverse.org/articles/programming.html
# https://github.com/tidyverse/rlang/issues/116
# http://www.json.blog/2017/05/making-sense-of-dplyr-0.6/?utm_content=bufferb2c45&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
# https://maraaverick.rbind.io/2017/08/tidyeval-resource-roundup/

head(starwars)


# use bare variable to create quosure
var_quo1 <- quo(homeworld)  
var_quo1 
select(starwars, !!var_quo1) 

###########################################


# use string to create quosure

# using sym
var_sym <- sym("homeworld")
var_sym
select(starwars, !!var_sym)

# using quo
var_quo2 <- quo("homeworld") 
var_quo2
select(starwars, !!var_quo2) 


#########################################


# use variable

# note a quosure isn't needed because we're "not referring to symbols from the contextual environment"
# https://github.com/tidyverse/rlang/issues/116
var_name <- "homeworld"
var_name_sym <- sym(var_name)
var_name_sym
select(starwars, !!var_name_sym)

# this also works, but seems unnecessarily complex
# var_name <- "homeworld"
# var_name_sym <- sym(var_name)
# var_name_sym 
# var_name_quo <- quo(!!var_name_sym)
# var_name_quo 
# select(starwars, !!var_name_quo)


##########################################


# filter using variable
var_name <- "homeworld"
var_name_sym <- sym(var_name)
var_name_sym
starwars %>% filter((!!var_name_sym) == "Naboo")

# this doesn't work for some reason??
# var_name_quo <- quo("homeworld")
# var_name_quo
# starwars %>% filter((!!var_name_quo) == "Naboo")


##########################################


# filter using string
var_name_sym <- sym("homeworld")
var_name_sym
starwars %>% filter((!!var_name_sym) == "Naboo")


##########################################


# when creating quosure inside a function from a bare variable or string, use sym (or enquo)
# bare_to_quo_in_func <- function(x, var) {
#         var_enq <- enquo(var)
#         print(var_enq)
#         x %>% select(!!var_enq) %>% head(1)
# }

bare_to_quo_in_func <- function(x, var) {
        var_sym <- sym(var)
        print(var_sym)
        x %>% select(!!var_sym) %>% head(1)
}

# call function with var as string or bare or symbol
# bare_to_quo_in_func(starwars, homeworld)
bare_to_quo_in_func(starwars, "homeworld")


#############################################


# when creating quosure inside a function from a variable, just use sym
bare_to_quo_in_func <- function(x, var) {
        var_sym <- sym(var)
        print(var_sym)
        x %>% select(!!var_sym) %>% head(1)
}

var_name <- "homeworld"
bare_to_quo_in_func(starwars, var_name)


#############################################


# use string expression to filter
criteria1 <- parse_expr("height < 170")
criteria2 <- parse_expr("height < 170 & mass < 70")
criteria3 <- parse_expr("height < 170 & mass < 70 & homeworld == 'Naboo'")
criteria4 <- parse_exprs(str_c("height < 170", "mass < 70", "homeworld == 'Naboo'", sep = ";"))

starwars %>% filter(!!criteria1)
starwars %>% filter(!!criteria2)
starwars %>% filter(!!criteria3)
starwars %>% filter(!!!criteria4)


###############################################


# use variable to name new variable in mutate/summarize
var_name <- "homeworld"
var_name_sym <- sym(var_name)
var_name_sym
starwars %>% mutate(!!var_name_sym := "it worked") %>% select(name, !!var_name_sym)

# use string to name new variable in mutate/summarize
var_name_sym <- sym("homeworld")
var_name_sym 
starwars %>% mutate(!!var_name_sym := "it worked") %>% select(name, !!var_name_sym)

# note you can't pass sym a bare term - it needs a string
# var_name_sym <- sym(homeworld)
# var_name_sym 
# starwars %>% mutate(!!var_name_sym := "it worked") %>% select(name, !!var_name_sym)

# note you also can't pass quo_name a bare term - it needs a string; but sym works just as well for NSE names
# var_quo_name <- quo_name(homeworld)
# var_quo_name 
# starwars %>% mutate(!!var_quo_name := "it worked") %>% select(name, !!var_quo_name)


####################################################


# note that sym is also used inside functions - there is no "enquo_sym" or anything different required

# use variable in function
nse_name_in_function_using_var <- function(data, var) {
        var_name_sym <- sym(var)
        starwars %>% mutate(!!var_name_sym := "it worked") %>% select(name, !!var_name_sym)
}

var_name <- "homeworld"
nse_name_in_function_using_var(starwars, var_name)

# use string in function
nse_name_in_function_using_string <- function(data, var) {
        var_name_sym <- sym(var)
        starwars %>% mutate(!!var_name_sym := "it worked") %>% select(name, !!var_name_sym)
}

nse_name_in_function_using_string(starwars, "homeworld")


#############################################


# use syms to handle multiple syms
var1 <- "species"
var2 <- "homeworld"
var3 <- "skin_color"
var_syms <- syms(c(var1, var2, var3))
var_syms

starwars %>% select(!!!var_syms)
