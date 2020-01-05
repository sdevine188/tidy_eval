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


##############################################################


# experiments with dots

pre_existing_var <- "pre_exist"

tabler_total_row <- function(table, stats = NULL, preceding_arg = "default", ..., last_arg) {
        
        dots <- list(...)
        # return(dots)
        return(stats)
}

# the function will look for all named arguments first
# if it finds a specified named argument, great
# if it finds an unspecified named argument, that gets passed to dots
# if it finds an unnamed argument, it will pass it to the next specified named arg it hasn't filled yet
# once it has filled all its specified named arguments, any remaining unnamed arguments are passed to dots
tabler_total_row(table = starwars, "stats_here", "precede",
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3")
tabler_total_row(table = starwars, "stats_here", 
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3", "precede")
tabler_total_row(table = starwars, "stats_here", 
                 test = "var1", "precede", test2 = c("var2", "var4"), last_arg = "test3")
tabler_total_row(table = starwars, "stats_here", 
                 test = "var1", "dot_arg", test2 = c("var2", "var4"), last_arg = "test3", "precede")
# note that when an argument is not passed a value, either named or unnamed, and it has no default, then
# the function will error out if that argument is used in function (eg "argument 'stats' is missing, with no default")
tabler_total_row(table = starwars, 
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3", preceding_arg = "precede")
tabler_total_row(table = starwars, "stats_here", 
                 test = "var1", "dot_arg", test2 = c("var2", "var4"), last_arg = "test3", preceding_arg = "precede")
# can pass pre_existing variables to named args
tabler_total_row(table = starwars, stats = "stats_here", preceding_arg = pre_existing_var,
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3")


##############################################################################


# passing bare variable names to function arguments 
tabler_total_row <- function(table, var1, var2, preceding_arg = "default", ..., last_arg) {
        
        # dots <- list(...)
        # dots <- deparse(substitute(...))
        dots <- enquos(...)
        return(dots)
        
        # var1 <- deparse(substitute(var1))
        # var2 <- deparse(substitute(var2))
        # var_syms <- syms(c(var1, var2))
        # return(starwars %>% select(!!!var_syms))
}

tabler_total_row(table = starwars, var1 = species, var2 = mass, preceding_arg = "precede",
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3")

# can also pass multiple bare variables via dots
tabler_total_row(table = starwars, var1 = species, preceding_arg = "precede",
                 name, homeworld, last_arg = "test3", var2 = mass)

# deparse/substitute also works without specifying the names argument, but then it must be in order
# but deparse/substitute is more rigid than dots, which can accept any number of bare variables
tabler_total_row(table = starwars, species, mass, preceding_arg = "precede",
                 test = "var1", test2 = c("var2", "var4"), last_arg = "test3")


##############################################################


# use vars() to pass one or more bare variable names to a function 
# using vars() is better than using dots (see below), since you can have other arguments
add_string_w_vars <- function(tbl, vars, string = "test") {
        
        tbl %>% select(!!!vars) %>% map_dfc(.x = ., .f = ~ str_c(.x, string, sep = "_"))
}

starwars %>% add_string_w_vars(vars = vars(homeworld, name))
starwars %>% add_string_w_vars(vars = vars(homeworld, name), string = "hello")


##################


# use enquos on the dots to pass one or more bare variable names to a function
# better to just use vars() instead though (see above)
add_string_w_dots <- function(tbl, ...) {
        
        # convert bare variable names passed to dots into quosures
        variable_quos <- enquos(...)
        
        tbl %>% select(!!!variable_quos) %>% map_dfc(.x = ., .f = ~ str_c(.x, "_test"))
}

# call add_string
starwars %>% add_string_w_dots(name, homeworld, name)


############################################


# if you were so inclined, you can pass a list of bare variables
# note that vars() function literally just passes inputs to quos()
var_list <- list(vars(name, homeworld), vars(species))
var_list

# use vars() to pass one or more bare variable names to a function 
# using vars() is better than using dots (see below), since you can have other arguments
add_string_w_var_list <- function(tbl, var_list_tbl, current_origin_list, string = "test") {
        
        # print(current_origin_list)
        # print(var_list_tbl %>% filter(origin_list == current_origin_list) %>% pull(value))
        # print(var_list_tbl)
        
        # get current_var_list_syms
        current_var_list_vars <- var_list_tbl %>% filter(origin_list == current_origin_list) %>% pull(value)
        current_var_list_vars_syms <- syms(current_var_list_vars)

        # add strings to selected variables
        tbl %>% select(!!!current_var_list_vars_syms) %>% map_dfc(.x = ., .f = ~ str_c(.x, string, sep = "_"))
}

# create call_add_string_on_var_list function
call_add_string_on_var_list <- function(tbl, var_list) {
        
        # get var_list_length tbl
       var_list_length_tbl <- map(.x = var_list, .f = length) %>% enframe() %>% unnest() %>% select(value) %>% 
                rename(var_list_length = value) %>% mutate(origin_list = row_number())
       
       # get var_list_tbl
       var_list_tbl <- pmap_dfr(.l = var_list_length_tbl, 
            .f = function(var_list_length, origin_list, ...) { 
                    tibble(origin_list = rep(origin_list, times = var_list_length))
                    })
       
       # add vars as strings to var_list_tbl
       var_list_tbl <- map(.x = var_list %>% unlist(), .f = as_name) %>% enframe() %>% unnest() %>% select(value) %>%
               bind_cols(var_list_tbl, .)
       
       # map through each list in var_list calling add_string
       map(.x = var_list_length_tbl %>% pull(origin_list), 
           .f = ~ add_string_w_var_list(tbl = tbl, var_list_tbl = var_list_tbl, 
                                        current_origin_list = .x))
}

# call call_add_string_on_var_list()
starwars %>% call_add_string_on_var_list(var_list = list(vars(homeworld), vars(species, name)))


################################################################################################3
################################################################################################3
################################################################################################3


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

# using quo on string
var_name <- "homeworld" 
var_quo <- quo(var_name)
starwars %>% select(!!var_quo) 

# using quos on string
var_names <- c("homeworld", "mass")
var_quos <- quos(var_names)
starwars %>% select(!!!var_quos)


#########################################


# use variable

# note a quosure isn't needed because we're "not referring to symbols from the contextual environment"
# https://github.com/tidyverse/rlang/issues/116
var_name <- "homeworld"
var_name_sym <- sym(var_name)
var_name_sym
select(starwars, !!var_name_sym)

# using quo on string
starwars %>% select(!!quo("homeworld"))


##########################################


# filter using variable
var_name <- "homeworld"
var_name_sym <- sym(var_name)
var_name_sym
starwars %>% filter((!!var_name_sym) == "Naboo")

# this doesn't work for some reason??
var_name <- "homeworld"
var_name_quo <- quo(var_name)
starwars %>% filter((!!var_name_quo) == "Naboo")


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
criteria5 <- map(.x = c("mass", "height"), 
                 .f = ~ str_glue("is.na({.x})") %>% as.character()) %>% 
        unlist() %>% str_c(., collapse = " | ") %>% parse_exprs()

starwars %>% filter(!!criteria1)
starwars %>% filter(!!criteria2)
starwars %>% filter(!!criteria3)
starwars %>% filter(!!!criteria4)
starwars %>% filter(!!!criteria5)


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

# note you also can't pass as_name a bare term - it needs a string; but sym works just as well for NSE names
# var_as_name <- as_name(homeworld)
# var_as_name 
# starwars %>% mutate(!!var_as_name := "it worked") %>% select(name, !!var_as_name)

# not that as_name is unable to parse quosures with tidyselect helpers, but as_label can
tidyselect_quo <- vars(starts_with("zzz"))
tidyselect_quo
tidyselect_quo[[1]]
as_name(tidyselect_quo[[1]])
as_label(tidyselect_quo[[1]])


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
