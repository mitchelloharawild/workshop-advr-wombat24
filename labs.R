# T F

# TRUE FALSE

`TRUE` <- FALSE

`TRUE`

T <- FALSE
F <- TRUE

T
F

# activeBindingFunction()


# active bindings
evil <- function() {
  runif(1) > 0.95
}
# evil

rm(F)
makeActiveBinding("F", evil, .GlobalEnv)

F


log(10)
log <- exp

log(10)

unlockBinding("log", asNamespace("base"))
assign("log", exp, envir = asNamespace("base"))

base::log(10)

devtools::load_all()


library(devtools)
devtools::load_all("../fabletools")

?forecast



0.1 + 0.2 == 0.3

(1/3 + 1/3 + 1/3) == 1

?Arith
?`+`


`+` <- `-`

3 + 1

FALSE + 2

rm(`+`)

TRUE + 2



is.atomic(Sys.Date())
is.atomic(as.POSIXlt(Sys.Date()))


unclass(Sys.Date())
unclass(as.POSIXlt(Sys.Date()))

matrix(1:10, nrow = 2)
c(matrix(1:10, nrow = 2))

matrix(letters, nrow = 2)
matrix(Sys.Date() + 1:10, nrow = 2)


cbind(
  1:26,
  letters
)

# These are reserved
NULL <- 3
NA <- 3
NA_real_ <- 3
NaN <- 3

letters[c(-1, 1)]


letters[c(-1, 0)]
letters[c(-1)]

n <- 0
letters[1:n]
letters[seq_len(n)]


samples <- character(10)
samples
for(i in seq_along(samples)) {
  samples <- letters[sample(seq_along(letters), 1)]
}
samples

samples <- NULL
samples
for(i in seq_len(10)) {
  samples <- c(samples, letters[sample(seq_along(letters), 1)])
}

samples

samples_list <- list()
samples_list
for(i in seq_len(10)) {
  samples_list[[i]] <- letters[sample(seq_along(letters), 1)]
}
samples_list


pryr::object_size(samples)
pryr::object_size(samples_list)

pryr::object_size(mtcars)


image(volcano)
pryr::object_size(volcano)

volcano
class(volcano)
class(volcano[1,3])

volcano[1,]

volcano[,1]
volcano[,1:2]
volcano[,1,drop=FALSE]


Orange[1L] |> class()
Orange[[1L]] |> class()

letters[1][1][1][1][[1]][[1]]

Orange["age"]
Orange[["age"]]


Orange["age",]
Orange[,"age", drop = FALSE]

Orange[NA,]

Orange$alpine <- runif(35) > 0.5

Orange$a

Orange_tbl <- as_tibble(Orange)

Orange_tbl$a

Orange_tbl

Orange[rep(1, 10000),]
as_tibble(Orange[rep(1, 10000),])

Orange_tbl[,1]
Orange_tbl[1,]


sample(letters, s = 2)



Orange["1"] <- rnorm(35)
Orange

Orange$`1`

Orange[["1"]]

c(Sys.Date(), 1)
c(1, Sys.Date())

c(FALSE, 1)

class(NA)
c(NA_real_, 1)

x <- c(1, NA, Sys.Date(), 1+1i)

class(x[2])
c(TRUE, as.POSIXlt(Sys.Date()))

TRUE + 2

vctrs::vec_c(Sys.Date(), 2)

library(readr)
penguins <- readr::read_csv("../whyhive_explorer/data/penguins.csv",
                            col_types = cols(
                              studyName = col_character(),
                              `Sample Number` = col_double(),
                              Species = col_character(),
                              Region = col_character(),
                              Island = col_character(),
                              Stage = col_character(),
                              `Individual ID` = col_character(),
                              `Clutch Completion` = col_character(),
                              `Date Egg` = col_date(format = ""),
                              `Culmen Length (mm)` = col_double(),
                              `Culmen Depth (mm)` = col_double(),
                              `Flipper Length (mm)` = col_double(),
                              `Body Mass (g)` = col_double(),
                              Sex = col_character(),
                              `Delta 15 N (o/oo)` = col_double(),
                              `Delta 13 C (o/oo)` = col_double(),
                              Comments = col_character()
                            ))
readr::spec(penguins)


fruit <- c("apple", "banana", "kiwi", "strawberry")
sales <- c(10, 3, 8, 100)
price <- c(2.99, 4.39)
sales*price

vec_recycle_common(price, sales)


square <- function(x) {
  return(x^2)
}
square(8)

`+`
1 + 2

if(TRUE) 1 else 0
`if`(TRUE, 1, 0)


# `function`(alist(x=), return(x^2))


square
square(7)

square[1]

as.list(square)[[2]]

formals(square)
body(square)
environment(square)

formals(mean)

?mean

mean

as.list(mean)[1]
alist(x = , y = )

mutate
ggplot
?geom_point



square <- function(x) {
  return(x^2)
}

square()
square(8)

library(rlang)
formals(square) <- alist(x = 2)
square()
body(square) <- expr(log(x))
square(8)
environment(square) <- emptyenv()
square(8)


MAE <- function(e, ...) mean(abs(e), ...)
RMSE <- function(e, ...) sqrt(mean(e^2, ...))

max_error <- function(e, ...) max(e, ...)
accuracy <- function(e, measure, ...) {
  measure(e, ...)
}
accuracy(rnorm(100), measure = max_error)


x <- palette(hcl.colors(8, "viridis"))


breakpoints <- function(x, n.breaks) {
  force(x)
  function(n.breaks) {
    seq(min(x), max(x), length.out = n.breaks)
  }
}

breakpoints(rnorm(10, 100))(3)


1:10 |>
  map(log)

# |> native pipe (>4.1.0)
# %>% magrittr pipe

1:10 |>
  purrr::map_vec(log, .ptype = Sys.time())

`%+%` <- function(lhs, rhs) lhs + rhs

1 %+% 2

# `%>%` <- function(lhs, rhs)


library(furrr)
library(purrr)
plan(multisession, workers = 4)
n <- c(1e3, 1e4, 1e5)
future_map(n, compose(mean, rnorm))



seq(1, 10, by = 0.5)
mycode <- parse(text = "seq(1, 10, by = 0.5)")
eval(mycode)

library(rlang)
mycode <- parse_expr("seq(1, 10, by = 0.5)")
mycode[1]
mycode[2]
eval_tidy(mycode)


5 + 3 * 7
mymath <- parse_expr("5 + 3 * 7")
as.list(mymath)

lobstr::ast(5 + 3 * 7)

`+`(5, `*`(3, 7))


lobstr::ast(
  mtcars |> select(cyl)
)
lobstr::ast(
  mtcars %>% select(cyl)
)

lobstr::ast(
  mtcars |> mutate(wt/hp)
)

`%>%`(mtcars, select(cyl))

lobstr::ast(
  -2^2
)
lobstr::ast(
  (-2)^2
)

lobstr::ast(
  !y %in% x
)

pkgs <- c("lobstr", "pryr", "rlang")
lapply(pkgs, library)

library(pkgs[1])


pkgs <- "lobstr"
library(pkgs[1])
library("lobstr")

library(pkgs[1], character.only = TRUE)
pkgs |> map(library, character.only = TRUE)

rlang
library(rlang)


log(3 + 5 * 7)
3 + 5 * 7


cyl

mtcars |>
  select(cyl)

library(ggplot2)
ggplot() +
  geom_line()

mpg
mtcars$mp

wacky_env <- function(x) {
  your_code <- enexpr(x)
  log <- exp
  eval(your_code)
}

wacky_env(log(1+4))
log(1+4)


less_wacky_env <- function(x) {
  your_code <- enquo(x)
  log <- exp
  eval_tidy(your_code)
}
less_wacky_env(log(1+4))



sym("2 * pi")
expr(2 * pi)
quo(2 * pi)


math <- expr(2 * pi)
math

logmath <- expr(log(math))
logmath

logmath <- expr(log(!!math))
logmath

my_seq_expr <- function(from, to, by) {
  expr(seq(!!from, !!to, by = !!by))
}
my_seq_expr(3, 9, 2)

my_mutate <- function(data, your_code) {
  my_code <- expr(log({{your_code}}))
  mutate(data, !!my_code)
}

mtcars |>
  mutate(mpg*2)
mtcars |>
  my_mutate(mpg*2)


my_mutate <- function(data, your_code) {
  mutate(data, !!deparse(enexpr(your_code)) := log({{your_code}}))
}

mtcars |>
  my_mutate(mpg*2)


my_mutate <- function(data, your_code) {
  your_code_captured <- list(enquo(your_code))
  names(your_code_captured) <- "asdfojdsaf"

  mutate(data, !!!your_code_captured)
}
mtcars |>
  my_mutate(mpg*2)

varname <- "log"
mtcars |>
  mutate(!!varname := log(mpg*2))



function() {
  .data <- env()
  .env <- caller.env()


  cyl <- 4
  mtcars |>
    filter(cyl == !!cyl)
}


apple <- 4
mtcars |>
  filter(.data$apple == cyl)

eval_tidy


my_mutate <- function(.data, ...) {
  mutation <- enquos(...)

  needs_name <- names(mutation) == ""
  names(mutation)[needs_name] <- mutation[needs_name] |> map(get_expr) |> map(as_label)

  result <- map(mutation, eval_tidy, data = .data, env = caller_env())
  .data[names(mutation)] <- result
  .data
}
mtcars |>
  my_mutate(mpg/wt, a= mpg + wt)


x <- as_tibble(mtcars)

x <- set_names(x, "a")

x$a


mtcars |>
  transform(a = mpg, a=wt)


x |>
  mutate(a + a)
