library(dplyr)
N <- 1000
n <- 20
rho <- 0.5
simulated_data <- tibble(
    sample = rep(1:N, rep(n, N)),
    x = rnorm(N * n, 0, sd = 1),
    y = x * rho + rnorm(N * n, 0, sd = sqrt(1 - rho^2))
)

## ---------------------------------------------------
## calculamos los estadísticos para cada muestra
## ---------------------------------------------------

statistics <- simulated_data %>%
    group_by(sample) %>%
    summarise(
        znx = mean(x) / (1 / sqrt(n())),
        zny = mean(y) / (1 / sqrt(n())),
        px = 2 * pnorm( abs(znx), lower.tail = FALSE),
        py = 2 * pnorm(abs(zny), lower.tail = FALSE),
        Rx = px < 0.05,
        Ry = py < 0.05,
        Rxy = Rx | Ry
    )
## ---------------------------------------------------------
## calculamos los ratios de falsos positivos
## ---------------------------------------------------------

statistics %>%
    ungroup() %>%
    summarise(
        fprx = sum(Rx) / N * 100,
        fpry = sum(Ry) / N * 100,
        fprxy = sum(Rxy) / N * 100
    )

## ------------------------------------------------------------
##
## Para parada selectiva de recogida de datos
##
## --------------------------------------------------------------

N <- 1000
n <- 20
simulated_data <- tibble(
    sample = rep(1:N, rep(n, N)),
    x = rnorm(N * n, 0, sd = 1),
)

statistics <- simulated_data %>%
    group_by(sample) %>%
    summarise(
        znx = mean(x) / (1 / sqrt(n())),
        px = 2 * pnorm( abs(znx), lower.tail = FALSE),
        Rx = px < 0.05,
        znx_hacked = if_else(
            Rx,
            znx,
            mean(c(x, rnorm(10, 0, sd = 1))) * sqrt(n() + 10)
           ),
        px_hacked = 2 * pnorm( abs(znx_hacked), lower.tail = FALSE),
        Rx_hacked = px_hacked < 0.05
    )
statistics %>%
    ungroup() %>%
    summarise(
        fprx = sum(Rx) / N * 100,
        fprx_hacked = sum(Rx_hacked) / N * 100,
    )
library("dplyr")
library("purrr")
N <- 10000
n <- 20
rho <- 0.5
simulated_data <- tibble(
    sample = 1:N,
    x = map(sample, ~ rnorm( n, 0, sd = 1)),
    y = map(x, ~ . * rho + rnorm(n, 0, sd = sqrt(1 - rho^2)))
)
## ---------------------------------------------------
## calculamos los estadísticos para cada muestra
## ---------------------------------------------------

statistics <- simulated_data %>%
    mutate(
        znx = map_dbl(x, ~ mean(.) * sqrt(length(.))),
        zny = map_dbl(y, ~ mean(.) * sqrt(length(.))),
        px = 2 * pnorm(abs(znx), lower.tail = FALSE),
        py = 2 * pnorm(abs(zny), lower.tail = FALSE),
        Rx = px < 0.05,
        Ry = py < 0.05,
        Rxy = Rx | Ry
    )

## -----------------------------------------------------
## calculamos los ratios de falsos positivos
## -----------------------------------------------------

statistics %>%
    summarise(
        fprx = sum(Rx) / N * 100,
        fpry = sum(Ry) / N * 100,
        fprxy = sum(Rxy) / N * 100
    )




N <- 10000
n <- 20
simulated_data <- tibble(
    sample = 1:N,
    x = map(sample, ~ rnorm( n, 0, sd = 1))
)
statistics <- simulated_data %>%
        mutate(
        znx = map_dbl(x, ~ mean(.) * sqrt(length(.))),
        px = 2 * pnorm( abs(znx), lower.tail = FALSE),
        Rx = px < 0.05,
        znx_hacked = if_else(
            Rx,
            znx,
            map_dbl(x, ~ mean(c(., rnorm(10, 0, sd = 1))) * sqrt(length(.) + 10))
        ),
        px_hacked = 2 * pnorm( abs(znx_hacked), lower.tail = FALSE),
        Rx_hacked = px_hacked < 0.05
        )
statistics %>%
    summarise(
        fprx = sum(Rx) / N * 100,
        fprx_hacked = sum(Rx_hacked) / N * 100,
    )
