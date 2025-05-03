fit_csf <- function(.split,Time,W,Event,horizon,...) {
    Time <- enquo(Time)
    W <- enquo(W)
    Event <- enquo(Event)
    col_vars <- quos(...)
    X <- .split |> select(!!!col_vars ) |> as.matrix()
    Y <- .split |> select(!!Time) |> pull()
    W <- .split |> select(!!W) |> pull()
    D <- .split |> select(!!Event) |> pull()
    causal_survival_forest(X, Y, W, D, target = "RMST", horizon = horizon)
}
