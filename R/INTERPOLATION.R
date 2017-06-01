# library(dplyr)

#' Interpolation of curve to
#' @param drhov    duns ros
#' @param GLR      gas liquid ratio
#' @param degree   degree of polynomial
#' @importFrom dplyr filter
#' @importFrom stats lm predict
#' @export
interp.fanbr <- function(drhov, GLR, degree) {
    # curves.0241 <- NULL
    # load("./data/curves.0241.rda")
    if (GLR < 1500) curve = "LT1500"
    if (GLR > 3000) curve = "GT3000"
    if (GLR <= 3000 & GLR >= 1500) curve = "15003000"

    data <- curves.0241 %>%
        filter(GLR == curve)

    x <- log(data$drhov)
    y <- log(data$FF)

    model <- model.select(x, y, degree)
    val <- data.frame(x = log(drhov))

    # df = data.frame(x = log(seq(1, 81, 1)))     # dataframe with drhov sequence
    # y.df <- as.numeric(predict(model, df))
    # ff.df <- exp(y.df)

    y.val <- as.numeric(predict(model, val))
    ff <- exp(y.val)

    # list(drhov = drhov,
    #      GLR = GLR,
    #      curve = curve,
    #      degree = degree,
    #      # y.df = y.df,
    #      # ff.df = ff.df,
    #      model = model,
    #      ff.val = ff.val
    # )
    named.list( drhov, GLR, degree, curve, model, y.val, ff )
}

model.select <- function(x, y, degree) {
    models <- list(
        lm(y ~ x),
        lm(y ~ x + I(x^2)),
        lm(y ~ x + I(x^2) + I(x^3)),
        lm(y ~ x + I(x^2) + I(x^3) + I(x^4)),
        lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) )
    )
    sel.mod <- models[[degree]]
    return(sel.mod)
}
