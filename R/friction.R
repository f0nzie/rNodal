#' Chen Friction factor correl1ation
#'
#' Guo pg 4/48
#' @param ed relative roughness
#' @param Re Reynolds number
ff.chen <- function(ed, Re) {
  1 / (-4 * log10(ed / 3.7065 - 5.042 / Re * log10(ed^1.1098/2.8527 +
                                                     (7.149/Re)^0.8981)))^2
}



#' Moody friction factor calculation
#' @param Re Reynolds number
#' @param ed Relative roughness
#' @param tol tolerance
#' @param trials number of calculations to try
moody.ff <- function(Re, ed, tol = 0.00001, trials = 100) {
  # calculation of Friction Factor
  if (Re > 2000) {
    # turbulent flow
    f0 <- blasius.2(Re)  # first raw calculation of `f`
  } else {
    f = 64. / Re
    return(f)
  }

  # Colebrook iteration
  f1 <- f0
  iter <- 1
  while (TRUE) {
    sqrt.f.inv = 1.14 - 2 * log10(ed + 9.34 / (Re * sqrt(f1)))
    f = (1 / sqrt.f.inv)^2

    delta = abs(f1 - f)
    if (delta <= tol) break

    f1 = (f1 + f ) /2
    iter <- iter + 1
    if (iter > trials) break

    f = f1
  }
  return(f)
}


#' Friction factor calculatiobn using Colebrook
#' @param REY Reynolds number
#' @param ED Relative roughness
#' @param f1 a Blasius function
friction.factor <- function(REY, ED, f1 = blasius.0) {
  result <- list(fgi = NA, FF = NA, iter = NA)

  if (REY > 2000) {
    # turbulent flow
    # FGI = 0.056 + 0.5 / REY^0.32
    # FGI <- do.call(f1, list(REY))
    FGI <- f1(REY)
    fgi = FGI
  } else {
    # laminar flow
    FF = 64. / REY
    result$FF <- FF
    return(result)
  }

  # Colebrook iteration

  I = 1
  while (TRUE) {
    DEN = 1.14 - 2 * log10(ED + 9.34 / (REY * sqrt(FGI)))
    FF = (1 / DEN)^2
    DIFF = abs(FGI - FF)

    if (DIFF <= 0.0001) break

    FGI = (FGI + FF ) /2
    I = I + 1
    if (I > 10) break

    FF = FGI
  }
  # cat(DIFF, I)
  #cat(fgi, FGI)
  #ret <- FF
  result <- list(fgi = fgi, FF = FF, iter = I)
  return(result)

}


blasius.0 <- function(re) {
  f = 0.056 + 0.5 / re^0.32
}

blasius.1 <- function(re) {
  f <- 0.04 / re^0.25    # from https://arxiv.org/pdf/1007.2466.pdf
}

blasius.2 <- function(re) {
  f <- 0.316 / re^0.25    # from http://www.kolumbus.fi/jukka.kiijarvi/clunowa/fluid_mechanics/pdf_articles/darcy_friction_factor.pdf
}

# list of blasius functions
blasius <- list(bla0 = blasius.0, bla1 = blasius.1, bla2 = blasius.2)



haaland <- function(re, ed) {
  f.inv <- -1.8 * log((ed / 3.7)^1.11 + 6.9/re)
  return(1/f.inv)
}
