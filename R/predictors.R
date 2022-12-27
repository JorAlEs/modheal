#' Predictors list
#'
#' This function returns a list of predictors in order of importance
#' these predictors help in the need of reevaluate the observation
#' @author Jorge Alcantara Espinosa
#' @param df dataframe we are studying
#' @param n, number of predictors we want to return in orden of importance
#' @return list of coeficients of a lineal model that predicts need
#' of reevaluation
#' @export
#' @example predictor(df,10)


predictors <- function(df,n){
    fit <- lm(nday~., data= df)
    coeficientes <- summary(fit)$coefficients[,4]
    coeficientes <- sort(coeficientes)
    coeficientes[1:n]
}
