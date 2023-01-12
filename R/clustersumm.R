#' Cluster summary
#'
#' This function returns a summary of all the clusters  this summary can help in
#' the grouping of health data
#' @author Jorge Alcantara Espinosa
#' @param df dataframe we are studying
#' @param k, number of cluster we want to predict
#' @return summary of the information contained in each cluster
#' @export
#' @example clustersumm(df,3)



clustersumm <- function(df,k){
    if (!require(cluster)) {

        stop("cluster not installed")

    } else {
        if (!require(Rtsne)) {
            stop("Rtsne not installed")
        } else {
            if (!require(tidyr)) {

                stop("tidyr not installed")

            } else {
            if (!require(ggplot2)) {

                stop("ggplot2 not installed")

            } else {
                df$hday <- as.numeric(df$hday)
                gd <- daisy(df,
                            metric = "gower",
                            type = list(logratio = 3))
                gd_mat <- as.matrix(gd)
                sil_width <- c(NA)
                for(i in 2:10){

                    pam_fit <- pam(gd,
                                   diss = TRUE,
                                   k = i)

                    sil_width[i] <- pam_fit$silinfo$avg.width

                }
                pam_fit <- pam(gd, diss = TRUE, k)
                pam_results <- df %>%
                    mutate(cluster = pam_fit$clustering) %>%
                    group_by(cluster) %>%
                    do(the_summary = summary(.))
                pam_results$the_summary
            }}}}}
