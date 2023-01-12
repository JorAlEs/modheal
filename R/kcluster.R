#' Cluster recommendations
#'
#'this function returns a graphical plot containing information about the number of recommended clusters
#'
#' @author Jorge Alcantara Espinosa
#' @param df dataframe we are studying
#'
#' @return summary of the information contained in each cluster
#' @export
#' @example kcluster(df)


kcluster <- function(df){
    if (!require(cluster)) {

        stop("cluster not installed")

    } else {
        if (!require(Rtsne)) {
            stop("Rtsne not installed")
        } else {
            if (!require(ggplot2)) {

                stop("ggplot2 not installed")

            } else {
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
                plot(1:10, sil_width,
                     xlab = "Number of clusters",
                     ylab = "Silhouette Width")
                lines(1:10, sil_width)
            }}}}

