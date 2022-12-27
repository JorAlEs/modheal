#' Visualization of posible relations
#'
#' This function allows the user to view multiples plots
#' It needs having the ggplot2 library installed.
#'
#' @author Jorge Miguel Alcantara Espinosa
#' @param df is the dataframe we are analysing
#' @param y is the plot type we are looking at.
#' @return return a graphical visualization of the data
#' @examples
#' visualization(df,"plot1")


visualization <- function(df,y) {

    if (!require(ggplot2)) {

        stop("ggplot not installed")

    } else {
        if(y == "plot1"){plot <- ggplot(df, aes(x = age,  fill = gender))+ geom_histogram(col = "grey")}
        if(y == "plot2"){plot <- ggplot(df, aes(x = gender, y = hday, fill = gender)) + geom_boxplot()}
        if(y == "plot3"){plot <- ggplot(df, aes(x = gender, ..count..))         + geom_bar(aes(fill=wday),position = "dodge")}
        if(y == "plot4"){plot <- ggplot(df, aes(x = prof, ..count..))         + geom_bar(aes(fill=wday),position = "dodge")}
        if(y == "plot5"){plot <- ggplot(ggplot(df, aes(x = gender, ..count..))                            + geom_bar(aes(fill=prof),position = "dodge"))}
        if(y == "plot6"){plot <- ggplot(df, aes(x = age,  fill = prof))         + geom_histogram(col = "grey")}
    }
    return(plot)
}

