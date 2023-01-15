#'  Transform a data.frame

#' Transform a df into the columns needed for the analysis
#' @df needs to be assigned to the dataframe chosen.
#' @origin if the dataframe has a datetime column, we must chose the first date
#' in this dataframe
#' @return The data.frame coded.
#' @author Jorge Alcantara Espinosa
#' @examples
#' transformation(df)






transformation <- function(df){
    col = ncol(df)
    if(col < 12){
        miscol <- c((col+1):12)
        df[miscol] <- NA
        colnames(df) <- c("center","act","prof",
                          "gender","age","location"
                          ,"actw","acts","wday","hday","census","nday")
        cols <- c("center","act","prof","gender","location","actw","acts","wday","census")
        df[cols] <- lapply(df[cols], factor)
        df$hday <- as.POSIXct(df$hday, tz = "", format = "%H:%M")
    }


    if(ncol(df) > 12){
        df <- df[1:12]
        colnames(df) <- c("center","act","prof",
                          "gender","age","location"
                          ,"actw","acts","wday","hday","census","nday")
        cols <- c("center","act","prof","gender","location","actw","acts","wday","census")
        df[cols] <- lapply(df[cols], factor)
        df$hday <- as.POSIXct(df$hday, tz = "", format = "%H:%M",
                              origin = df$hday[1])
    }

    if(ncol(df) == 12){
        colnames(df) <- c("center","act","prof",
                          "gender","age","location"
                          ,"actw","acts","wday","hday","census","nday")
        cols <- c("center","act","prof","gender","location","actw","acts","wday","census")
        df[cols] <- lapply(df[cols], factor)
        df$hday <- as.POSIXct(df$hday, tz = "", format = "%H:%M")
    }
    df <<- df
}
