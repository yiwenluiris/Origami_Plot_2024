#' @title Function to prepare the data into designated format
#' @param df dataset with each column representing a variable name paired with its value and each row representing a graph
#' @param min_value auxiliary point in the graph, default is min(df)/2
#' @details This function takes a single-row dataframe as input and output a formatted dataframe.
#' It introduces an auxiliary point for each variable, positioned equidistantly from the central point along auxiliary axes.
#' Users can customize the distance from the point to the center.
#' Without user customization, the distance defaults to half of the smallest value within the dataset.
#' @return df
#'
#' @examples
#' data(sucra)
#' data_preparation(sucra,min_value=0.15)
#'
#' @export

data_preparation <- function(df, min_value = NULL){
  df_names <- names(df)
  n_prime <- ncol(df)
  if(is.null(min_value)){
    min_value <- as.numeric(min(df)/2)
  }
  df <- data.frame(mapply(cbind, df, "aux"=min_value, SIMPLIFY=F))
  aux_array_even <- seq(2,2*n_prime,2)
  aux_array_odd <- seq(1,2*n_prime-1,2)
  colnames(df)[aux_array_even]=""
  colnames(df)[aux_array_odd]=df_names
  df <- as.data.frame(rbind(1,0,df))
  return(df)
}
