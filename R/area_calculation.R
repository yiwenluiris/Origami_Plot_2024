#' @title Function to calculate area of the generated polygon
#' @param df input dataframe in the required format
#' @details This function serves as a supplementary tool to compute the area of each generated origami
#' plot when the maximal area achievable within the defined parameters (when all the variables attain 1) is set to 1.
#' The resulting calculated area offers an interpretation of the proportion between the actual origami plot and the
#' maximum achievable area.
#'
#' @return result
#'
#' @examples
#' data(sucra)
#' area_calculation(sucra)
#'
#' @export

area_calculation <- function(df){
  names <- rownames(df)
  result_list <- c()
  for(i in 1:nrow(df)){
    area <- sum(df[i,])/ncol(df)
    area <- round(area,digits = 2)
    result_temp <- paste0('The area of ', names[i], ' is ',area,'.')
    result_list <- c(result_list,result_temp)
  }
  result <- paste0(result_list)
  return(result)
}
