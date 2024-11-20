#### TP Titanic packages

#' taux de survie en fonction du sex
#'
#' @param data data frame
#' @return A list
#' @examples
#' taux_survie_sex(TitanicSurvival)
#' @export

taux_survie_sex <- function(data) {
  library(dplyr)
  return(data %>%
           group_by(sex) %>%
           summarize(TauxSurvie = sum(survived == "yes") / n()))
}


######################
######################

#' taux de survie en fonction de la classe
#' 
#' @author djawad
#' @param data data frame
#' @return A list
#' @examples
#' taux_survie_class(TitanicSurvival)
#' @export

taux_survie_class <- function(data) {
  library(dplyr)
  return(data %>%
           group_by(passengerClass) %>%
           summarize(TauxSurvie = sum(survived == "yes") / n()))
}
