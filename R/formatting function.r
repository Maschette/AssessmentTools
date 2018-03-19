#' Formatting CMIX data to plot time series of Numbers/km2
#'
#' @param data
#' The CMIX input data.
#' @param Year
#' Year of the Survey.
#'
#'
#' @export

#This function uses gather to reformat the CMIX data into two columns called "Station" and "Density".
#It then creates a column for "Year" based of the input variable, and creates the "Haul" and "Strata" columns based on station.

#data is the input CMIX data
#year is the year of the data.

ANIformat<-function(data, year){
  tmp<-tidyr::gather(data, key=Station, value=Density, 2:ncol(data))
  tmp$Year<-year
  tmp$Haul<-gsub(".*[X]([^.]+)[_].*", "\\1", tmp$Station)
  tmp$tmp<-gsub(".*[_]([^.]+)", "\\1", tmp$Station) # return everything after the "_"
  tmp$Strata<-dplyr::if_else(tmp$tmp=="GR", "Gunnari Ridge",
                             dplyr::if_else(tmp$tmp=="PSE", "Plateau Southeast",
                                            dplyr::if_else(tmp$tmp=="PW","Plateau West", "Other")))
  tmp<-tmp %>% select(-tmp)
  return(tmp)
}

