#' Uses an IMO number to obtain information about a vesel
#' 
#' @param imo International Maritime Organisation number
#' 
#' @return Array of info describing the vessel
#' \itemize{
#'   \item imo - A unique seven-digit number assigned to identify it internationally.
#'   \item mmsi - (Maritime Mobile Service Identity) is a nine-digit number used for identifying and communicating with vessels via VHF radio or satellite.
#'   \item callsign - A unique combination of letters and numbers used to identify the vessel in communications and can be used to track the vessel's movement.
#'   \item flag - Represents the country in which it is registered and provides information about the vessel's ownership and the laws and regulations under which it operates.
#'   \item vessel_name - A distinctive name given to the ship, which is used for identification purposes and to establish ownership.
#'   \item year_built - Provides information on the age of the ship, which can affect its condition, safety features, and compliance with current regulations.
#'   \item length - The length of a vessel provides important information about its size and capacity, which affects its stability, maneuverability, and suitability for different types of cargo.
#'   \item beam - Provides information about the maximum width of the ship, which is useful in determining its stability, speed and cargo capacity.
#'   \item gross_tonnage - Provides information about the ship's carrying capacity and volume, and is used to calculate various fees and taxes.
#'   \item summer_dwt - A measure of its carrying capacity and indicates the maximum weight of cargo and supplies that the ship can safely transport during the summer months.
#' }
#' 
#' @importFrom httr GET user_agent
#' @importFrom dplyr %>%
#' @importFrom rvest read_html html_element html_text html_form html_form_set 
#' @importFrom stringr str_split_fixed
#' @export
from_imo <- function(imo){
    url <- paste("https://www.vesselfinder.com/vessels/details/", imo, "/", sep="")

    # Retrieve the html
    ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    html <- read_html(GET(url, user_agent(ua)))

    # Retrieve IMO and MMSI number
    imo_mmsi <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[7]/td[2]") %>% html_text()
    imo_mmsi <- str_split_fixed(imo_mmsi, " / ", 2)
    imo <- imo_mmsi[1]
    mmsi <- imo_mmsi[2]

    callsign <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[8]/td[2]") %>% html_text()
    flag <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[9]/td[2]") %>% html_text()
    vessel_name <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[2]/td[2]") %>% html_text()

    year_built <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[11]/td[2]") %>% html_text()

    # length and beam are measured in metres
    length <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[8]/td[2]") %>% html_text()
    beam <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[9]/td[2]") %>% html_text()

    # gross_tonnage and summer_dwt are measured in tons
    gross_tonnage <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[6]/td[2]") %>% html_text()
    summer_dwt <- html_element(html, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[7]/td[2]") %>% html_text()


    info <- c(imo, mmsi, callsign, flag, vessel_name, year_built, length, beam, gross_tonnage, summer_dwt)
    return(info)
}

#' Uses a vessel's name to obtain information about a vesel
#' 
#' @param name Name of vessel
#' 
#' @return Array of info describing the vessel
#' \itemize{
#'   \item imo - A unique seven-digit number assigned to identify it internationally.
#'   \item mmsi - (Maritime Mobile Service Identity) is a nine-digit number used for identifying and communicating with vessels via VHF radio or satellite.
#'   \item callsign - A unique combination of letters and numbers used to identify the vessel in communications and can be used to track the vessel's movement.
#'   \item flag - Represents the country in which it is registered and provides information about the vessel's ownership and the laws and regulations under which it operates.
#'   \item vessel_name - A distinctive name given to the ship, which is used for identification purposes and to establish ownership.
#'   \item year_built - Provides information on the age of the ship, which can affect its condition, safety features, and compliance with current regulations.
#'   \item length - The length of a vessel provides important information about its size and capacity, which affects its stability, maneuverability, and suitability for different types of cargo.
#'   \item beam - Provides information about the maximum width of the ship, which is useful in determining its stability, speed and cargo capacity.
#'   \item gross_tonnage - Provides information about the ship's carrying capacity and volume, and is used to calculate various fees and taxes.
#'   \item summer_dwt - A measure of its carrying capacity and indicates the maximum weight of cargo and supplies that the ship can safely transport during the summer months.
#' }
#' 
#' @importFrom httr user_agent
#' @importFrom dplyr %>%
#' @importFrom rvest read_html html_element html_text session html_form html_form_set session_submit session_follow_link
#' @importFrom stringr str_split_fixed
#' @export
from_name <- function(name){
    url <- "https://www.vesselfinder.com/vessels"

    # Creating session
    ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    session <- session(url, user_agent(ua))

    # Search query form
    form <- html_form(session)

    # Sets search query
    form[[1]] <- html_form_set(form[[1]], name = name)

    # Submits search query
    session <- session_submit(session, form[[1]], submit = "")

    # Navigates to the found ship's link
    session <- session_follow_link(session, xpath = "/html/body/div[1]/div/main/div/table/tbody/tr/td[1]/a")

    # Retrieve IMO and MMSI number
    imo_mmsi <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[7]/td[2]") %>% html_text()
    imo_mmsi <- str_split_fixed(imo_mmsi, " / ", 2)
    imo <- imo_mmsi[1]
    mmsi <- imo_mmsi[2]

    callsign <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[8]/td[2]") %>% html_text()
    flag <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[2]/div/div[2]/div/div[2]/table/tbody/tr[9]/td[2]") %>% html_text()
    vessel_name <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[2]/td[2]") %>% html_text()

    year_built <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[11]/td[2]") %>% html_text()

    # length and beam are measured in metres
    length <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[8]/td[2]") %>% html_text()
    beam <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[9]/td[2]") %>% html_text()

    # gross_tonnage and summer_dwt are measured in tons
    gross_tonnage <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[6]/td[2]") %>% html_text()
    summer_dwt <- html_element(session, xpath = "/html/body/div[1]/div/main/div/section[5]/div/div[1]/table/tbody/tr[7]/td[2]") %>% html_text()


    info <- c(imo, mmsi, callsign, flag, vessel_name, year_built, length, beam, gross_tonnage, summer_dwt)
    return(info)
}
