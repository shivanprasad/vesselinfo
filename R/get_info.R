#' Uses an IMO number to obtain information about a vesel
#' 
#' @param imo International Maritime Organisation number
#' 
#' @return Array of info describing the vessel
#' 
#' @importFrom httr GET user_agent
#' @importFrom rvest read_html html_text session html_form html_form_set session_submit session_follow_link
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
#' @param name The name of a vessel
#' 
#' @return Array of info describing the vessel
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
