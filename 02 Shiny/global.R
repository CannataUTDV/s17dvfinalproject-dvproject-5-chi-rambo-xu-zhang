# global.R
require(readr)
require(lubridate)
require(dplyr)
require(data.world)

online0 = TRUE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
    dataset="andyzhang/final-project", type="sql",
    query="select Year, TotalVictims
    from MassShooting
    order by 1"
  ) 
  # View(globals)
} else {
  file_path = "www/MassShooting.csv"
  df <- readr::read_csv(file_path) 
  globals <- df %>% dplyr::select(Year, TotalVictims) %>% dplyr::distinct()
}

