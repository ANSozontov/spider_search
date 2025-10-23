library(tidyverse)
library(RPostgreSQL)
library(sf)

con <- dbConnect()

   # create table
#s <- readxl::read_excel("spider_data_2025-10-16.xlsx")
#s_1 <- s %>% 
    #select(-23, -24)
#dbWriteTable(con, "spiders_test", s_1)

   # create dictionary ?????
adm_dict <- readxl::read_excel("adm_list_2025-03-17.xlsx")

   # test
input <- list()
input$sql_adult <- T
input$sql_juv <- F
input$sql_males <- TRUE
input$sql_females <- F
input$year1 <- 1985
input$year2 <- 1992
input$lat1 <- NA
input$lat2 <- NA
input$lon1 <- NA
input$lon2 <- NA
input$sql_fam <- "Thomisidae"
input$sql_gen <- "Xysticus "
input$sql_sp <- NA
input$sql_province <- NA


   # search list
search_list<- list()

   # life stage
if (input$sql_adult == TRUE) {
    if (input$sql_juv == TRUE) {
        lifestage_query <- " "
    } else {
        lifestage_query <- "\"lifeStage\" = 'adult'"
    }
} else if(input$sql_juv == TRUE){
    lifestage_query <- "\"lifeStage\" = 'juvenile'"  
} else { 
    lifestage_query <- " "
}
search_list$lifestage <- lifestage_query
#lifestage_query

   # sex
if (input$sql_males == TRUE) {
    if (input$sql_females == TRUE) {
        sex_query <- " "
    } else {
        sex_query <- "sex = 'male'"
    }
} else if(input$sql_females == TRUE){
    sex_query <- "sex = 'female'"  
}  else { 
    sex_query <- " "
}
search_list$sex <- sex_query
#sex_query

   # year
if(is.null(input$year1) || is.na(input$year1)){
    if(is.null(input$year2) || is.na(input$year2)){
        year_query <- " "
    } else {
        year_query <- paste0(" year <= ", input$year2) 
    }
} else if(is.null(input$year2) || is.na(input$year2)){
    year_query <- paste0(" year >= ", input$year1)   
} else {
    year_query <- paste0(" year BETWEEN ", min(c(input$year1, input$year2)), 
                         " AND ",  max(c(input$year1, input$year2)))
} 
search_list$year <- year_query
#year_query

   #latcoord
if(is.null(input$lat1) || is.na(input$lat1)){
    if(is.null(input$lat2) || is.na(input$lat2)){
        lat_query <- " "
    } else {
        lat_query <- paste0(" \"decimalLatitude\" <= ", input$lat2) 
    }
} else if(is.null(input$lat2) || is.na(input$lat2)){
    lat_query <- paste0(" \"decimalLatitude\" >= ", input$lat1)   
} else {
    lat_query <- paste0(" \"decimalLatitude\" BETWEEN ", min(c(input$lat1, input$lat2)), 
                        " AND ",  max(c(input$lat1, input$lat2)))
} 
search_list$lat <- lat_query
#lat_query

   #longcoord
if(is.null(input$lon1) || is.na(input$lon1)){
    if(is.null(input$lon2) || is.na(input$lon2)){
        lon_query <- " "
    } else {
        lon_query <- paste0(" \"decimalLongitude\" <= ", input$lon2) 
    }
} else if(is.null(input$lon2) || is.na(input$lon2)){
    lon_query <- paste0(" \"decimalLongitude\" >= ", input$lon1)   
} else {
    lon_query <- paste0(" \"decimalLongitude\" BETWEEN ", min(c(input$lon1, input$lon2)), 
                        " AND ",  max(c(input$lon1, input$lon2)))
} 
search_list$lon <- lon_query
#lon_query

   # taxon: family, genus, species 
   # family
if (!is.null(input$sql_fam) && 
    !is.na(input$sql_fam) && 
    nchar(str_squish(input$sql_fam)) > 1) {
    fam_query <- paste0("family ILIKE '%",str_squish(input$sql_fam),"%'" )
} else {
    fam_query <- " "
}
search_list$fam <- fam_query
#fam_query

   # genus
if (!is.null(input$sql_gen) && 
    !is.na(input$sql_gen) && 
    nchar(str_squish(input$sql_gen)) > 1) {
    gen_query <- paste0("genus ILIKE '%",str_squish(input$sql_gen),"%'" )
} else {
    gen_query <- " "
}
search_list$gen <- gen_query
#gen_query

   # species
if (!is.null(input$sql_sp) && 
    !is.na(input$sql_sp) && 
    nchar(str_squish(input$sql_sp)) > 1) {
    sp_query <- paste0("\"specificEpithet\" ILIKE '%",str_squish(input$sql_sp),"%'" )
} else {
    sp_query <- " "
}
search_list$sp <- sp_query
#sp_query

   # region ?????
if (!is.null(input$sql_province) && 
    !is.na(input$sql_province) && 
    nchar(str_squish(input$sql_province)) > 1) {
    if (grepl("^[a-zA-Z\\s]+$", 
              str_squish(input$sql_province), perl = TRUE) == TRUE){
        prov_query <- paste0("\"stateProvince\"  ILIKE '%",
                             str_squish(input$sql_province),"%'" ) 
    } else if (grepl("^[А-Яа-яЁё\\s\\-]+$",
                     str_squish(input$sql_province), perl = TRUE) == TRUE){
        idx <- grep(str_squish(input$sql_province), adm_dict$region, ignore.case = TRUE)
        if (length(idx) > 0) {
            prov_query <- paste0("\"stateProvince\"  ILIKE '%",
                                 adm_dict$en_region[idx[1]],"%'" ) 
        } else {
            prov_query <- " "
        }
    }
} else { 
    prov_query <- " "
}
search_list$prov <- prov_query
#prov_query

   #full query
all_queries <- search_list %>% 
    Filter(function(x) x != " ", .) %>%
    unlist()
if (!is.null(all_queries) && 
       length(all_queries) > 0 && 
       all(!is.na(all_queries))){
    query <- paste0("SELECT * FROM spiders_test WHERE ", 
                    paste0(all_queries, collapse = " AND "), 
                    ";")
    query <- str_squish(query) %>%
        str_replace("WHERE AND", "WHERE") %>%
        gsub("(AND\\s+)+", "AND ", .)
} else {
    query <- "SELECT * FROM spiders_test;"
}
 #query

   #result
result <- dbGetQuery(con, query) %>% 
    as_tibble()