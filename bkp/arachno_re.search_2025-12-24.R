# initial -----------------------------------------------------------------
library(shiny)
library(leaflet)
# library(raster)
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(RPostgreSQL))

# raster data
if(file.exists("rasters/rasters.RData")){
    load("rasters/rasters.RData")
} else {
    tif_files <- dir("rasters", "tif")
    # tif_metad <- dir("rasters", "tsv") %>% 
    tif_metad <- c("other.tsv", "worldclim.tsv", "landcover.tsv" ) %>% 
        map_dfr(~readr::read_delim(paste0("rasters/", .x), "\t", show_col_types = FALSE))
    if(all(tif_files %in% tif_metad$file) & all(tif_metad$file %in% tif_files)) {
        rst <- tif_metad$file %>% 
            map(~raster::raster(paste0("rasters/", .x))) %>% 
            `names<-`(tif_metad$name)
        rm(tif_files, tif_metad)
        save.image("rasters/rasters.RData")
    } else {
        cli::cli_abort("Something wrong with TIFF files")
    }
}

cc <- function(){
    dbConnect(RPostgreSQL::PostgreSQL(),
              dbname = "rnf_db", # your database
              host = "localhost", 
              port = "5432",
              user = "spi_search", # your user
              password = readLines("/var/sec/spi_search.pass") # <- put paswd to this file
    )
}

adm_dict <- readxl::read_excel("srv/adm_list_2025-03-17.xlsx")
tax_list <- readxl::read_excel("srv/wsc_list_2025-08-08.xlsx") %>% 
    mutate (scientificname = paste0(genus, " ", species), .before = 6)


# taxa values
{
    con <- cc()
    tax_names <- dbGetQuery(con, "SELECT DISTINCT(scientificname)  FROM spiders where taxonrank ilike '%species%';") %>% 
        as_tibble() %>% 
        separate(1, into = c("g", "s"), sep = " ", extra = "drop") %>% 
        transmute(sp = paste0(g, " ", s)) %>% 
        pull(1) %>% 
        unique() %>% 
        sort()
    dbDisconnect(con)
        
        
        # tax_gen.names <- c(tax_names, pull(dbGetQuery(con, "select distinct(genus) from spiders;"), 1))
        
        # tax_names <- dbGetQuery(con, "SELECT DISTINCT taxonrank, verbatimidentification, genus, scientificname  FROM spiders;")
        # pull(1) 
        # c(tax_names, )
    # tax_names <- c(tax_names, pull(dbGetQuery(con, "select distinct() from spiders;"), 1))
}

# tax_dict
vcenter <- "/* Center the numeric input container vertically */
      .center-numeric {
        display: flex;
        align-items: center;   /* Vertical alignment */
        justify-content: center; /* Horizontal alignment (optional) */
        height: 12vh;         /* Full viewport height for demonstration */
      }
      /* Adjust label and input alignment if needed */
      .center-numeric .form-group {
        margin-bottom: 0;      /* Remove default bottom margin */
      }
    "

# server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    values <- reactiveValues(
        last_querry = "" , 
        last_table = tibble(x = numeric())
    )
    
    observeEvent(input$change_language, {
        shinyalert::shinyalert(
            title = "Смена языка",
            text = "Смена языка пока не реализована<br> <br>Language change is not implemented yet",
            type = "info", 
            html = TRUE
        )
    })
    
    output$download_block <- NULL 
    
    output$coords_filter  <- NULL
    
    output$box_fam <- renderUI({
        selectInput(
            "sql_fam", 
            label = NULL, 
            choices = c("", unique(tax_list$family)),
            multiple = TRUE,
            )
    })
    
    observeEvent(input$sql_sp, {  
        updateSelectizeInput(session, 'sql_sp', choices = c("", tax_names), server = TRUE)
    }, once = TRUE)
    
    # new way species
    output$box_scname <- renderUI({
        selectizeInput(
            "sql_sp",
            label = NULL,
            choices = NULL,
            # selected = NULL,
            # multiple = TRUE,
            # selectize = TRUE
        )
    })
    
    output$box_region <- renderUI({
        selectInput(
            "sql_province", 
            label = NULL, 
            choices = c("", unique(adm_dict$region), unique(adm_dict$en_region)),
            # selected = NULL,
            # multiple = TRUE,
            selectize = TRUE
        )
    })
    
    observeEvent(input$sql_coords, {
        if(input$sql_coords){
            output$coords_filter <- renderUI({tagList(fluidRow(
                column(
                    4,
                    div(
                        class = "center-numeric", 
                        numericInput("lon1", label = "West", value = -180, min = -180, max = 0)
                    )
                ),
                column(
                    4, 
                    numericInput(
                        "sql_lat2", 
                        label = "North", 
                        value = 90, min = 0, max = 90
                    ), 
                    numericInput("sql_lat1", label = "South", value = -90, min = -90, max = 0)
                ),
                column(
                    4,
                    div(
                        class = "center-numeric", 
                        numericInput("lon2", label = "East", value = 180, min = 0, max = 180)
                    )
                ), 
            ))})
        } else {
            output$coords_filter <- NULL
        }
        })

    observeEvent(input$search_run, {
        # browser()
        conditions <- character()
        
        # year
        {if(is.null(input$sql_year1) || is.na(input$sql_year1)){
            if(is.null(input$sql_year2) || is.na(input$sql_year2)){
                year_query <- " "
            } else {
                year_query <- paste0(" year <= ", input$sql_year2) 
            }
        } else if(is.null(input$sql_year2) || is.na(input$sql_year2)){
            year_query <- paste0(" year >= ", input$sql_year1)   
        } else {
            if(input$sql_year1 == 1770 && input$sql_year2 == 2025) {
                year_query <- " "
            } else if(input$sql_year1 == 1770){
                year_query <- paste0(" year <= ", input$sql_year2) 
            } else if(input$sql_year2 == 2025){
                year_query <- paste0(" year >= ", input$sql_year1)
            } else {
                year_query <- paste0(" year BETWEEN ", min(c(input$sql_year1, input$sql_year2)), 
                                     " AND ",  max(c(input$sql_year1, input$sql_year2)))    
            }
            
        } }
        conditions <- c(conditions, year_query)
        
        # TAXA
        {# family
            # browser()
            if (
                !is.null(input$sql_fam) #&& # !is.na(input$sql_fam) 
                ){
                if(length(input$sql_fam) == 1){
                    fam_query <- paste0("family ILIKE '%", input$sql_fam, "%'" )
                } else {
                    fam_query <- input$sql_fam %>% 
                        paste0(collapse = "%' OR family ILIKE '%") %>% 
                        paste0("family ILIKE '%", ., "%'" )
                }
            } else {
                fam_query <- " "
            }
            
            # genus
            if (!is.null(input$sql_gen) && 
                !is.na(input$sql_gen) && 
                nchar(str_squish(input$sql_gen)) > 1) {
                gen_query <- paste0("genus ILIKE '%",str_squish(input$sql_gen),"%'" )
            } else {
                gen_query <- " "
            }
            
            # species
            if (!is.null(input$sql_sp) &&
                !is.na(input$sql_sp) &&
                nchar(str_squish(input$sql_sp)) > 1) {
                sp_query <- paste0("scientificname ILIKE '%", str_squish(input$sql_sp),"%'" )
            } else {
                sp_query <- " "
            }}
        conditions <- c(conditions, fam_query)
        conditions <- c(conditions, gen_query)
        conditions <- c(conditions, sp_query)
        
        # COORDS
        {
            # browser()
            if(input$sql_coords) {
                
                # Не выполнять вообще никакого поиска 
                # если поля для координат заполнены значениями по умолчанию
                
                
                
                #latcoord
                if(is.null(input$sql_lat1) || is.na(input$sql_lat1)){
                    if(is.null(input$sql_lat2) || is.na(input$sql_lat2)){
                        lat_query <- " "
                    } else {
                        # if(input$sql_lat2 == 180){
                        #     lat_query <- " decimallatitude IS NOT NULL"
                        # } else {
                            lat_query <- paste0(" decimallatitude <= ", input$sql_lat2) 
                        # }
                    }
                } else if(is.null(input$sql_lat2) || is.na(input$sql_lat2)){
                    lat_query <- paste0(" decimallatitude >= ", input$sql_lat1)   
                } else {
                    lat_query <- paste0(" decimallatitude BETWEEN ", min(c(input$sql_lat1, input$sql_lat2)), 
                                        " AND ",  max(c(input$sql_lat1, input$sql_lat2)))
                } 
                
                
                #longcoord
                if(is.null(input$lon1) || is.na(input$lon1)){
                    if(is.null(input$lon2) || is.na(input$lon2)){
                        lon_query <- " "
                    } else {
                        lon_query <- paste0(" decimallongitude <= ", input$lon2) 
                    }
                } else if(is.null(input$lon2) || is.na(input$lon2)){
                    lon_query <- paste0(" decimallongitude >= ", input$lon1)   
                } else {
                    lon_query <- paste0(" decimallongitude BETWEEN ", min(c(input$lon1, input$lon2)), 
                                        " AND ",  max(c(input$lon1, input$lon2)))
                }
                
            } else {
                lat_query <- " "
                lat_query <- " "
            }
        }
        conditions <- c(conditions, lat_query)
        conditions <- c(conditions, lat_query)
        
        # region 
        {
            if (!is.null(input$sql_province) && 
                !is.na(input$sql_province) && 
                nchar(str_squish(input$sql_province)) > 1) {
                if (grepl("^[a-zA-Z\\s]+$", 
                          str_squish(input$sql_province), perl = TRUE) == TRUE){
                    prov_query <- paste0("stateprovince  ILIKE '%",
                                         str_squish(input$sql_province),"%'" ) 
                } else if (grepl("^[А-Яа-яЁё\\s\\-]+$",
                                 str_squish(input$sql_province), perl = TRUE) == TRUE){
                    idx <- grep(str_squish(input$sql_province), adm_dict$region, ignore.case = TRUE)
                    if (length(idx) > 0) {
                        prov_query <- paste0("stateprovince  ILIKE '%",
                                             adm_dict$en_region[idx[1]],"%'" ) 
                    } else {
                        prov_query <- " "
                    }
                }
            } else { 
                prov_query <- " "
            }
        }
        conditions <- c(conditions, prov_query)
        
        # Sex & Age
        {
            # sex
            if (input$sql_males == TRUE) {
                if (input$sql_females == TRUE) {
                    sex_query <- " "
                } else {
                    sex_query <- " (sex ilike 'male%' OR sex ilike '% male%')"
                }
            } else if(input$sql_females == TRUE){
                sex_query <- "sex ilike '%female%'"  
            }  else { 
                sex_query <- " "
            }
            
            
            # life stage
            if (input$sql_adult == TRUE) {
                if (input$sql_juv == TRUE) {
                    lifestage_query <- " "
                } else {
                    lifestage_query <- "lifestage ilike '%ad%'"
                }
            } else if(input$sql_juv == TRUE){
                lifestage_query <- "lifestage ilike '%juv%'"  
            } else { 
                lifestage_query <- " "
            }
        }
        conditions <- c(conditions, sex_query)
        conditions <- c(conditions, lifestage_query)
        
        # prepare querry
        # drop empty conditions
        conditions <- conditions[conditions != " "]
        
        query <- paste0("SELECT * FROM spiders WHERE ", 
                        paste0(conditions, collapse = " AND "), 
                        ";")
        values$last_query <- str_squish(query) %>%
            str_replace("WHERE AND", "WHERE") %>%
            str_replace("WHERE ;", " ;") %>%
            gsub("(AND\\s+)+", "AND ", .)
        
        showNotification(values$last_query, duration = 10, type = "message")
        
        con <- cc()
        # r2 <- dbGetQuery(con, "SELECT * FROM spiders;")  %>% rename_with(tolower) %>% as_tibble() %>% select (scientificname)
        values$last_table <- dbGetQuery(con, values$last_query) %>% 
            rename_with(tolower)
        # result <- dbGetQuery(con, values$last_query) # %>% as_tibble()
        dbDisconnect(con)
        
        # for download
        
        #for map
        for.map.result <- values$last_table %>%
            filter(!is.na(decimallatitude), !is.na(decimallongitude)) 
        
        if(nrow(for.map.result) > 0){
            output$results_map <- for.map.result %>% 
                arrange(occurrenceid) %>% 
                group_by(decimallongitude, decimallatitude) %>% 
                summarise(
                    N = paste0("находок: ", n()),
                    occurrenceid = paste0(occurrenceid, collapse = ", "),
                    .groups = "drop") %>% 
                st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) %>% 
                leaflet() %>% 
                addTiles() %>% 
                addCircleMarkers(
                    label = ~N,
                    popup = ~ occurrenceid
                ) %>% 
                renderLeaflet()
        } else {
            output$results_map <- leaflet() %>% addTiles() %>% renderLeaflet()
        }
        
        if(nrow(values$last_table)<1){
            output$results_table <- DT::renderDataTable({
                tibble(`Результаты поиска:` = "отсутствуют...")
            }, escape = FALSE)
            
            output$download_block <- NULL
            
            shinyalert::shinyalert(title = "Не найдено", text = "", type = "warning")
        } else if(nrow(values$last_table)>2000){
            output$results_table <- DT::renderDataTable({
                tibble(`Результаты поиска:` = c(
                    nrow(values$last_table), 
                    "Более 2000 находок отобразить не получится", 
                    "(Но вы все равно можете скачать их в виде файла и изучить самостоятельно)",
                    "Пожалуйста, конкретизируйте условия поиска"))
            }, escape = FALSE)
            
            output$download_block <- renderUI({tagList(
                HTML("<h4 style='text-align: center;'>Скачать найденное</h4>"),
                fluidRow(
                    column(
                        6,
                        downloadButton(
                            "download_xls", label = "Excel", style = "width:100%;"
                        ),
                    ),
                    column(
                        6, 
                        downloadButton(
                            "download_tsv", label = "tsv", style = "width:100%;"
                        ),
                    )
                ),
            )})
            
            shinyalert::shinyalert(
                title = paste0(nrow(values$last_table), " записей найдено"), 
                text = "Более 2000 находок отобразить не получится.
\n(Но вы все равно можете скачать их в виде файла и изучить самостоятельно)
\nПожалуйста, конкретизируйте условия поиска.", 
                type = "info")
        } else {
            # prepare short_ref and URL links
            # ...
            
            output$results_table <- DT::renderDataTable({
                values$last_table %>% 
                    mutate(y = str_extract(bibliographiccitation, "[:digit:]{4}")) %>% 
                    separate(bibliographiccitation, into = c("author"), sep = "[0-9]", extra = "drop") %>% 
                    mutate(
                        author = str_remove_all(author, "[:upper:]{1}\\."),
                        author = str_replace_all(author, " ,", ",")
                    ) %>% 
                    separate(author, c("a1", "a2", "a3"), sep = ", ", extra = "drop", fill = "right") %>% 
                    mutate(
                        author = case_when(
                            is.na(a2) ~ paste0(str_squish(a1), ", ", y),
                            is.na(a3) ~ paste0(str_squish(a1), ", ", str_squish(a2), ", ", y),
                            TRUE ~ paste0(str_squish(a1), " et al., ", y)
                        ), 
                        .keep = "unused"
                    ) %>% 
                    transmute(
                        article = case_when(
                            is.na(references) ~ author,
                            TRUE ~ paste0(
                                '<a href="',
                                references, '" target="_blank">',
                                author,
                                '</a>')
                        ),
                        Пол = sex,
                        Возраст = lifestage,
                        Страна = countrycode, # country
                        Регион = stateprovince,
                        Место = locality,
                        `N & E` = case_when(
                            !is.na(decimallatitude) & !is.na(decimallongitude) ~ 
                                paste0(round(decimallatitude,  1), "N, ", round(decimallongitude, 1), "E"),
                            TRUE ~ "–"
                        ),
                        Дата = eventdate,
                        Биотоп = habitat,
                        Семейство = family, # 
                        Вид = scientificname
                        # rank = substr(taxonrank, 1, 3)
                    )
            }, escape = FALSE)
            
            output$download_block <- renderUI({tagList(
                HTML("<h4 style='text-align: center;'>Скачать найденное</h4>"),
                fluidRow(
                    column(
                        6,
                        downloadButton(
                            "download_xls", label = "Excel", style = "width:100%;"
                        ),
                    ),
                    column(
                        6, 
                        downloadButton(
                            "download_tsv", label = "tsv", style = "width:100%;"
                        ),
                    )
                ),
            )})
            
            shinyalert::shinyalert(
                title = ("Успех!"), 
                text = paste0("Обнаружено находок: ", nrow(values$last_table)), 
                type = "success")
        }
    })
    
    observeEvent(input$drop_env, {
        browser()
        if(input$wc_temp || input$wc_hum || input$srtm_elev || input$landcover || input$hfp){
            updateCheckboxInput(session, "wc_temp", value = FALSE)
            updateCheckboxInput(session, "wc_hum", value = FALSE)
            updateCheckboxInput(session, "srtm_elev", value = FALSE)
            updateCheckboxInput(session, "landcover", value = FALSE)
            updateCheckboxInput(session, "hfp", value = FALSE)
            showNotification("Выбор параметров отменен", duration = 3, type = "message")
        } else {
            showNotification("Ничего не было выбрано ", duration = 3, type = "warning")
        }
    })
    
    observeEvent(input$download_env, {
        showModal(modalDialog(
            title="Подготовка данных",
            easyClose = FALSE,
            size = "m",
            footer = tagList(
                HTML("<br><br><br><br><p style='text-align: center; font-style: italic;'>Подготавливаем данные для извлечения</p>")
            )
        ))
        
        if(nrow(values$last_table)<1){
            # слишком мало
            Sys.sleep(1.5)
            removeModal(session)
            shinyalert::shinyalert(
                "Ничего не найдено",
                "Попробуйте изменить условия поиска \nи получить хотя бы одну запись",
                "warning"
            )
        } else if(nrow(values$last_table)>2000){ 
            # слишком много
            Sys.sleep(1.5)
            removeModal(session)
            shinyalert::shinyalert(
                "Найдено слишком много",
                paste(
                    "Ваши текущие результаты поиска содержат", nrow(values$last_table), "записей.",
                    "\n\nЭто слишком много \nдля прикрепления пространственных данных\n\n",
                    "Попробуйте конкретизировать условия поиска",
                    "\nчтобы получить не более 2000 записей."
                ),
                "warning"
            )
        } else if (nrow(filter(values$last_table, !is.na(decimallatitude), !is.na(decimallongitude)))<1) {
            # нет координат
            Sys.sleep(1.5)
            removeModal(session)
            shinyalert::shinyalert(
                "Нет координат",
                paste(
                    "Ваши результаты поиска не содержат \nзаписей с координатами.",
                    "\n\n\nПопробуйте изменить условия поиска",
                    "\n\nили обратитесь к разработчикам \nесли готовы произвести геопривязку этих записей."
                ),
                "warning"
            )
            
        } else {
            # В самый раз
            layers_to_extract <- c(
                input$st_srtm_elev,
                input$st_hfp,
                input$st_wc_temprature,
                input$st_wc_humidity,
                input$st_landcover
                ) %>% 
                `[`(list(1, 2, 3:13, 14:21, 22:33), .) %>% 
                flatten_int()
            
            coords_to_extract <-  values$last_table %>% 
                select(starts_with("decimal")) %>% 
                distinct() %>%
                filter(!is.na(decimallongitude), !is.na(decimallatitude)) %>% 
                st_as_sf(
                    crs = 4326, 
                    remove = FALSE,
                    coords = c("decimallongitude", "decimallatitude"))
            extracted_data <- list()
            
            for(i in 1:length(layers_to_extract)){
                showModal(modalDialog(
                    title= paste("Данные готовы, извлекаю:", i, "из", length(layers_to_extract)),
                    easyClose = FALSE,
                    size = "m",
                    footer = tagList(
                        HTML(paste0(
                            "<br><br><br><br><p style='text-align: center; font-style: italic;'>",
                            "Процент выполнения: ", 
                            round(i/length(layers_to_extract)*100),
                            "</p>"
                        ))
                    )
                ))
                
                extracted_data[[i]] <- raster::extract(rst[[i]], coords_to_extract)
            }
            
            suppressMessages(`<-`(extracted_data, map_dfc(extracted_data, cbind)))
            colnames(extracted_data) <- names(rst)[layers_to_extract]
            extracted_data <- cbind(coords_to_extract, extracted_data) %>% 
                st_drop_geometry %>% 
                mutate_all(~round(.x, 1))
            values$last_table <- left_join(
                values$last_table, 
                extracted_data, 
                by = c("decimallatitude", "decimallongitude")
            )
            
            # browser()
            output$download_block <- renderUI({tagList(
                HTML("<h4 style='text-align: center;'>Скачать найденное</h4>"),
                fluidRow(
                    column(
                        6,
                        downloadButton(
                            "download_xls", label = "Excel", style = "width:100%; background-color: #ADFF2F; border-color: #006400"
                        ),
                    ),
                    column(
                        6, 
                        downloadButton(
                            "download_tsv", label = "tsv", style = "width:100%; background-color: #ADFF2F; border-color: #006400"
                        ),
                    )
                ),
            )})
            removeModal(session)
            shinyalert::shinyalert(
                "Параметры среды успешно подготовлены и добавлены к вашим данным",
                "Вы можете скачать их\nв удобном формате (xlsx/tsv),\nнажав на зелёные кнопки\nна панели слева.",
                "success"
            )
            
        }
        
        
        
    })
    
    output$download_xls <- {downloadHandler(
        filename = function() {
            paste0("result_", format(Sys.time(), "%d-%m-%Y_%Hh%Mm"), ".xlsx")
        },
        content = function(file) {
            values$last_table %>% 
            select(-remove, -publ_id, -vol_ids) %>% 
            writexl::write_xlsx(path = file)
            
        }
    )}
    
    output$download_tsv <- {downloadHandler(
        filename = function() {
            paste0("result_", format(Sys.time(), "%d-%m-%Y_%Hh%Mm"), ".tsv")
        },
        content = function(file) {
            values$last_table %>% 
            select(-remove, -publ_id, -vol_ids) %>% 
            readr::write_delim(
                .,
                file = file, 
                delim = "\t", 
                na = ""
            )
        }
    )}
    
}


# UI ----------------------------------------------------------------------
ui <- fluidPage( 
    shinyjs::useShinyjs(),
    fluidRow(
        column(
            width = 1, 
            tags$div(style="position: relative; margin-right: 90px", 
                     tags$img(src="logo.png", height = "80px"))
        ), 
        column(width = 10, titlePanel(
            "Arachno(RE)search: поиск литературных данных о находках пауков", 
            windowTitle = "Arachno(RE)search"
        )),
        column(1, actionButton("change_language",
            "RU", 
            icon = icon("globe"),
            style = "position: absolute; top: 8px; right: 5px; z-index:10000;"))
    ),
    tags$style(HTML(".black-hr {border: 1px solid black;margin: 20px 0;}")),
    tags$head(
        tags$link(rel="shortcut icon", href="favicon.ico"), 
        tags$style(HTML(vcenter)),
    ),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            # h2("Условия поиска"),
            # br(),
            HTML("<h4 style='text-align: left;'>Семейство</h4>"),
            uiOutput("box_fam"),
            # uiOutput("box_gen"),
            # uiOutput("box_spec"), 
            HTML("<h4 style='text-align: left;'>Вид (род + видовой эпитет)</h4>"),
            uiOutput("box_scname"),
            div(
                style = "text-align: right; font-size: 10px; font-style: italic;", 
                "*указать актуальное название, синонимия не учтена"
            ),
            # textInput(inputId = "sql_fam", label = "Семейство"),
            # textInput(inputId = "sql_gen", label = "Род"),
            #textInput(inputId = "sql_sp", label = "Вид", placeholder = "Актуальное название. Синонимия не учтена"),
            br(),
            fluidRow(
                column(6, actionButton(
                    inputId = "search_run", 
                    label = "Поиск", 
                    width = "100%", 
                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                column(6, actionButton(
                    inputId = "clear_search", 
                    label = "Очистить", 
                    width = "100%")
                )
            ),
            uiOutput("download_block"),
            hr(),
            # br(),
            HTML("<h4 style='text-align: center;'>Годы находок</h4>"),
            fluidRow(
                column(5, numericInput(
                    "sql_year1", 
                    label = NULL,
                    value = 1770,
                    min = 1770, 
                    max = year(Sys.Date()), 
                    width = "100%"
                    )
                ),
                column(2, HTML('<p style="text-align: center;">–</p>')),
                column(5, numericInput(
                    "sql_year2", 
                    label = NULL, 
                    value = year(Sys.Date()),
                    min = 1770, 
                    max = year(Sys.Date()), 
                    width = "100%"
                    )
                )
            ),
            # br(),
            h4("Координаты"),
            checkboxInput("sql_coords", "Только с координатами", value = FALSE),
            uiOutput("coords_filter"),
            # br(),
            h4("Регион"),
            uiOutput("box_region"), 
            #textInput(inputId = "sql_province", label = NULL),
            # br(),
            fluidRow(
               column(6,
                      h4("Пол"),
                      checkboxInput("sql_males", "Самцы", value = FALSE),
                      checkboxInput("sql_females", "Самки", value = FALSE)),
               column(6,
                      h4("Возраст"),
                      checkboxInput("sql_adult", "Взрослые", value = FALSE),
                      checkboxInput("sql_juv", "Ювенильные", value = FALSE))
           )
        ), 
        mainPanel(tabsetPanel(
            tabPanel(
                "Таблица с результатами",
                DT::DTOutput("results_table")
            ), 
            tabPanel(
                "Карта",
                leafletOutput("results_map")
            ), 
            tabPanel("Параметры среды", 
                h4("Выберите интересующие параметры"), 
                br(),
                hr(), 
                flowLayout(
                    tagList(
                        checkboxInput("st_wc_temprature", "WorldClim v. 2.1, температура"), 
                        br(),
                        HTML("<a href = 'https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/joc.5086' target = '_blank'>Fick & Hijmans (2017)</a>"),
                        br(),
                        hr(), 
                    ),
                    tagList(
                        checkboxInput("st_wc_humidity", "WorldClim v. 2.1, влажность"),
                        br(),
                        br(),
                        HTML("<a href = 'https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/joc.5086' target = '_blank'>Fick & Hijmans (2017)</a>"),
                        br(),
                        hr(), 
                    ),
                    tagList(
                        checkboxInput("st_srtm_elev", "Высота, м н.у.м."),
                        br(),
                        br(),
                        HTML("<a href = 'https://doi.org/10.5069/G9445JDF' target = '_blank'>NASA Shuttle Radar, 2013</a>"),
                        br(),
                        hr(), 
                    ),
                    tagList(
                        checkboxInput("st_landcover", "Классификация поверхности (land-cover)"), 
                        br(),
                        HTML("<a href = 'https://doi.org/10.1111/geb.12182' target = '_blank'>Tuanmu, Jetz, 2014</a>"),
                        # br(),
                        br(),
                        hr(), 
                    ),
                    tagList(
                        checkboxInput("st_hfp", "Степень антропогенного воздействия"), 
                        br(),
                        HTML("<a href = 'https://doi.org/10.5061/dryad.ttdz08m1f' target = '_blank'>Francis et al., 2023</a>"),
                        br(),
                        hr(), 
                    ),
                    tagList(),
                    tagList(
                        br(),
                        br(),
                        actionButton(
                            "drop_env", 
                            "Сбросить", 
                            width = "100%"
                        ),
                        br(),
                        br(),
                        actionButton(
                            "download_env", 
                            "Получить", 
                            width = "100%",
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                        )
                    )
                )
            ), 
            tabPanel(
                "Характеристики видов",
                br(),
                HTML("Находится на стадии тестирования, подождите")
            ), 
        ))
    ) 
)

# Run the app -------------------------------------------------------------
shinyApp(ui = ui, 
         server = server, 
         onStart = function() {
             onStop(function() {
                 cat("Doing application cleanup...\n")
                 # dbDisconnect(con)
                 cat("Application cleanup is done!\n")
             })
         }, 
         options = list(port = 3333, host = "0.0.0.0", launch.browser = F)
)


# output$box_gen <- renderUI({
#     selectInput(
#         "sql_gen", 
#         label = "Род", 
#         choices = c("", unique(tax_list$genus)),
#         # selected = NULL,
#         # multiple = TRUE,
#         selectize = TRUE
#     )
# })

# output$box_spec <- renderUI({
#     selectInput(
#         "sql_sp", 
#         label = "Вид", 
#         choices = c("", unique(tax_list$species)),
#         # selected = NULL,
#         # multiple = TRUE,
#         selectize = TRUE
#     )
# })