DBI::dbWriteTable(
    con, "spiders", append = TRUE, row.names = FALSE, 
    value = `colnames<-`(d, nm$nm))