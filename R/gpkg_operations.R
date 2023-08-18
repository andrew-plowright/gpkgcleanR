
#' @export
gpkg_table_delete <- function(gpkg_path, table_name){

  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg_path)

  # List of tables
  table_list <- DBI::dbListTables(con)

  # Check if table exists
  if(!table_name %in% table_list) stop("Could not find table '", table_name, "'")

  # Geometry column name
  extensions <- DBI::dbReadTable(con, 'gpkg_extensions')
  geometry_column_name <- extensions[extensions$table_name == table_name, "column_name", drop = TRUE]

  # Drop actual table
  result <- DBI::dbSendQuery(con, paste0("DROP TABLE ", table_name, ";"))
  DBI::dbClearResult(result)

  # Drop rtree
  result <- DBI::dbSendQuery(con, paste0("DROP TABLE rtree_", table_name,"_", geometry_column_name,";"))
  DBI::dbClearResult(result)

  # Delete row from 'gpkg_contents'
  result <- DBI::dbSendQuery(con, paste0("delete from gpkg_contents where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  # Delete row from 'gpkg_ogr_contents'
  result <- DBI::dbSendQuery(con, paste0("delete from gpkg_ogr_contents where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  # Delete row from 'gpkg_geometry_columns'
  result <-  DBI::dbSendQuery(con, paste0("delete from gpkg_geometry_columns where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  # Delete row from 'gpkg_extensions'
  result <-  DBI::dbSendQuery(con, paste0("delete from gpkg_extensions where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  # Delete row from 'sqlite_sequence'
  result <-  DBI::dbSendQuery(con, paste0("delete from sqlite_sequence where name = '",table_name,"';"))
  DBI::dbClearResult(result)

}


#' @export
gpkg_table_rename <- function(gpkg_path, table_name, new_name){

  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg_path)

  # List of tables
  table_list <- DBI::dbListTables(con)

  # Check of table exists
  if(!table_name %in% table_list) stop("Could not find table '", table_name, "'")

  # Geometry column name
  extensions <- DBI::dbReadTable(con, 'gpkg_extensions')
  geometry_column_name <- extensions[extensions$table_name == table_name, "column_name", drop = TRUE]

  # Rename actual table
  result <- DBI::dbSendQuery(con, paste0("ALTER TABLE ", table_name," RENAME to ", new_name, ";"))
  DBI::dbClearResult(result)

  # Rename rtree
  result <- DBI::dbSendQuery(con, paste0("ALTER TABLE rtree_", table_name,"_",geometry_column_name," RENAME to rtree_", new_name, "_",geometry_column_name,";"))
  DBI::dbClearResult(result)

  result <- DBI::dbSendQuery(con, paste0("update gpkg_contents set table_name = '", new_name, "' where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  result <- DBI::dbSendQuery(con, paste0("update gpkg_ogr_contents set table_name = '", new_name, "' where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  result <-  DBI::dbSendQuery(con, paste0("update gpkg_geometry_columns set table_name = '", new_name, "' where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  result <-  DBI::dbSendQuery(con, paste0("update gpkg_extensions set table_name = '", new_name, "' where table_name = '",table_name,"';"))
  DBI::dbClearResult(result)

  result <-  DBI::dbSendQuery(con, paste0("update sqlite_sequence set name = '", new_name, "' where name = '",table_name,"';"))
  DBI::dbClearResult(result)

  DBI::dbDisconnect(con)

}
