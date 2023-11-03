#' Extended details about table columns
#'
#' @param conn A DBI Connection Object
#' @param name A table name in the DB
#' @param schema A table schema in the DB
#' @param catalog A table catalog in the DB
#'
#' @export
dbColumnInfoExtended <- function(conn, name, schema, catalog) {

  rdbms <- conn %>% class() %>% as.character()

  if (rdbms == "PqConnection") {
    col_info <- DBI::dbGetQuery(
      conn = conn,
      statement = "
        SELECT
          column_name,
          data_type,
          CASE is_nullable WHEN 'NO' THEN FALSE ELSE TRUE END AS is_nullable,
          CASE is_identity WHEN 'NO' THEN FALSE ELSE TRUE END AS is_identity,
          CASE is_generated WHEN 'NO' THEN FALSE ELSE TRUE END AS is_generated,
          CASE is_updatable WHEN 'NO' THEN FALSE ELSE TRUE END AS is_updatable,
          CASE identity_generation WHEN 'ALWAYS' THEN FALSE ELSE TRUE END AS can_insert_id
        FROM
          information_schema.columns
        WHERE
          table_name = $1
          AND table_schema = $2
          AND table_catalog = $3;",
      params = list(name, schema, catalog)
    )
  } else if (rdbms == "MySQLConnection") {
    col_info <- DBI::dbGetQuery(
      conn = conn,
      statement = "
          SELECT
            COLUMN_NAME AS column_name,
            DATA_TYPE AS data_type,
            CASE WHEN IS_NULLABLE = 'NO' THEN FALSE ELSE TRUE END AS is_nullable,
            CASE WHEN COLUMN_KEY = 'PRI' AND EXTRA = 'auto_increment' THEN TRUE ELSE FALSE END AS is_identity,
            CASE WHEN EXTRA = 'auto_increment' THEN TRUE ELSE FALSE END AS is_generated,
            CASE WHEN PRIVILEGES LIKE '%update%' THEN TRUE ELSE FALSE END AS is_updateable,
            CASE WHEN COLUMN_KEY = 'PRI' AND EXTRA = 'auto_increment' AND PRIVILEGES LIKE '%insert%' THEN TRUE ELSE FALSE END AS can_insert_id
        FROM
          information_schema.columns
        WHERE
          table_name = ?
          AND table_schema = ?
          AND table_catalog = ?;",
      params = list(name, schema, catalog)
    )
  } else if (rdbms == "Microsoft SQL Server") {
    warning("Auto-detection for identities and auto-generated sequences not yet implemented for SQL Server.")

    col_info <- DBI::dbGetQuery(
      conn = conn,
      statement = "
        SELECT
          COLUMN_NAME AS column_name,
        	DATA_TYPE AS data_type,
          IS_NULLABLE AS is_nullable,
        	CAST(0 AS BIT) AS is_identity,
        	CAST(0 AS BIT) AS is_generated,
        	CAST(0 AS BIT) AS is_updateable,
        	CAST(1 AS BIT) AS can_insert_id
        FROM
          INFORMATION_SCHEMA.COLUMNS
        WHERE
          TABLE_NAME = ?
          AND TABLE_SCHEMA = ?
          AND TABLE_CATALOG = ?;",
      params = list(name, schema, catalog)
    )
  } else {
    stop("No implementation planned for DB backend.")
  }

  return(col_info)
}
