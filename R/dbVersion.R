#' Gets SQL DB Version
#'
#' Given a DBI connection, evaluate the information to extract the DB Version.
#' This may be necessary when executing RDBMS-specific SQL syntax.
#'
#' @param conn A DBI Connection Object
#'
#' @examples
#' \dontrun{
#' db_con <- dbConnect(...)
#' dbVersion(db_con)
#' }
#'
#' @export
dbVersion <- function(conn) {
  rdbms <- conn %>% class() %>% as.character()

  if (rdbms == "PqConnection") {
    return(DBI::dbGetInfo(conn)[["db.version"]])
  } else if (rdbms == "MySQLConnection") {
    return(DBI::dbGetInfo(conn)[["serverVersion"]])
  } else if (rdbms == "Microsoft SQL Server") {
    return(DBI::dbGetInfo(conn)[["db.version"]])
  } else {
    return("unknown")
  }

  return(sql_version)
}
