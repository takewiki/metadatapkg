
#' 读取数据进入源数据表
#'
#' @param conn 连接
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' metadata_read()
metadata_read <- function(conn= tsda::conn_rds('metadata'),file_name = "data-raw/metadata.xlsx") {

  data <- readxl::read_excel(file_name)
  data$FEntityName <- tsdo::na_replace(data$FEntityName,'')
  data$FAuxKey <- tsdo::na_replace(data$FAuxKey,'')
  data$FDefaultValue <- tsdo::na_replace(data$FDefaultValue,'')
  data$FTableName <- tsdo::na_replace(data$FTableName,'')
  data$FTableFieldName <- tsdo::na_replace(data$FTableFieldName,'')
  data$FViewName <- tsdo::na_replace(data$FViewName,'')
  data$FViewFieldName <- tsdo::na_replace(data$FViewFieldName,'')
  #View(data)


  tsda::db_writeTable(conn=conn,table_name = 't_api_erp_kdc',r_object = data,append = T)

}


