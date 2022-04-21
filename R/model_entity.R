#' 删除模型中的数据
#'
#' @param conn 连接
#' @param FOwnerName 所有者名称
#' @param FFormName 表名
#' @param FActionDesc 操作名称
#'
#' @return 返回值
#'
#' @examples
#' model_entity_delete()
model_entity_delete <- function(conn = tsda::conn_rds('metadata'),
                              FOwnerName ='CP',
                              FFormName ='客户',
                              FActionDesc ='保存'

                              ) {
sql <- paste0("delete  from t_api_kdc_entity where FOwnerName ='",FOwnerName,"'  and FFormName ='",FFormName,"' and FActionDesc ='",FActionDesc,"'")
tsda::sql_update(conn,sql)

}


#' 模型实体读取数据
#'
#' @param conn 连接
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entity_read()
model_entity_read <- function(conn = tsda::conn_rds('metadata'),
                 file_name = "data-raw/kdc_entityTable.xlsx"
                 ) {
  data <- readxl::read_excel(file_name)
  data$FEntityName <- tsdo::na_replace(data$FEntityName,'')
  FFormName = unique(data$FFormName)
  FActionDesc = unique(data$FActionDesc)
  FOwnerName = unique(data$FOwnerName)
  #删除已有数据
  model_entity_delete(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)
  #上传更新的数据
  tsda::db_writeTable(conn = conn,table_name = 't_api_kdc_entity',r_object = data,append = TRUE)
  return(data)

}

