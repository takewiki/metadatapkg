
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


#' 获取数据库名称
#'
#' @param FToken 凭据
#' @param conn 连接
#'
#' @return 返回值
#'
#' @examples
#' metadata_getDbName()
metadata_getDbName <- function(conn = tsda::conn_rds('willingox'),FToken ='970C9E78-F276-4073-95AB-790A43BB38AB') {

sql <- paste0("select  FDbName    from t_sec_key where FToken ='",FToken,"'")
data <- tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount >0){
  res <- data$FDbName
}else{
  res <- ''
}
return(res)


}



#' 检查对象是否存在
#'
#' @param FToken 凭证
#' @param objectName 对象表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' metadata_objectIsNew()
metadata_objectIsNew <- function(FToken ='970C9E78-F276-4073-95AB-790A43BB38AB',objectName = 'rds_bd_material') {
dbName =  metadata_getDbName(conn = tsda::conn_rds('willingox'),FToken =FToken)
conn <- tsda::conn_rds(dbName)
sql <- paste0("select * from sys.objects where name = '",objectName,"' ")
data = tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount >0){
  res <- FALSE
}else{
  res <- TRUE
}

return(res)

}

#' 无数据创建表格
#'
#' @param conn 连接
#' @param FOwnerName 所有者
#' @param FFormName 表单名称
#' @param FActionDesc 操作名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' metadata_createTable()
metadata_createTable <- function(conn= tsda::conn_rds('metadata'),
                                 FFormName ='物料',FActionDesc ='保存',FOwnerName ='kingdee') {
  sql <- paste0(" select FTableName,FTableFieldName,FSqlDataType,FAccessToken,FType,FTableKey,FEntrykey  from t_api_erp_kdc
  where FFormName ='",FFormName,"' and FActionDesc ='",FActionDesc,"' and FOwnerName ='",FOwnerName,"'")
  print(sql)
  data <- tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount >0){
    token = data$FAccessToken[1]
    #print(token)
    dbName = metadata_getDbName(FToken =token)
    #print(dbName)
    if(dbName !=''){
      conn_target =  tsda::conn_rds(dbName)

      data_split = split(data,data$FTableName)
      res = lapply(data_split, function(item){

        tableName = unique(item$FTableName)
        # type can be head and entry  so we choose head one.
        type =  unique(item$FType)
        type = type[1]
        tableKey = unique(item$FTableKey)
        entryKey = unique(item$FEntrykey)
        #print('bug_type')
        #print(type)
        if(type == 'entryList'){
          sql_key = paste0(tableKey," nvarchar(100), ",entryKey," int, ")
        }else{
          sql_key = " "
        }

        sql_head = paste0("create table ",tableName,"  ")
        #做了更加精细化的管理
        #取代原来的根据FDataType + FDataLength的判断方式
        sql_body =  paste0(item$FTableFieldName,' ',item$FSqlDataType,collapse = ",")
        sql_all = paste0(sql_head,"(",sql_key,sql_body,")")

        if(metadata_objectIsNew(FToken = token,objectName =tableName )){
          tsda::sql_update(conn_target,sql_all)
        }
        return(sql_all)
      })

    }

  }else{
    res <- ''
  }
  return(res)



}


#' 构建视图SQL语句
#'
#' @param conn  连接
#' @param FOwnerName 所有者
#' @param FFormName 表单名称
#' @param FActionDesc 操作描述
#'
#' @return 返回值
#' @export
#'
#' @examples
#' metadata_createView()
metadata_createView <- function(conn= tsda::conn_rds('metadata'),
                                FFormName ='物料',FActionDesc ='保存',FOwnerName ='kingdee') {
  sql <- paste0(" select FTableName,FTableNameAlias,FTableFieldName, FViewName,FViewFieldName,FAccessToken,FType,FTableKey,FEntrykey  from t_api_erp_kdc
   where FFormName ='",FFormName,"' and FActionDesc ='",FActionDesc,"' and FOwnerName ='",FOwnerName,"'")
  #print(sql)
  data <- tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if( ncount >0){
    token = data$FAccessToken[1]
    dbName = metadata_getDbName(FToken =token)
    if(dbName !=''){
      conn_target =  tsda::conn_rds(dbName)
      view_name =  unique(data$FViewName)
      sql_head = paste0("create view " ,view_name ," as ")
      # print(sql_head )

      fieldList = paste0(data$FTableNameAlias,".",data$FTableFieldName," as  ",data$FViewFieldName,collapse = ",")
      entryKey = paste0(' ',unique(data$FEntrykey),collapse = ",")
      #print(entryKey)
      sql_field = paste0("select  ",fieldList,entryKey)
      #print(sql_field)

      table_name =   unique( data[ ,c('FTableName','FTableNameAlias','FTableKey')])
      table_count = nrow(table_name)
      from_join = rep(' left join  ',table_count)
      from_join[1] = ' from '
      sql_on = paste0(' on t1.',table_name$FTableKey," = ",table_name$FTableNameAlias,".",table_name$FTableKey,' ')
      sql_on[1] = " "
      sql_table = paste0(from_join,table_name$FTableName,' ',table_name$FTableNameAlias,sql_on ,collapse = "")
      #print(sql_table)

      sql_all <- paste0(sql_head,' ',sql_field,' ',sql_table)
      if(metadata_objectIsNew(FToken = token,objectName =view_name )){
        tsda::sql_update(conn_target,sql_all)
      }
      #print(sql_all)


    }


  }



  return(sql_all)



}




