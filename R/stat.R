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
#' metadata_createStat()
metadata_createStat <- function(conn= tsda::conn_rds('metadata'),
                                FFormName ='销售订单',FActionDesc ='保存',FOwnerName ='CP') {

  sql <- paste0(" select FFormId,FActionName,FEntityName,Ftype,FOwnerName,FFormName,FActionDesc,
 FTableName +FEntityName as FTableName,
 FTableKey,
 FViewName,
 FAccessToken

 from t_api_kdc_entity where FFormName ='",FFormName,"' and FActionDesc ='",FActionDesc,"' and FOwnerName ='",FOwnerName,"'
 and Ftype <> 'entry'")
  data = tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount >0){
    col_name = names(data)

    FViewName = unique(data$FViewName)
    view_name = paste0(FViewName,"Stat")
    token = unique(data$FAccessToken)
    dbName = metadata_getDbName(FToken =token)
    if(dbName !=''){
      conn_target =  tsda::conn_rds(dbName)
      sql_list = lapply(1:ncount,function(i){
        item = data[i,]
        FFormId = item$FFormId
        FActionName = item$FActionName
        FEntityName =  item$FEntityName
        Ftype =  item$Ftype
        FOwnerName = item$FOwnerName
        FFormName =  item$FFormName
        FActionDesc = item$FActionDesc
        FTableName =  item$FTableName
        FTableKey = item$FTableKey
        if(Ftype =='head'){
          sql_unit = paste0("select
  '",FFormId,"' as FFormId,
  '",FActionName,"'  as  FActionName,
  '' as FEntityName,
  'head' as Ftype,
  '",FOwnerName,"' as FOwnerName,
  '",FFormName,"' as FFormName,
  '",FActionDesc,"' as FActionDesc,
  '",FTableName,"' as FTableName,
  ",FTableKey," ,
  count(1)-1  as FListCount
  from   ",FTableName,"
  group by  ",FTableKey,"")
        }else{
          sql_unit = paste0("select
  '",FFormId,"' as FFormId,
  '",FActionName,"'  as  FActionName,
  '",FEntityName,"' as FEntityName,
  '",Ftype,"' as Ftype,
  '",FOwnerName,"' as FOwnerName,
  '",FFormName,"' as FFormName,
  '",FActionDesc,"' as FActionDesc,
  '",FTableName,"' as FTableName,
  ",FTableKey,"  ,
  count(1)   as FListCount
  from   ",FTableName,"
  group by  ",FTableKey,"")

        }
        return(sql_unit)





      })

      sql_all = unlist(sql_list)
      sql_res = paste(sql_all,sep = " ",collapse = " union ")
      res =  paste0("create view  ",view_name,"  as ",sql_res)
      if(metadata_objectIsNew(FToken = token,objectName =view_name )){
        tsda::sql_update(conn_target,res)
      }
    }


  }



  return(res)
}
