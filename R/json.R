#' 读取json数据
#'
#' @param json_file 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getFromJson()
#'
model_getFromJson <- function(json_file='data-raw/material_save_cp.json') {
  data = jsonlite::fromJSON(json_file,simplifyVector = F)
  data_model = data$Model
  return(data_model)
}


#' 获取主数据表最大ID
#'
#' @param conn 连接
#'
#' @return 返顺值
#' @export
#'
#' @examples
#' model_getMaxId()
model_getMaxId <- function(conn = tsda::conn_rds('metadata')) {
sql <- paste0("select isnull(max(FInterId),0) as FinterId from t_api_erp_kdc")
data = tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount >0){
  res = data$FinterId
}else{
  res = 0
}

return(res)

}


#' 获取模型实体名称
#'
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName 操作
#' @param FType 类型
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityName()
model_getEntityName <- function(conn = tsda::conn_rds('metadata'),
                                FFormId = 'BD_MATERIAL',
                                FActionName ='Save',
                                FType='entry',
                                FOwnerName ='kingdee'
                                ) {
sql <- paste0("select  FEntityName   from t_api_kdc_entity
where  FFormId = '",FFormId,"' and FActionName ='",FActionName,"' and FType='",FType,"' and FOwnerName ='",FOwnerName,"'")
data = tsda::sql_select(conn,sql)
ncount = nrow(data)
if(ncount>0){
  res = data$FEntityName
}

return(res)

}



#' 模型返回头部标签
#'
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName  操作
#' @param FOwnerName  所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityName_head()
model_getEntityName_head <- function(conn = tsda::conn_rds('metadata'),
                                FFormId = 'BD_MATERIAL',
                                FActionName ='Save',
                                FOwnerName ='kingdee'
) {

  res = model_getEntityName(conn = conn,
                            FFormId = FFormId,
                            FActionName =FActionName,
                            FType='head',
                            FOwnerName =FOwnerName)
  return(res)

}
#' 模型返回表体
#'
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName  操作
#' @param FOwnerName  所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityName_entry()
model_getEntityName_entry <- function(conn = tsda::conn_rds('metadata'),
                                     FFormId = 'BD_MATERIAL',
                                     FActionName ='Save',
                                     FOwnerName ='kingdee'
) {

  res = model_getEntityName(conn = conn,
                            FFormId = FFormId,
                            FActionName =FActionName,
                            FType='entry',
                            FOwnerName =FOwnerName)
  return(res)

}

#' 模型返回表体列表
#'
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName  操作
#' @param FOwnerName  所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityName_entry()
model_getEntityName_entryList <- function(conn = tsda::conn_rds('metadata'),
                                      FFormId = 'BD_MATERIAL',
                                      FActionName ='Save',
                                      FOwnerName ='kingdee'
) {

  res = model_getEntityName(conn = conn,
                            FFormId = FFormId,
                            FActionName =FActionName,
                            FType='entryList',
                            FOwnerName =FOwnerName)
  return(res)

}

#' 模型返回表体列表
#'
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName  操作
#' @param FOwnerName  所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityName_entryAll()
model_getEntityName_entryAll <- function(conn = tsda::conn_rds('metadata'),
                                          FFormId = 'BD_MATERIAL',
                                          FActionName ='Save',
                                          FOwnerName ='kingdee'
) {

  res1 = model_getEntityName(conn = conn,
                            FFormId = FFormId,
                            FActionName =FActionName,
                            FType='entryList',
                            FOwnerName =FOwnerName)
  res2 = model_getEntityName(conn = conn,
                             FFormId = FFormId,
                             FActionName =FActionName,
                             FType='entry',
                             FOwnerName =FOwnerName)
  res =c(res2,res1)
  return(res)

}

#' 模型获取主键
#'
#' @param model 模型
#' @param conn 连接
#' @param FFormId 表单ID
#' @param FActionName 操作
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_head_MainKey()
#'
model_head_MainKey<- function(model,
                              conn = tsda::conn_rds('metadata'),
                              FFormId ='BD_MATERIAL',
                              FActionName ='Save',
                              FOwnerName ='kingdee'
                              ) {
  # entry and entryList
  engityName = model_getEntityName_entryAll(conn=conn,FFormId = FFormId,FActionName = FActionName,FOwnerName = FOwnerName)
  model_key = names(model)


  head_key = model_key[! model_key   %in% engityName]
  return(head_key)

}



#' 返回主键
#'
#' @param model 模型
#' @param entityName 名称
#' @param conn 连接
#' @param FFormId  表单ID
#' @param FActionName  操作
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_MainKey()
model_entry_MainKey<- function(model,entityName = 'SubHeadEntity',
                              conn = tsda::conn_rds('metadata'),
                              FFormId ='BD_MATERIAL',
                              FActionName ='Save',
                              FOwnerName ='kingdee'
) {
  data = model[[entityName]]
  res = names(data)
  if(FFormId == 'BD_MATERIAL'){
    #物料的后续处理
    #后续进行特殊处理
    flag = !res %in% 'FStockPlaceId'
    res =res[flag]
    #后续进行特殊处理
    flag2 = !res %in%'FPickBinId'
    res = res[flag2]
  }
  if(FFormId == 'SAL_SaleOrder'){
    flag3 = !res %in%'FSOStockLocalId'
    res = res[flag3]
    flag4 = !res %in%'FAuxPropId'
    res = res[flag4]
    flag5 = !res %in%'FOrderEntryPlan'
    res = res[flag5]
    flag6 = !res %in%'FTaxDetailSubEntity'
    res = res[flag6]
    flag7 = !res %in%'FSaleOrderPlanEntry'
    res = res[flag7]
    flag8 = !res %in%'FSalOrderTraceDetail'
    res = res[flag8]



  }

  return(res)



}

#' 返回主键
#'
#' @param model 模型
#' @param entityName 名称
#' @param conn 连接
#' @param FFormId  表单ID
#' @param FActionName  操作
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entryList_MainKey()
model_entryList_MainKey<- function(model,entityName = 'SubHeadEntity',
                               conn = tsda::conn_rds('metadata'),
                               FFormId ='BD_MATERIAL',
                               FActionName ='Save',
                               FOwnerName ='kingdee'
) {
  data = model[[entityName]][[1]]
  res = names(data)
  if(FFormId == 'BD_MATERIAL'){
    #物料的后续处理
    #后续进行特殊处理
    flag = !res %in% 'FStockPlaceId'
    res =res[flag]
    #后续进行特殊处理
    flag2 = !res %in%'FPickBinId'
    res = res[flag2]
  }
  if(FFormId == 'SAL_SaleOrder'){
    flag3 = !res %in%'FSOStockLocalId'
    res = res[flag3]
    flag4 = !res %in%'FAuxPropId'
    res = res[flag4]
    flag5 = !res %in%'FOrderEntryPlan'
    res = res[flag5]
    flag6 = !res %in%'FTaxDetailSubEntity'
    res = res[flag6]
    flag7 = !res %in%'FSaleOrderPlanEntry'
    res = res[flag7]
    flag8 = !res %in%'FSalOrderTraceDetail'
    res = res[flag8]

  }



  return(res)



}

#' 模型返回主值类型
#'
#' @param model 模型
#' @param head_key 键值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_head_key_class()
model_head_dataType <- function(model,mainKey) {
  head_key_class =lapply(mainKey, function(key){
    res = class(model[[key]])
    if(res=='intege'){
      res ='int'
    }else if(res =='character'){
      res ='nvarchar'

    }else if(res =='list'){
      res ='list'
    }else{
      res = res
    }
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}

#' 模型返回主值类型
#'
#' @param entityName  entity name
#' @param mainKey  main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataType()
model_entry_dataType <- function(model,entityName,mainKey) {
  model = model[[entityName]]
  head_key_class =lapply(mainKey, function(key){
    res = class(model[[key]])
    if(res=='intege'){
      res ='int'
    }else if(res =='character'){
      res ='nvarchar'

    }else if(res =='list'){
      res ='list'
    }else{
      res = res
    }
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}

#' 模型返回主值类型
#'
#' @param entityName  entity name
#' @param mainKey  main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataType()
model_entryList_dataType <- function(model,entityName,mainKey) {
  model = model[[entityName]][[1]]
  head_key_class =lapply(mainKey, function(key){
    res = class(model[[key]])
    if(res=='intege'){
      res ='int'
    }else if(res =='character'){
      res ='nvarchar'

    }else if(res =='list'){
      res ='list'
    }else{
      res = res
    }
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}



#' 模型返回主值是否列表
#'
#' @param model 模型
#' @param head_key 键值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_head_valueType()
model_head_valueType <- function(model,mainKey) {
  head_key_class =lapply(mainKey, function(key){
    res = is.list(model[[key]])
    if(res){
      info ='complex'
    }else{
      info ='simple'
    }
    return(info)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}

#' 模型返回主值是否列表
#'
#' @param entityName  entity name
#' @param mainKey main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_valueType()
model_entry_valueType <- function(model,entityName,mainKey) {
  model =  model[[entityName]]
  head_key_class =lapply(mainKey, function(key){
    res = is.list(model[[key]])
    if(res){
      info ='complex'
    }else{
      info ='simple'
    }
    return(info)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}
#' 模型返回主值是否列表
#'
#' @param entityName  entity name
#' @param mainKey main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_valueType()
model_entryList_valueType <- function(model,entityName,mainKey) {
  model =  model[[entityName]][[1]]
  head_key_class =lapply(mainKey, function(key){
    res = is.list(model[[key]])
    if(res){
      info ='complex'
    }else{
      info ='simple'
    }
    return(info)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}

#' 获取子键内容
#'
#' @param model 模型
#' @param head_key 主题
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_head_subKey_class()
model_head_auxKey <- function(model,mainKey) {
  head_key_class =lapply(mainKey, function(key){
    res = names(model[[key]])
    if(is.null(res)){
      res =''
    }
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}


#' 获取子键内容
#'
#' @param entityName entity name
#' @param mainKey main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_auxKey()
model_entry_auxKey <- function(model,entityName,mainKey) {
  model = model[[entityName]]
  head_key_class =lapply(mainKey, function(key){
    res = names(model[[key]])
    if(is.null(res)){
      res =''
    }
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}


#' 获取子键内容
#'
#' @param entityName entity name
#' @param mainKey main key
#' @param model 模型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_auxKey()
model_entryList_auxKey <- function(model,entityName,mainKey) {
  model = model[[entityName]][[1]]
  #i = 0
  head_key_class =lapply(mainKey, function(key){
    res = names(model[[key]])
    print(res)
    if(is.null(res)){
      res =''
    }
    #i = i+1
    #print(paste0(i,'-',mainKey,"~",res))
    return(res)
  })
  head_key_class = unlist(head_key_class)
  return(head_key_class)

}


#' 查询表名信息
#'
#' @param conn 连接
#' @param FOwnerName 所有者
#' @param Ftype 类型
#' @param FFormName 表名
#' @param FActionDesc 操作名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_getEntityList()
model_getEntityList<- function(conn = tsda::conn_rds('metadata'),
                                  FOwnerName = 'kingdee',
                                   Ftype ='head',
                                  FFormName ='物料',
                                  FActionDesc ='保存'
                                  ) {
sql <- paste0("SELECT  [FFormId]
      ,[FActionName]
      ,[FEntityName]
      ,[Ftype]
      ,[FOwnerName]
      ,[FFormName]
      ,[FActionDesc]
      ,[FTableName]
      ,[FViewName]
      ,[FAccessToken]
      ,[FTableKey]
      ,[FTableNameAlias]
  FROM [metadata].[dbo].[t_api_kdc_entity]
  where FOwnerName = '",FOwnerName,"' and Ftype ='",Ftype,"' and FFormName ='",FFormName,"' and FActionDesc ='",FActionDesc,"'")

data =tsda::sql_select(conn,sql)
return(data)
}





#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormId  表单
#' @param FActionName  动作
#' @param FOwnerName 所有者
#' @param FFormName  f
#' @param FActionDesc a
#' @param FTableName  t
#' @param FViewName  v
#' @param FAccessToken  a
#' @param FTableKey t
#' @param FTableNameAlias  a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_head_data()
model_head_data <- function(json_file='data-raw/material_save_cp.json',
                            conn = tsda::conn_rds('metadata'),
                            FFormId ='BD_MATERIAL',
                            FFormName = '物料',
                            FActionName ='Save',
                            FActionDesc ='保存',
                            FTableName = 'rds_bd_material',
                            FViewName = 'rds_vw_bd_material',
                            FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                            FTableKey ='FNumber',
                            FTableNameAlias ='t1',





                            FOwnerName ='kingdee'){
  model= model_getFromJson(json_file = json_file)
  FMainKey = model_head_MainKey(model=model,conn = conn,FFormId =FFormId,FActionName =FActionName,FOwnerName =FOwnerName)
  FDataType =model_head_dataType(model = model,mainKey = FMainKey)
  FValueType = model_head_valueType(model = model,mainKey = FMainKey)
  FAuxKey = model_head_auxKey(model=model,mainKey =FMainKey )
  data_head = data.frame(FMainKey,FDataType,FValueType,FAuxKey,stringsAsFactors = F)
  FDefaultValue = lapply(1:nrow(data_head), function(i){
    item = data_head[i,]

    FMainKey  =  item$FMainKey
    FDataType = item$FDataType
    FValueType = item$FValueType
    FAuxKey = item$FAuxKey
    if (FValueType =='complex'){
      value = model[[FMainKey]][[FAuxKey]]
    }else{
      value =  model[[FMainKey]]
    }
    return(value)
  })
  FDefaultValue = unlist(FDefaultValue)
  #处理数据，按表的顺序处理
  ncount = nrow(data_head)
  FOwnerName = rep(FOwnerName,ncount)
  FFormId = rep(FFormId,ncount)
  FFormName = rep(FFormName,ncount)
  FActionName =rep(FActionName,ncount)
  FActionDesc = rep(FActionDesc,ncount)
  FNodeName = rep('Model',ncount)
  Findex = 1:ncount
  Ftype = rep('head',ncount)
  FListCount = rep(0,ncount)
  FEntityName = rep('',ncount)
  FKeyCaption = rep('',ncount)
  FMustInput =rep(0,ncount)
  FIsShow = rep(0,ncount)
  FTableName = rep(FTableName,ncount)
  FTableFieldName = FMainKey
  FViewName = rep(FViewName,ncount)
  FViewFieldName = FMainKey
  FDataLength =rep('',ncount)
  FAccessToken = rep(FAccessToken,ncount)
  FTableKey =rep(FTableKey,ncount)
  FEntryKey = rep('',ncount)
  FTableNameAlias = rep(FTableNameAlias,ncount)
  maxId = model_getMaxId(conn=conn)
  FInterId = Findex + maxId






  res = data.frame(FInterId,FOwnerName,
                   FFormId,
                   FFormName,
                   FActionName,
                   FActionDesc,
                   FNodeName,
                   Findex,
                   Ftype,
                   FListCount,
                   FEntityName,
                   FMainKey,
                   FAuxKey,
                   FDefaultValue,
                   FDataType,
                   FValueType,
                   FKeyCaption,
                   FMustInput,
                   FIsShow,
                   FTableName,
                   FTableFieldName,
                   FViewName,
                   FViewFieldName,
                   FDataLength,
                   FAccessToken,
                   FTableKey,
                   FEntryKey,
                   FTableNameAlias,



                   stringsAsFactors = F)
  return(res)
}



#' 模型获取头部数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormName 表单名称
#' @param FActionDesc 操作名称
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_headPart()
model_headPart <- function(json_file='data-raw/material_save_cp.json',
                            conn = tsda::conn_rds('metadata'),
                            FFormName = '物料',
                            FActionDesc ='保存',
                            FOwnerName ='kingdee'){
info = model_getEntityList(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,Ftype = 'head',FOwnerName = FOwnerName)
ncount = nrow(info)
if(ncount>0){

  FFormId = info$FFormId[1]
  FActionName =  info$FActionName[1]
  FEntityName = info$FEntityName[1]
  Ftype = info$Ftype[1]
  FOwnerName = info$FOwnerName[1]
  FFormName = info$FFormName[1]
  FActionDesc = info$FActionDesc[1]
  FTableName = info$FTableName[1]
  FViewName = info$FViewName[1]
  FAccessToken = info$FAccessToken[1]
  FTableKey = info$FTableKey[1]
  FTableNameAlias = info$FTableNameAlias[1]

  res = model_head_data(json_file=json_file,
                                    conn = conn,
                                    FFormId =FFormId,
                                    FFormName = FFormName,
                                    FActionName =FActionName,
                                    FActionDesc =FActionDesc,
                                    FTableName = FTableName,
                                    FViewName = FViewName,
                                    FAccessToken = FAccessToken,
                                    FTableKey =FTableKey,
                                    FTableNameAlias =FTableNameAlias,
                                    FOwnerName =FOwnerName)

}else{
  res = NULL
}
return(res)

}





#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormId  表单
#' @param FActionName  动作
#' @param FOwnerName 所有者
#' @param entityName  entity name
#' @param FFormName  fornaeme
#' @param FActionDesc a
#' @param FTableName t
#' @param FViewName v
#' @param FAccessToken  t
#' @param FTableKey  t
#' @param FTableNameAlias a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataOne()
model_entry_dataOne <- function(json_file='data-raw/material_save_cp.json',
                            conn = tsda::conn_rds('metadata'),
                            entityName='FSubHeadEntity',
                            FFormId ='BD_MATERIAL',
                            FFormName = '物料',
                            FActionName ='Save',
                            FActionDesc ='保存',
                            FTableName = 'rds_bd_material',
                            FViewName = 'rds_vw_bd_material',
                            FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                            FTableKey ='FNumber',

                            FTableNameAlias ='t1',





                            FOwnerName ='kingdee'){
  print('bug1')
  model= model_getFromJson(json_file = json_file)
  #model = model[[entityName]]
  print('bug2')
  FMainKey = model_entry_MainKey(model=model,entityName = entityName,conn = conn,FFormId =FFormId,FActionName =FActionName,FOwnerName =FOwnerName)
  print('bug3')
  FDataType =model_entry_dataType(model = model,entityName = entityName,mainKey = FMainKey)
  print('bug4')
  FValueType = model_entry_valueType(model = model,entityName = entityName,mainKey = FMainKey)
  print('bug5')
  FAuxKey = model_entry_auxKey(model=model,entityName = entityName,mainKey =FMainKey )
  print('bug6')
  data_head_list = list(FMainKey,FDataType,FValueType,FAuxKey)
  print(data_head_list)
  data_head = data.frame(FMainKey,FDataType,FValueType,FAuxKey,stringsAsFactors = F)
  print('bug7')
  print(data_head)
  FDefaultValue = lapply(1:nrow(data_head), function(i){
    item = data_head[i,]

    FMainKey  =  item$FMainKey
    FDataType = item$FDataType
    FValueType = item$FValueType
    FAuxKey = item$FAuxKey
    if (FValueType =='complex'){
      value = model[[entityName]][[FMainKey]][[FAuxKey]]
    }else{
      value =  model[[entityName]][[FMainKey]]
    }
    return(value)
  })
  print('bug8')
  FDefaultValue = unlist(FDefaultValue)

  #处理数据，按表的顺序处理
  ncount = nrow(data_head)

  FOwnerName = rep(FOwnerName,ncount)

  FFormId = rep(FFormId,ncount)

  FFormName = rep(FFormName,ncount)

  FActionName =rep(FActionName,ncount)

  FActionDesc = rep(FActionDesc,ncount)

  FNodeName = rep('Model',ncount)

  Findex = 1:ncount

  Ftype = rep('entry',ncount)

  FListCount = rep(0,ncount)

  FEntityName = rep(entityName,ncount)

  FKeyCaption = rep('',ncount)

  FMustInput =rep(0,ncount)

  FIsShow = rep(0,ncount)

  #进行处理
  FTableName = rep(FTableName,ncount)

  FTableFieldName =paste0(FMainKey,'_',FEntityName)

  FViewName = rep(FViewName,ncount)

  FViewFieldName = paste0(FMainKey,'_',FEntityName)

  FDataLength =rep('',ncount)

  FAccessToken = rep(FAccessToken,ncount)

  FTableKey =rep(FTableKey,ncount)

  FEntryKey = rep('',ncount)

  FTableNameAlias = rep(FTableNameAlias,ncount)

  maxId = model_getMaxId(conn=conn)

  FInterId = Findex + maxId




  print('bug10')




  res = data.frame(FInterId,FOwnerName,
                   FFormId,
                   FFormName,
                   FActionName,
                   FActionDesc,
                   FNodeName,
                   Findex,
                   Ftype,
                   FListCount,
                   FEntityName,
                   FMainKey,
                   FAuxKey,
                   FDefaultValue,
                   FDataType,
                   FValueType,
                   FKeyCaption,
                   FMustInput,
                   FIsShow,
                   FTableName,
                   FTableFieldName,
                   FViewName,
                   FViewFieldName,
                   FDataLength,
                   FAccessToken,
                   FTableKey,
                   FEntryKey,
                   FTableNameAlias,



                   stringsAsFactors = F)
  return(res)
}



#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormId  表单
#' @param FActionName  动作
#' @param FOwnerName 所有者
#' @param FFormName  fornaeme
#' @param FActionDesc a
#' @param FTableName t
#' @param FViewName v
#' @param FAccessToken  t
#' @param FTableKey  t
#' @param FTableNameAlias a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataAll()
model_entry_dataAll <- function(json_file='data-raw/material_save_cp.json',
                                conn = tsda::conn_rds('metadata'),

                                FFormId ='BD_MATERIAL',
                                FFormName = '物料',
                                FActionName ='Save',
                                FActionDesc ='保存',
                                FTableName = 'rds_bd_material',
                                FViewName = 'rds_vw_bd_material',
                                FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                                FTableKey ='FNumber',

                                FTableNameAlias ='t1',





                                FOwnerName ='kingdee'){
  entryList = model_getEntityName_entry(conn = conn,FFormId = FFormId,FActionName = FActionName,FOwnerName = FOwnerName)
  res_list = lapply(entryList, function(entityName){
    print(entityName)
    model_entry_dataOne(json_file=json_file,
                                    conn = conn,
                                    entityName=entityName,
                                    FFormId =FFormId,
                                    FFormName = FFormName,
                                    FActionName =FActionName,
                                    FActionDesc = FActionDesc,
                                    FTableName =  FTableName,
                                    FViewName = FViewName,
                                    FAccessToken = FAccessToken,
                                    FTableKey =FTableKey,

                                    FTableNameAlias =FTableNameAlias,





                                    FOwnerName =FOwnerName)

  })
  res = do.call('rbind',res_list)

  return(res)
}



#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FOwnerName 所有者
#' @param FFormName  fornaeme
#' @param FActionDesc a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entryPart()
model_entryPart <- function(json_file='data-raw/material_save_cp.json',
                                conn = tsda::conn_rds('metadata'),
                                FFormName = '物料',
                                FActionDesc ='保存',
                                FOwnerName ='kingdee'){
  info = model_getEntityList(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,Ftype = 'entry',FOwnerName = FOwnerName)
  ncount = nrow(info)
  if(ncount>0){
    res_list = lapply(1:ncount, function(i){

      FFormId = info$FFormId[i]
      FActionName =  info$FActionName[i]
      FEntityName = info$FEntityName[i]
      Ftype = info$Ftype[i]
      FOwnerName = info$FOwnerName[i]
      FFormName = info$FFormName[i]
      FActionDesc = info$FActionDesc[i]
      FTableName = info$FTableName[i]
      FViewName = info$FViewName[i]
      FAccessToken = info$FAccessToken[i]
      FTableKey = info$FTableKey[i]
      FTableNameAlias = info$FTableNameAlias[i]
      model_entry_dataOne(json_file=json_file,
                          conn = conn,
                          entityName=FEntityName,
                          FFormId =FFormId,
                          FFormName = FFormName,
                          FActionName =FActionName,
                          FActionDesc = FActionDesc,
                          FTableName =  FTableName,
                          FViewName = FViewName,
                          FAccessToken = FAccessToken,
                          FTableKey =FTableKey,

                          FTableNameAlias =FTableNameAlias,





                          FOwnerName =FOwnerName)

    })


    res = do.call('rbind',res_list)


  }else{
    res = NULL
  }


  return(res)
}



#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormId  表单
#' @param FActionName  动作
#' @param FOwnerName 所有者
#' @param entityName  entity name
#' @param FFormName  fornaeme
#' @param FActionDesc a
#' @param FTableName t
#' @param FViewName v
#' @param FAccessToken  t
#' @param FTableKey  t
#' @param FTableNameAlias a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataOne()
model_entryList_dataOne <- function(json_file='data-raw/material_save_cp.json',
                                conn = tsda::conn_rds('metadata'),
                                entityName='FSubHeadEntity',
                                FFormId ='BD_MATERIAL',
                                FFormName = '物料',
                                FActionName ='Save',
                                FActionDesc ='保存',
                                FTableName = 'rds_bd_material',
                                FViewName = 'rds_vw_bd_material',
                                FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                                FTableKey ='FNumber',

                                FTableNameAlias ='t1',





                                FOwnerName ='kingdee'){
  print('bug1')
  model= model_getFromJson(json_file = json_file)
  #model = model[[entityName]]
  print('bug2')
  FMainKey = model_entryList_MainKey(model=model,entityName = entityName,conn = conn,FFormId =FFormId,FActionName =FActionName,FOwnerName =FOwnerName)
  print('bug3')
  FDataType =model_entryList_dataType(model = model,entityName = entityName,mainKey = FMainKey)
  print('bug4')
  FValueType = model_entryList_valueType(model = model,entityName = entityName,mainKey = FMainKey)
  print('bug5')
  FAuxKey = model_entryList_auxKey(model=model,entityName = entityName,mainKey =FMainKey )
  print('bug6')
  data_head_list = list(FMainKey,FDataType,FValueType,FAuxKey)
  print(data_head_list)
  data_head = data.frame(FMainKey,FDataType,FValueType,FAuxKey,stringsAsFactors = F)
  print('bug7')
  print(data_head)
  FDefaultValue = lapply(1:nrow(data_head), function(i){
    item = data_head[i,]

    FMainKey  =  item$FMainKey
    FDataType = item$FDataType
    FValueType = item$FValueType
    FAuxKey = item$FAuxKey
    if (FValueType =='complex'){
      value = model[[entityName]][[1]][[FMainKey]][[FAuxKey]]
    }else{
      value =  model[[entityName]][[1]][[FMainKey]]
    }
    return(value)
  })
  print('bug8')
  FDefaultValue = unlist(FDefaultValue)
  #处理数据，按表的顺序处理
  ncount = nrow(data_head)
  FOwnerName = rep(FOwnerName,ncount)
  FFormId = rep(FFormId,ncount)
  FFormName = rep(FFormName,ncount)
  FActionName =rep(FActionName,ncount)
  FActionDesc = rep(FActionDesc,ncount)
  FNodeName = rep('Model',ncount)
  Findex = 1:ncount
  Ftype = rep('entryList',ncount)
  FListCount = rep(1,ncount)
  FEntityName = rep(entityName,ncount)
  FKeyCaption = rep('',ncount)
  FMustInput =rep(0,ncount)
  FIsShow = rep(0,ncount)
  #进行处理
  FTableName = paste0(rep(FTableName,ncount),FEntityName)
  FTableFieldName =paste0(FMainKey,'_',FEntityName)
  FViewName = rep(FViewName,ncount)
  FViewFieldName = paste0(FMainKey,'_',FEntityName)
  FDataLength =rep('',ncount)
  FAccessToken = rep(FAccessToken,ncount)
  FTableKey =rep(FTableKey,ncount)
  FEntryKey = paste0('FListCount_',FEntityName)
  FTableNameAlias = rep(FTableNameAlias,ncount)
  maxId = model_getMaxId(conn=conn)
  FInterId = Findex + maxId


  print('bug9')
  res_list = list(FInterId,FOwnerName,
                        FFormId,
                        FFormName,
                        FActionName,
                        FActionDesc,
                        FNodeName,
                        Findex,
                        Ftype,
                        FListCount,
                        FEntityName,
                        FMainKey,
                        FAuxKey,
                        FDefaultValue,
                        FDataType,
                        FValueType,
                        FKeyCaption,
                        FMustInput,
                        FIsShow,
                        FTableName,
                        FTableFieldName,
                        FViewName,
                        FViewFieldName,
                        FDataLength,
                        FAccessToken,
                        FTableKey,
                        FEntryKey,
                        FTableNameAlias)
  str(res_list)


  res = data.frame(FInterId,FOwnerName,
                   FFormId,
                   FFormName,
                   FActionName,
                   FActionDesc,
                   FNodeName,
                   Findex,
                   Ftype,
                   FListCount,
                   FEntityName,
                   FMainKey,
                   FAuxKey,
                   FDefaultValue,
                   FDataType,
                   FValueType,
                   FKeyCaption,
                   FMustInput,
                   FIsShow,
                   FTableName,
                   FTableFieldName,
                   FViewName,
                   FViewFieldName,
                   FDataLength,
                   FAccessToken,
                   FTableKey,
                   FEntryKey,
                   FTableNameAlias,



                   stringsAsFactors = F)
  return(res)
}



#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FFormId  表单
#' @param FActionName  动作
#' @param FOwnerName 所有者
#' @param FFormName  fornaeme
#' @param FActionDesc a
#' @param FTableName t
#' @param FViewName v
#' @param FAccessToken  t
#' @param FTableKey  t
#' @param FTableNameAlias a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataAll()
model_entryList_dataAll <- function(json_file='data-raw/material_save_cp.json',
                                conn = tsda::conn_rds('metadata'),

                                FFormId ='BD_MATERIAL',
                                FFormName = '物料',
                                FActionName ='Save',
                                FActionDesc ='保存',
                                FTableName = 'rds_bd_material',
                                FViewName = 'rds_vw_bd_material',
                                FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                                FTableKey ='FNumber',

                                FTableNameAlias ='t1',





                                FOwnerName ='kingdee'){
  entryList = model_getEntityName_entryList(conn = conn,FFormId = FFormId,FActionName = FActionName,FOwnerName = FOwnerName)
  res_list = lapply(entryList, function(entityName){
    print(entityName)
    model_entryList_dataOne(json_file=json_file,
                        conn = conn,
                        entityName=entityName,
                        FFormId =FFormId,
                        FFormName = FFormName,
                        FActionName =FActionName,
                        FActionDesc = FActionDesc,
                        FTableName =  FTableName,
                        FViewName = FViewName,
                        FAccessToken = FAccessToken,
                        FTableKey =FTableKey,

                        FTableNameAlias =FTableNameAlias,





                        FOwnerName =FOwnerName)

  })
  res = do.call('rbind',res_list)

  return(res)
}



#' 读取模型元数据
#'
#' @param json_file json file
#' @param conn 连接
#' @param FOwnerName 所有者
#' @param FFormName  fornaeme
#' @param FActionDesc a
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_entry_dataAll()
model_entryListPart <- function(json_file='data-raw/material_save_cp.json',
                                    conn = tsda::conn_rds('metadata'),
                                    FFormName = '物料',
                                    FActionDesc ='保存',
                                    FOwnerName ='kingdee'){
  info = model_getEntityList(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,Ftype = 'entryList',FOwnerName = FOwnerName)
  ncount = nrow(info)
  if(ncount>0){
    res_list = lapply(1:ncount, function(i){

      FFormId = info$FFormId[i]
      FActionName =  info$FActionName[i]
      FEntityName = info$FEntityName[i]
      Ftype = info$Ftype[i]
      FOwnerName = info$FOwnerName[i]
      FFormName = info$FFormName[i]
      FActionDesc = info$FActionDesc[i]
      FTableName = info$FTableName[i]
      FViewName = info$FViewName[i]
      FAccessToken = info$FAccessToken[i]
      FTableKey = info$FTableKey[i]
      FTableNameAlias = info$FTableNameAlias[i]
      model_entryList_dataOne(json_file=json_file,
                          conn = conn,
                          entityName=FEntityName,
                          FFormId =FFormId,
                          FFormName = FFormName,
                          FActionName =FActionName,
                          FActionDesc = FActionDesc,
                          FTableName =  FTableName,
                          FViewName = FViewName,
                          FAccessToken = FAccessToken,
                          FTableKey =FTableKey,

                          FTableNameAlias =FTableNameAlias,





                          FOwnerName =FOwnerName)

    })


    res = do.call('rbind',res_list)


  }else{
    res = NULL
  }

   return(res)
}

#' 返回所有数据
#'
#' @param json_file json file
#' @param conn   连接
#' @param FFormId 表单
#' @param FFormName 表名
#' @param FActionName 操作
#' @param FActionDesc 描述
#' @param FTableName 表名
#' @param FViewName 视图表
#' @param FAccessToken 图表
#' @param FTableKey 键值
#' @param FTableNameAlias 别名
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_dataAll()
model_dataAll <-function(json_file='data-raw/material_save_cp.json',
                         conn = tsda::conn_rds('metadata'),
                         FFormId ='BD_MATERIAL',
                         FFormName = '物料',
                         FActionName ='Save',
                         FActionDesc ='保存',
                         FTableName = 'rds_bd_material',
                         FViewName = 'rds_vw_bd_material',
                         FAccessToken = '970C9E78-F276-4073-95AB-790A43BB38AB',
                         FTableKey ='FNumber',
                         FTableNameAlias ='t1',
                         FOwnerName ='kingdee'){
  data_head <- model_head_data(json_file=json_file,
                              conn = conn,
                              FFormId =FFormId,
                              FFormName = FFormName,
                              FActionName =FActionName,
                              FActionDesc =FActionDesc,
                              FTableName = FTableName,
                              FViewName = FViewName,
                              FAccessToken = FAccessToken,
                              FTableKey =FTableKey,
                              FTableNameAlias =FTableNameAlias,
                              FOwnerName =FOwnerName)
  data_entry <- model_entry_dataAll(json_file=json_file,
                                  conn = conn,

                                  FFormId =FFormId,
                                  FFormName = FFormName,
                                  FActionName =FActionName,
                                  FActionDesc =FActionDesc,
                                  FTableName = FTableName,
                                  FViewName = FViewName,
                                  FAccessToken = FAccessToken,
                                  FTableKey =FTableKey,
                                  FTableNameAlias =FTableNameAlias,
                                  FOwnerName =FOwnerName)
  data_entryList <- model_entryList_dataAll(json_file=json_file,
                                      conn = conn,

                                      FFormId =FFormId,
                                      FFormName = FFormName,
                                      FActionName =FActionName,
                                      FActionDesc =FActionDesc,
                                      FTableName = FTableName,
                                      FViewName = FViewName,
                                      FAccessToken = FAccessToken,
                                      FTableKey =FTableKey,

                                      FTableNameAlias =FTableNameAlias,
                                      FOwnerName =FOwnerName)

  res = rbind(data_head,data_entry,data_entryList)
  maxId = model_getMaxId(conn = conn)
  res$FInterId = maxId +1:nrow(res)
  res$Findex = 1:nrow(res)
  res[res$FDefaultValue =='0','FDataType'] = 'int'
  res[res$FDefaultValue =='100','FDataType'] = 'int'
  res[res$FDefaultValue =='','FDataType'] = 'nvarchar'
  res[res$FDataType == 'int','FDataLength'] = ''
  res[res$FDataType == 'nvarchar','FDataLength'] = '(200)'

  return(res)


}


#' 模型删除数据
#'
#' @param conn 连接
#' @param FFormName 表单名称
#' @param FActionDesc 操作名称
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_delete()
model_delete <-function(conn = tsda::conn_rds('metadata'),
                        FFormName = '物料',
                        FActionDesc ='保存',
                        FOwnerName ='kingdee'){

  sql <- paste0("delete  from  t_api_erp_kdc
where FOwnerName ='",FOwnerName,"' and FFormName = '",FFormName,"' and FActionDesc ='",FActionDesc,"'")
  tsda::sql_update(conn,sql)
}


#' 模型更新数据更新标题，用于设置对照字典及必录项
#'
#' @param conn 连接
#' @param FFormName 表单名称
#' @param FActionDesc 操作名称
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_updateCaption()
model_updateCaption <-function(conn = tsda::conn_rds('metadata'),
                        FFormName = '物料',
                        FActionDesc ='保存',
                        FOwnerName ='kingdee'){

  sql1 <- paste0("  update a set a.FKeyCaption =   b.FKeyCaption,a.FMustInput = b.FMustInput
  FROM [metadata].[dbo].[t_api_erp_kdc] a
  inner  join t_api_kdc_field b
  on a.FOwnerName =b.FOwnerName and a.FMainKey = b.FMainKey
  and a.FFormId = b.FFormId and a.FActionName = b.FActionName
  where  a.FFormName ='",FFormName,"' and a.FActionDesc ='",FActionDesc,"'  and a.FOwnerName ='",FOwnerName,"'
  and a.FMainKey <> 'FEntryId'")
  tsda::sql_update(conn,sql1)
  sql2 <- paste0("  update a set a.FKeyCaption =  '实体主键',a.FMustInput = 0
  FROM [metadata].[dbo].[t_api_erp_kdc] a
  where  a.FFormName ='",FFormName,"' and a.FActionDesc ='",FActionDesc,"'  and a.FOwnerName ='",FOwnerName,"'
  and a.FMainKey =  'FEntryId'")
  tsda::sql_update(conn,sql2)



}


#' 更新SQL数据类型,进行更加精细化管理
#'
#' @param conn 连接
#' @param FFormName 表单名称
#' @param FActionDesc 操作名称
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_updateSqlDataType()
model_updateSqlDataType <-function(conn = tsda::conn_rds('metadata'),
                               FFormName = '物料',
                               FActionDesc ='保存',
                               FOwnerName ='kingdee'){

  sql <- paste0("
update a set a.FSqlDataType = b.FSqlDataType    from t_api_erp_kdc a
inner join vw_api_kdc_field b
on a.FOwnerName = b.FOwnerName and a.FFormName=  b.FFormName and a.FActionDesc = b.FActionDesc and a.FMainKey = b.FMainKey
  where  a.FFormName ='",FFormName,"' and a.FActionDesc ='",FActionDesc,"'  and a.FOwnerName ='",FOwnerName,"'")
  tsda::sql_update(conn,sql)




}







#' 返回所有数据
#'
#' @param json_file json file
#' @param conn   连接
#' @param FFormName 表名
#' @param FActionDesc 描述
#' @param FOwnerName 所有者
#'
#' @return 返回值
#' @export
#'
#' @examples
#' model_dataAll()
model_dataAllPart <-function(json_file='data-raw/material_save_cp.json',
                         conn = tsda::conn_rds('metadata'),
                         FFormName = '物料',
                         FActionDesc ='保存',
                         FOwnerName ='kingdee'){
  data_head <- model_headPart(json_file = json_file,conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)
  data_entry <- model_entryPart(json_file = json_file,conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)
  data_entryList <- model_entryListPart(json_file = json_file,conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)

  res = rbind(data_head,data_entry,data_entryList)
  maxId = model_getMaxId(conn = conn)
  res$FInterId = maxId +1:nrow(res)
  res$Findex = 1:nrow(res)
  res[res$FDefaultValue =='0','FDataType'] = 'int'
  res[res$FDefaultValue =='100','FDataType'] = 'int'
  res[res$FDefaultValue =='','FDataType'] = 'nvarchar'
  res[res$FDataType == 'int','FDataLength'] = ''
  res[res$FDataType == 'nvarchar','FDataLength'] = '(200)'
  #默认显示,可以调整
  res$FIsShow =1
  #删除历史数据
  model_delete(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)
  #上传相关数据
  tsda::db_writeTable(conn=conn,table_name = 't_api_erp_kdc',r_object = res,append = TRUE)
  #更新字面说明及必录项部分，这个单独的表，用于匹配
  model_updateCaption(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)
  #更新字段类型,用于SQL的数据存储
  model_updateSqlDataType(conn = conn,FFormName = FFormName,FActionDesc = FActionDesc,FOwnerName = FOwnerName)

  return(res)


}





