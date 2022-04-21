  library(metadatapkg)

  # step 1: read the data from json

  model = model_getFromJson(json_file = 'data-raw/customer_save_cp.json')

  #step 2:create the entity table into dbms.
  # t_api_kdc_entity
   model_entity_read(file_name = 'data-raw/kdc_entityTable.xlsx')
  #step3: create the data
   mydata = model_dataAllPart(json_file = 'data-raw/customer_save_cp.json',FOwnerName = 'CP',FFormName = '客户',FActionDesc = '保存')











  mydata = model_dataAllPart(FOwnerName = 'CP')

  View(mydata)


  # mydata = model_dataAll()
  # View(mydata)
  #
  # unique(mydata$FEntityName)
  #
  #
  #
  # model_headPart()

#   data_head = model_head_data()
#   View(data_head)
#
#
#
#
#   entryD1 =model_entry_dataAll()
#   View(entryD1)
#
#
#   openxlsx::write.xlsx(entryD1,'entry.xlsx')
#
#   mydata2 = model_entry_dataOne()
#   nrow(mydata2)
#   openxlsx::write.xlsx(mydata2,'entry2.xlsx',overwrite = T)
#
# T
#
#   entryD2 =model_entryList_dataAll()
#   View(entryD2)
#
#
#
# data12 = rbind(data_head,entryD1)
# View(data12)
# data13  = rbind(data12,entryD2)
# View(data13)
#
#


