  library(metadatapkg)

  mydata = model_dataAllPart(FOwnerName = 'CP',FFormName = '销售订单',json_file = 'data-raw/sales/saleOrder_save_cp.json')

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


