% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json.R
\name{model_updateSqlDataType}
\alias{model_updateSqlDataType}
\title{更新SQL数据类型,进行更加精细化管理}
\usage{
model_updateSqlDataType(
  conn = tsda::conn_rds("metadata"),
  FFormName = "物料",
  FActionDesc = "保存",
  FOwnerName = "kingdee"
)
}
\arguments{
\item{conn}{连接}

\item{FFormName}{表单名称}

\item{FActionDesc}{操作名称}

\item{FOwnerName}{所有者}
}
\value{
返回值
}
\description{
更新SQL数据类型,进行更加精细化管理
}
\examples{
model_updateSqlDataType()
}
