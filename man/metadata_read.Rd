% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file.R
\name{metadata_read}
\alias{metadata_read}
\title{读取数据进入源数据表}
\usage{
metadata_read(
  conn = tsda::conn_rds("metadata"),
  file_name = "data-raw/metadata.xlsx"
)
}
\arguments{
\item{conn}{连接}

\item{file_name}{文件名}
}
\value{
返回值
}
\description{
读取数据进入源数据表
}
\examples{
metadata_read()
}
