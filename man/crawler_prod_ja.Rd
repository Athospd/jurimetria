\name{crawler_prod_ja}
\alias{crawler_prod_ja}
\title{Baixa PDF do Justica Aberta e transforma em data.frame}
\usage{
crawler_prod_ja(cod_prod, cod_vara, tipo = "vara", d_pdf, d_html,
  pdf2txt = TRUE)
}
\description{
Essa funcao so funciona no linux, e somente se vc instalar
o pdfminer (sudo pip install pdfminer==20110515). Essa
funcao recebe o codigo da produtividade, o codigo da vara
(que e necessario somente para completar a request feita
para o servidor do Justica Aberta), o tipo (se e
vara:"vara" ou magistrado:"mag") e dois diretorios (um para
guardar o pdf, e outro para guardar o arquivo html gerado
pelo pdf2txt). Essa funcao e extremamente lenta (demora ~1s
por produtividade, e geralmente queremos muitas
produtividades).
}

