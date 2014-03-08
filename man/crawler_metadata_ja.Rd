\name{crawler_metadata_ja}
\alias{crawler_metadata_ja}
\title{Retorna data.frames com metadados do Justica Aberta.}
\usage{
crawler_metadata_ja(tipo, ufs = NULL, cod_muni = NULL, cod_vara = NULL,
  instancia = 1, justica = 1)
}
\description{
Essa funcao retorna metadados uteis para a pesquisa
jurimetrica atraves do sistema Justica Aberta. Dependendo
do tipo ("muni", "vara", "prod"), retorna um nivel
diferente de metadados. Os parametros ufs, cod_muni e
cod_vara ajudam a determinar quais municípios,
produtividades ou varas serao retornados. Se forem nulos
(padrao), a funcao baixara da internet todos os metadados.
Tome cuidado pois essa funcao pode demorar um tempo
consideravel para rodar, pois acessa muitas paginas da
internet (27 requisicoes para baixar os municipios; ~5591
requisicoes para baixar as varas estaduais, e ~10000
requisicoes para baixar as produtividades). Para obter as
produtividades a partir de um dado codigo de produtividade,
considere a funcao crawler_prod_ja().
}

