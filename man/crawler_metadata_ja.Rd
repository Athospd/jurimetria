\name{crawler_metadata_ja}
\alias{crawler_metadata_ja}
\title{Retorna data.frames com metadados do Justiça Aberta.}
\usage{
crawler_metadata_ja(tipo, ufs = NULL, cod_muni = NULL, cod_vara = NULL,
  instancia = 1, justica = 1)
}
\description{
Essa função retorna metadados úteis para a pesquisa
jurimétrica através do sistema Justiça Aberta. Dependendo
do tipo ("muni", "vara", "prod"), retorna um nível
diferente de metadados. Os parâmetros e cod_muni, cod_vara
ajudam a determinar quais municípios, produtividades ou
varas serão retornados. Se forem nulos (padrão), a função
baixará da internet todos os metadados. Tome cuidado pois
essa função pode demorar um tempo considerável para rodar,
pois acessa muitas páginas da internet (27 requisições para
baixar os municípios; ~5591 requisições para baixar as
varas estaduais, e ~10000 requisições para baixar as
produtividades). Para obter as produtividades a partir de
um dado código de produtividade, considere a função
crawler_prod_ja().
}

