library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("uftm-biostat-database")

# Para carregar o dado direto no R
query <- bdplyr("br_inep_censo_escolar.turma")
df <- bd_collect(query)
read_sql(query = 'SELECT * FROM `br_inep_censo_escolar.turma` ')
download(query = 'SELECT * FROM `basedosdados.br_ana_atlas_esgotos.municipios` ', path = '/funciona.csv')
read_sql(query = 'SELECT * FROM `basedosdados.br_ana_atlas_esgotos.municipios` ', billing_project_id = "uftm-biostat-database")
