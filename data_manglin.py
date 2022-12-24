import basedosdados as bd

df = bd.read_table(dataset_id='br_inep_censo_escolar',
                   table_id='matricula',
                   billing_project_id="uftm-biostat-database")