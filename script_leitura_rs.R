library(tidyverse); library(vroom); library(genderBR); library(readxl); library(lubridate)
library(dygraphs)

setwd("~/LAPEI/Projeto SEBRAE/Atualização pesquisas/empreendedorismo RFB/estabelecimentos")

tab_cnae <- read_delim("https://raw.githubusercontent.com/danielppagotto/Sebrae-go-mulheres/main/bases%20de%20apoio/cnae.csv",
                       ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "windows-1252"))

municipios <- read_delim("https://raw.githubusercontent.com/danielppagotto/Sebrae-go-mulheres/main/bases%20de%20apoio/municipios_serpro.csv",
                         ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "windows-1252"))


natureza_juridica <- read_delim("https://raw.githubusercontent.com/danielppagotto/Sebrae-go-mulheres/main/bases%20de%20apoio/natureza_juridica.csv",
                                ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "windows-1252")) 


# Subindo bases depois de tratatadas -------------------------------------------

setwd("~/LAPEI/Projeto SEBRAE/Atualização pesquisas/empreendedorismo RFB/empresas")

ativas_base <- vroom("~/LAPEI/Projeto SEBRAE/Atualização pesquisas/empreendedorismo RFB/empresas/empresas_ativas.csv")

ativas <- ativas_base %>% 
  rename(razao_social = X2, natureza_juridica = X3, 
         qualifica_responsavel = X4, capital_social = X5,
         porte_empresa = X6, ente_federativo = X7) %>% 
         left_join(natureza_juridica, by = c("natureza_juridica" = "cod_subclass_natureza_juridica")) %>% 
         filter(cod_natureza_juridica > 1 & cod_natureza_juridica < 5)


# tratando ME separadamente -----------------------------------------------

microempresas <- ativas %>% 
        filter(natureza_juridica == "2135") 

me <- microempresas %>% 
  mutate(genero = get_gender(razao_social), .after = razao_social) %>% 
  filter(genero != "NA") %>% 
  left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
  mutate(data = parse_date_time(data_inicio_atividade, orders = "ymd"),
         ano = year(data)) 

municipios_me <- me %>% 
  rename(nome_municipio = Municipio) %>% 
  group_by(municipio, nome_municipio , genero) %>% 
  count() %>% 
  mutate(natureza = "me")

total_anos <- me %>% 
  group_by(ano, genero) %>% 
  count() %>% 
  spread(genero,n) %>% 
  mutate(total = Female + Male,
         prop_female = Female/total,
         prop_male = Male/total) 

total_anos %>% 
  filter(ano > 1965) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = prop_female, col = "darkred")) +
  geom_line(aes(x = ano, y = prop_male, col = "darkblue")) + 
  theme_minimal()
  

writexl::write_xlsx(total_anos, "total_anos.xlsx")

total_anos %>% 
  filter(ano > 2015) %>% 
  ggplot(aes(x = data, y = n, col = genero)) + geom_line() +
  theme_minimal() + 
  facet_wrap(~genero)


nome_diferentes <- me %>%  
      filter(is.na(genero)) 

# tratando não ME --------------------------------------------------------------

nao_me1 <- ativas %>% 
  filter(natureza_juridica != "2135") 

nao_me <- nao_me1 %>% 
        left_join(socios, by = c("cnpj_basico"="cnpj_basico")) 

nao_me_tratado <- nao_me %>% 
            mutate(genero = get_gender(nome_socio), .after = nome_socio) %>% 
            filter(data_inicio_atividade == entrada_sociedade) %>% 
            select(-`...1`) %>% 
            mutate(data = parse_date_time(data_inicio_atividade, orders = "ymd"),
            ano = year(data))


nao_me_tratado_s_na <- nao_me_tratado %>% 
                      filter(genero != "NA")


municipios_nao_me <- nao_me_tratado_s_na %>% 
                      left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
                      rename(nome_municipio = Municipio) %>% 
                      group_by(municipio,nome_municipio,genero) %>% 
                      count() %>% 
                      mutate(natureza = "não me")

total <- rbind(municipios_me, municipios_nao_me)

total <- total %>% 
          group_by(nome_municipio, genero) %>% 
          summarise(total = sum(n))

# total_por_sexo_spread <- nao_me_tratado_s_na %>% 
#                       group_by(ano, cnpj_basico, genero) %>% 
#                       count() %>% 
#                       spread(genero,n)
# 
# total_por_sexo_spread[is.na(total_por_sexo_spread)] <- 0
# 
# nao_me_generos <- nao_me1 %>% 
#   filter(natureza_juridica != "2135") %>% 
#   left_join(total_por_sexo_spread, by = "cnpj_basico") %>% 
#   filter(ano != "NA")
# 
# nao_me_generos %>% 
#   gather(key = "genero", value = "total", Female, Male) %>% 
#   left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
#   rename(nome_municipio = Municipio) %>% 
#   group_by(municipio, nome_municipio, genero) %>% 
#   summarise(total = sum(total)) %>% 
#   mutate(natureza = "nao me")
# 
# 
# 
# write.csv(nao_me, "ativas_nao_me.csv")
