# Cálculo de Taxa de Ocupação - Dados Datasus
# Author: Marcelo Santos
# Date: 01/05/2016
#
library(read.dbc)
library(dplyr)
library(data.table)
#
#
data_folder <- paste(getwd(), '/datasets/', sep='')
#
# Data inicial do periodo de apuracao (MUITA ATENCAO)
dt_inicio_processo <- as.POSIXct(strptime('2016-05-01',format='%Y-%m-%d'))
#
# lendo datasets
dfcnes  <- read.csv(paste(data_folder,"cnes_descritiva.csv",sep=''),sep=',',header=TRUE)
dfcnes$CNES <- substring(as.character(as.integer(dfcnes$CNES)+10000000),2,8)
#
dftipoleito <- read.csv(paste(data_folder,'dftipoleito.csv',sep=''),sep=',',encoding = "latin1",header=TRUE)
dftipoleito$codigotipoleito <- substring(as.character(as.integer(dftipoleito$codigotipoleito)+100),2,3)
#
dfsih <- read.dbc(paste(data_folder,"RDMG1501.dbc",sep='')) 
dfsih<-dfsih[,c('N_AIH','ANO_CMPT','MES_CMPT','CNES','ESPEC','PROC_REA','COMPLEX','SEXO','RACA_COR','NASC','MORTE','IDADE','CEP','MUNIC_RES','DIAG_PRINC','DT_INTER','DT_SAIDA','SEQUENCIA','REMESSA','QT_DIARIAS','VAL_SH','VAL_SP','VAL_TOT','VAL_UTI','DIAS_PERM')]
dfsih$N_AIH      <- as.factor(dfsih$N_AIH) 
dfsih$DT_INTER   <- as.POSIXct(strptime(dfsih$DT_INTER,format='%Y%m%d')) 
dfsih$DT_SAIDA   <- as.POSIXct(strptime(dfsih$DT_SAIDA,format='%Y%m%d')) 
dfsih$CNES       <- as.character(dfsih$CNES)
dfsih$paciente   <- paste(dfsih$SEXO,dfsih$NASC,dfsih$CEP,dfsih$RACA_COR,dfsih$MUNIC_RES,sep='')
dfsih$ESPEC <- as.character(dfsih$ESPEC)
#
dfsih<-group_by(dfsih,N_AIH,ANO_CMPT,MES_CMPT,CNES,ESPEC,PROC_REA,COMPLEX,SEXO,RACA_COR,NASC,CEP,MUNIC_RES,DIAG_PRINC)%>%
       summarize( MORTE=max(MORTE), IDADE=max(IDADE), DT_INTER=min(DT_INTER), DT_SAIDA=max(DT_SAIDA), QT_DIARIAS=sum(QT_DIARIAS), VAL_SH=sum(VAL_SH), VAL_SP=sum(VAL_SP), VAL_TOT=sum(VAL_TOT), VAL_UTI=sum(VAL_UTI), DIAS_PERM=sum(DIAS_PERM))
#
# Ajustando todas as datas de internação anteriores ao 
# periodo de apuração para dt_inicio_processo (MUITA ATENÇÃO)
#
dfsih[dfsih$DT_INTER<dt_inicio_processo,'DT_INTER'] <- dt_inicio_processo
#
# Filtrando apenas registros com data de saida maior que dt_inicio_processo
#
dfsih<-filter(dfsih,DT_SAIDA>=dt_inicio_processo)
#
# Cálculo dias permanência (ajustado para o período em análise - dt_inicio_processo)
#
dfsih$dias_perm_new <- as.integer(round((dfsih$DT_SAIDA-dfsih$DT_INTER)/(60*60*24)))
dfsih$faixa_etaria <- as.factor(cut(as.integer(dfsih$IDADE), c(0, 1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 120)))
dfsih[dfsih$IDADE==0,'faixa_etaria']<-'(0,1]'
#
# relacionando com tipos de leitos
dfsih <- left_join(dfsih,dftipoleito,by=c('ESPEC'='codigotipoleito'))
# relacionando com dados do CNES
dfsih <- left_join(dfsih,dfcnes,by=c('CNES'='CNES'))
dfsih <- subset(dfsih,!is.na(Porte))
# Observem que alguns códigos do CNES não foram encontrados... 
# Por esta razão estou excluindo os NAs... 
# Favor revisar o código de geração de dados do CNES.
#
##############################################################
# Geração de KPIs
#
# média de tempo de permanência
kpi1<-group_by(dfsih,Porte)%>%summarize(kpi_tempo_permanencia=round(mean(dias_perm_new),digits=2))
# valor médio por AIH (REVER CÁLCULO DE AIH)
kpi2<-group_by(dfsih,Porte)%>%summarize(kpi_valor_medio_aih=mean(VAL_TOT)) 
# taxa de mortalidade
kpi3<-group_by(dfsih,Porte,ANO_CMPT,MES_CMPT)%>%summarize(kpi_obitos=round(sum(MORTE)/n(),digits=3))
# profissionais por leito (cnes)
kpi4<-group_by(dfcnes,Porte)%>%summarize(kpi_profissionais_leito=round(sum(Numero_Profissionais)/sum(Quantidade_leitos),digits=3))
# média de leitos
kpi5<-group_by(dfcnes,Porte)%>%summarize(kpi_media_leitos=round(mean(Quantidade_leitos),digits=3))
# Proporção de leitos complementares
kpi6<-filter(dfcnes,Quantidade_leitos>0)%>%group_by(Porte)%>%summarize(kpi_prop_leitos_comp=round(mean(Leito_Complementar/Quantidade_leitos),digits=3))
# 
# Calculando leitos ocupados
#
vdtfull<-data.frame()
dfocup <- select(dfsih, CNES, DT_INTER, DT_SAIDA, dias_perm_new)%>%filter(dias_perm_new>0)%>%arrange(CNES)

f3<-function(a,b,c){
  b<-as.POSIXct(strptime(b,format='%Y-%m-%d',tz = "")) 
  c<-as.integer(c)
  vdt<-seq(b,by='DSTday',length.out = c+1)
  iii<-nrow(vdtfull)
  vdtfull[(iii+1):(iii+1+c),1]<-a
  vdtfull[(iii+1):(iii+1+c),2]<-vdt
  return(vdtfull)}

dfocup<-apply(dfocup[,1:4], 1, function(x) f3(x[1],x[2],x[4]))
dfocup<-rbindlist(dfocup)
#
dfocup<-group_by(dfocup,V1,V2)%>%summarize(c=n())
names(dfocup)<-c('cnes','data','leitos_ocupados')
#
dfocup<-left_join(dfocup,dfcnes,by=c('cnes'='CNES'))
dfocup<-filter(dfocup,Quantidade_leitos>0)
#
# ATENÇÃO: # Os 3 kpis a seguir foram gerados por CNES para facilitar a conferência
#
# Gerando Taxa de Ocupação
# OBS1: Observem que fiz o calculo de todas as internações, mas gerei a taxa apenas de 2018
# OBS2: eu poderia ter utilizado a unidade PACIENTE mas deixei por AIH (para facilitar)
kpi7<-filter(dfocup,data>'2017-12-31')%>%
      group_by(cnes,ANO_CMPT=format(data,'%Y'),MES_CMPT=format(data,'%m'))%>%
      summarize(media_leitos_ocupados=round(mean(leitos_ocupados)),
                total_leitos_ocupados=round(sum(leitos_ocupados)),
                quantidade_leitos_existente=max(Quantidade_leitos),
                total_quantidade_leitos=round(max(Quantidade_leitos)*n()),
                qtde_dias_com_movimento=n(),
                kpi_tx_ocupacao=round(total_leitos_ocupados/total_quantidade_leitos*100,digits=3))
#
# profissionais por leito ocupado
gg<-left_join(g,dfcnes,by=c('cnes'='CNES'))
kpi8<-group_by(gg,cnes)%>%summarize(nro_profissionais=max(Numero_Profissionais),media_leitos_ocupados=max(media_leitos_ocupados),kpi_profissionais_leito_ocupado=round(sum(Numero_Profissionais)/sum(media_leitos_ocupados),digits=1))
#
# índice de rotatividade
kpi9<-group_by(dfsih,CNES,ANO_CMPT,MES_CMPT)%>%summarize(quantidade_internacoes=n(),quantidade_leitos=max(Quantidade_leitos),kpi_indice_rotatividade=round(quantidade_internacoes/quantidade_leitos,digits=1))

                                 