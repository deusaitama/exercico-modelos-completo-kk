
zumbis_1_$galoes_por_morador<-zumbis_1_$agua/zumbis_1_$moradores

zumbis_1_$zumbi<-factor(zumbis_1_$zumbi)

zumbis_1_$comida<-factor(zumbis_1_$comida)
zumbis_1_$sexo<-factor(zumbis_1_$sexo,levels = c(0,1),labels = c('masculino','feminino'))

esquisser(zumbis_1_)

ggplot(data=zumbis_1_,aes(y=galoes_por_morador,fill=zumbi))+
  geom_boxplot()


esquisser(zumbis_1_)

dados_sobreviveram<-zumbis_1_%>%filter(zumbi==0)
mean(dados_sobreviveram$galoes_por_morador)
max(dados_sobreviveram$galoes_por_morador)
min(dados_sobreviveram$galoes_por_morador)





dados_morreram<-zumbis_1_%>%filter(zumbi>0)
mean(dados_morreram$galoes_por_morador)
max(dados_morreram$galoes_por_morador)
min(dados_morreram$galoes_por_morador)

esquisser(zumbis_1_)

mean(dados_sobreviveram$idade)

mean(dados_morreram$idade)

table(zumbis_1_$sexo,zumbis_1_$zumbi)
table(zumbis_1_$area_residencia,zumbis_1_$zumbi)
table(zumbis_1_$comida,zumbis_1_$zumbi)
table(zumbis_1_$medicamentos,zumbis_1_$zumbi)
table(zumbis_1_$ferramentas,zumbis_1_$zumbi)
table(zumbis_1_$primeiros_socorros,zumbis_1_$zumbi)
table(zumbis_1_$sanitizacao,zumbis_1_$zumbi)
table(zumbis_1_$roupas,zumbis_1_$zumbi)
table(zumbis_1_$documentos,zumbis_1_$zumbi)







esquisser(zumbis_1_)


modelo_1<-glm(zumbi~area_residencia+comida+sanitizacao+medicamentos,data=zumbis_1_,family = 'binomial')
summary(modelo_1)

exp(coef(modelo_1))

exp(coef(modelo_1)[1])/(1+exp(coef(modelo_1)[1]))


indices<-sample(1:200,160,replace=FALSE)

treino<-zumbis_1_[indices,]
teste<-zumbis_1_[-indices,]

modelo_1<-glm(zumbi~area_residencia+comida+sanitizacao+medicamentos,data=treino,family = 'binomial')
summary(modelo_1)



predicoes<-predict(modelo_1,newdata = teste,type = 'response')
predicoes<-ifelse(predicoes>0.5,1,0)

confusionMatrix(factor(predicoes),factor(teste$zumbi))


eu<-data.frame(area_residencia="Urbana",comida="1",sanitizacao="1",medicamentos="1")

eu_morreria<-predict(modelo_1,newdata=eu,type='response')
ifelse(eu_morreria>0.5,'sobrevivi','morri')

eu<-data.frame(idade=22,sexo="masculino",area_residencia="Rural",moradores=3,agua=5,comida="1",medicamentos=1,ferramentas="1",sanitizacao=1,roupas="1",documentos="1")


eu_morreria<-predict(modelo_1,newdata=eu,type='response')
ifelse(eu_morreria>0.5,'sobrevivi','morri')



