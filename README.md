## agvAPI

## Uso
```{r}
install.packages('remotes')
remotes::install_github('frzambra/agvAPI')
```
## Ejemplos

```{r}
library(agvAPI)
data_sm1 <- getdataAGV_sm()

data1 <- getDataAGV_clima(station_id ='z6-12948',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28')) 
 
data2 <- getDataAGV_clima(station_id ='0020500D',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28'))

data3 <- getDataAGV_clima(station_id ='00203581',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28')) 
```
