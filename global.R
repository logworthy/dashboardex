library(data.table)
library(ggplot2)

# Get Iris data & transform to a long format
data(iris)
dt <- data.table(iris)
dt[, id := ((seq_len(nrow(dt))-1) %% 50)+1]
metrics_long <- melt(dt, id.vars = c('id', 'Species'), value.name = 'Value', variable.name='Metric')

# key indicator relationships
reln1 <- expand.grid(Parent_Metric='Sepal.Length', Metric=c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
reln2 <- expand.grid(Parent_Metric='Sepal.Width', Metric=c('Sepal.Length', 'Sepal.Width', 'Petal.Width'))
reln3 <- expand.grid(Parent_Metric='Petal.Width', Metric=c('Petal.Length', 'Sepal.Width', 'Petal.Width'))
reln4 <- expand.grid(Parent_Metric='Petal.Length', Metric=c('Petal.Length', 'Sepal.Length', 'Petal.Width'))

driver_reln_all <- rbindlist(list(reln1,reln2,reln3,reln4))

# graph relationships
reln1 <- expand.grid(Parent_Metric='Sepal.Length', Metric=c('Sepal.Length', 'Petal.Length'))
reln2 <- expand.grid(Parent_Metric='Sepal.Width', Metric=c('Sepal.Width', 'Petal.Width'))
reln3 <- expand.grid(Parent_Metric='Petal.Width', Metric=c('Sepal.Width', 'Petal.Width'))
reln4 <- expand.grid(Parent_Metric='Petal.Length', Metric=c('Petal.Length', 'Sepal.Length'))

graph_reln_all <- rbindlist(list(reln1,reln2,reln3,reln4))
graph_reln_all[, display_name := stringr::str_extract(Metric, '^\\w+')]

# metric graph names
metrics_dim <- data.table(Metric=unique(metrics_long$Metric))
metrics_dim[, Graph_Name := stringr::str_extract(Metric, '\\w+$')]

# ID Mapping of in-scope periods
id_mapping <- data.table(id=unique(metrics_long$id))
id_mapping[, `Last 12 Months` := (id > 50-12 & id <= 50)]
id_mapping[, `All` := T]
id_mapping[, clicked := F]

# testing highlights
# metrics_long[id==best_fit$id & Value==best_fit$Value, clicked := T]

# Testing plotting
# ggplot(metrics_long[Species=='setosa' & Metric=='Petal.Width'], aes(x=id, y=Value))+geom_line()+
# geom_point(data=metrics_long[clicked==T], color='red')