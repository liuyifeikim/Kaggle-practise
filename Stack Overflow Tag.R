library(igraph)
library(ggplot2)
library(plyr)
library(useful)
library(car) #scatterplotMatrix
library(corrplot) #corrplot
library(corrgram) #corrgram

#read data
nodes <- read.csv('./stack-overflow-tag-network/stack_network_nodes.csv', stringsAsFactors = FALSE)  #nodes
links <- read.csv('./stack-overflow-tag-network/stack_network_links.csv', stringsAsFactors = FALSE)  #links
str(nodes)
str(links)

#some exploration
choose(length(nodes$name), 2) #可能出现的最大连接数=6555
combn(nodes$name, 2) %>% t() %>% as.data.frame()

ggplot(nodes, aes(reorder(name, nodesize), nodesize, fill = factor(group))) +
  geom_bar(stat = 'identity') +
  theme(axis.text.y = element_text(size = 8), panel.grid = element_blank()) + 
  coord_flip()

list_group <- rep(NA, length(unique(nodes$group)))
for (i in 1:length(unique(nodes$group))){
  list_group[i] = list(nodes$name[nodes$group == i])
}
list_group #每组的内容列表
table(nodes$group)

#net(directed)
net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)
net
str(net)
get.adjacency(net) #get matrix
get.adjedgelist(net) #get edgelist

#basic
vcount(net) #节点数
ecount(net) #边数
V(net) #节点列表
E(net) #边列表

#密度
edge_density(net) #density
ecount(net) / (choose(vcount(net), 2) * 2) #density

#距离
diameter(net, directed = T) #最长距离（直径）
get.diameter(net, directed = T) #最长距离路径
eccentricity(net, 'r') #特定点离心率，到各点距离最大值

#distances
distances(net) #距离矩阵
shortest.paths(net) #同distance
mean_distance(net) #平均距离
distances(net)['r',] %>% sort() %>% head(10) #特定点与其他点最短距离
distances(net, v = 'r', to = 'python') #特定点与特定点最短距离
shortest.paths(net, v = 'r', to = 'python') #同上
shortest_paths(net, from = 'r', to = 'jquery')$vpath #输出最短路径

#ego network
ego(net, 1, 'r') #n步内的连接对象
ego_size(net, 1, 'r') #n步内有连接的对象数量
net_r <- make_ego_graph(net, 1, 'r')[[1]] #局部网络
plot(net_r, vertex.label.cex = 2, edge.arrow.size = 0.4) #局部网络制图

#degree
degree(net) %>% sort(decreasing = T)
degree(net, normalized = TRUE) %>% sort(decreasing = T) 
degree(net, mode = 'in', normalized = T) %>% sort(decreasing = T) 
degree(net, mode = 'in', normalized = T)['r'] 

degree_distribution(net) #度的比例分布
degree_distribution(net, mode = 'in', cumulative = T)

degree(net, mode = 'in') %>% mean() #mean outdegree=mean indegree
degree(net, mode = 'in') %>% sd() #sd indegree

#closeness
closeness(net) %>% sort(decreasing = T) #closeness
closeness(net, mode = 'in', normalized = T) %>% sort(decreasing = T) #closeness
closeness(net, mode = 'in', normalized = T)['r']

#betweenness
betweenness(net) %>% sort(decreasing = T) #betweeness
betweenness(net, normalized = T) %>% sort(decreasing = T)
betweenness(net, normalized = T)['r']
edge.betweenness(net, directed = T) #edge betweeness

#subgroup
clusters(net) #有联系的结点聚合
components(net) #和cluster一样
component_distribution(net, cumulative = T)
decompose.graph(net) #子图分解

cliques(net) #完全子图,可能为最大完全子图的一部分
max_cliques(net) #最大完全子图,最少2个节点
count_max_cliques(net) #最大完全子图数量
largest_cliques(net) #全局最大完全子图
clique_num(net) #全局最大完全子图的节点数量

max_c1 <- largest_cliques(net)[[1]]
max_c1_plot <- induced_subgraph(graph = net, vids = max_c1) #转换为graph对象
plot(max_c1_plot)

cliques_python <- max_cliques(net, subset = 'python')
cliques_python_plot <- induced_subgraph(net, vids = cliques_python)
plot(cliques_python_plot)

max_cliques <- max_cliques(net)
max_cliques_num <- sapply(as.list(max_cliques), length)
table(max_cliques_num)






coreness(net) #每个节点所在最大k核图的核数
coreness(net)['python'] 
coreness(net)['r']

sg_python_r <- induced_subgraph(net, c('python', 'r')) #make a subgraph
sg_python_r
plot(sg_python_r)



#reciprocity
reciprocity(net)

#transitivity
transitivity(net, 'local') #联通度
transitivity(net, 'global')

#plot
min_max <- function(x){(x - min(x))/(max(x) - min(x))}
plot(net,
     vertex.color = 'blue',vertex.size = 0, 
     vertex.label.cex = min_max(V(net)$nodesize) + 1, vertex.label.family = 'Times',
     edge.color = 'grey', edge.width = E(net)$value / 10,
     edge.arrow.size = 0.1,
     layout = layout_with_fr(net))

#最长路径示意图
diam <- get.diameter(net, directed = T) #最长距离路径
vcol <- rep('blue', vcount(net))
vcol[diam] <- 'red'
ecol <- rep('grey', ecount(net))
ecol[E(net, path = diam)] <- 'red'
plot(net, 
     vertex.size = 0,
     vertex.label.color = vcol, vertex.label.cex = min_max(V(net)$nodesize) + 1,
     edge.color = ecol,
     edge.arrow.size = 0.2,
     layout = layout_with_fr(net))


#clustering
degree <- degree(net, normalized = T)
betweenness <- betweenness(net, normalized = T)
closeness <- closeness(net, normalized = T)

center <- cbind(degree, betweenness, closeness) %>% data.frame()
center_s <- lapply(center, scale) %>% data.frame()
summary(center_s)

k <- c(1:20)
prop <- rep(NA, 20)
for (i in 1:20){
  prop[i] = kmeans(center_s, i)$betweenss / kmeans(center_s, i)$totss
}
kmeans_data <- cbind(k, prop) %>% data.frame()
ggplot(kmeans_data, aes(k, prop)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(0, 20, 1)) + theme(panel.grid.minor = element_blank())

set.seed(5)
k5 <- kmeans(center_s, 5)
k5$centers
k5$cluster
plot(k5, data = center_s) + geom_point(size = 3) #useful package

v_cluster <- cbind(V(net)$name, k5$cluster) %>% data.frame(stringsAsFactors = FALSE) 
colnames(v_cluster) <- c('vector', 'cluster')
v_cluster
list_cluster <- rep(NA, length(unique(v_cluster$cluster)))
for (i in 1:length(unique(v_cluster$cluster))){
  list_cluster[i] = list(v_cluster$vector[v_cluster$cluster == i])
}
list_cluster
k5$centers

cluster <- cluster_edge_betweenness(net, directed = T)
dendPlot(cluster, mode = 'hclust')

#correlation
pairs(center_s)
scatterplotMatrix(center_s)
scatter3d(center_s$degree, center_s$betweenness, center_s$closeness)
corrgram(center_s, order = TRUE, upper.panel = NULL)
corrplot(cor(center_s), method = 'color', order = 'AOE', addCoef.col = 'black')
