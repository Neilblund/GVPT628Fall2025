#Build a network based on cosponsorships----------------------------------------------------------------
library(tidyverse)
library(igraph)


# consponsored legislation
download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/cosponsorships.rds', 
              destfile = 'cosponsors.rds',
              mode='wb')

# sponsored legislation
download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/sponsors.rds', 
              destfile = 'sponsors.rds',
              mode='wb')

# house members
download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/current_house_members.rds', 
              destfile = 'members.rds',
              mode='wb')

cosponsors<-readRDS('cosponsors.rds')

sponsors<-readRDS('sponsors.rds')

members<-readRDS('members.RDS')


# combining the sponsor/cosponsor data
legi_data<-
  cosponsors|>
  bind_rows(sponsors)|>
  select(number, legislator)|>
  distinct()|>
  group_by(number)|>
  # removing anything with a single sponsor
  filter(n()>2)



# counting the number of bills where member i and member j were both sponsors
leg_table <- xtabs(~number +legislator, data=legi_data)

# log inverse sponsorship frequency - downweights popular bills
idf<-log(ncol(leg_table)/rowSums(leg_table))

# multiplying the count of consponsorships by the log inverse sponsor frequency
tfa<-leg_table * idf

# creating a member x member matrix
terms <- t(tfa) %*% tfa

# normalizing sponsorship counts for each member
terms <- terms / (rowSums(terms) - diag(terms))


graph<-graph_from_adjacency_matrix(terms, weighted=TRUE, mode='lower',
                                   diag=FALSE
)
# add party ID
vnames<-names(V(graph))

V(graph)$party<-members$partyName[match(vnames, members$bioguideId)] == "Republican"
V(graph)$color=c("blue","red")[as.numeric(V(graph)$party)+1]

V(graph)$legname<-members$name[match(vnames, members$bioguideId)]

# trim a lot of edges (only retains the 90th percentile of connections - this is probably too aggressive, but 
# it makes things run quicker)
cut.off <- quantile(E(graph)$weight, .9)
trimmed_net<-delete_edges(graph, E(graph)[weight < cut.off])




# create a big plot ----

pdf(file = 'trimmed_cosponsors.pdf', width=30, height=30)
layout <- layout_with_fr(trimmed_net, weights=E(trimmed_net)$weight)

plot(
  trimmed_net, 
     
     vertex.label=V(trimmed_net)$legname,  
     vertex.size=0, 
     layout = layout,
     vertex.label.color=V(trimmed_net)$color)

dev.off()


# making an interactive version of this plot
library(visNetwork)
# convert nodes/edges to data frames
nodes<-as_data_frame(trimmed_net, what='vertices')
nodes$id<-nodes$legname
edges<-as_data_frame(trimmed_net, what='edges')

# adding legislator names to the nodes instead of their bioguide ID
edges$from<-nodes$id[match(edges$from, nodes$name)]
edges$to<-nodes$id[match(edges$to, nodes$name)]

# makes an interactive plot
interactive_graph <- visNetwork(nodes, edges) |>
  visIgraphLayout() |>
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             height="100%", width = "100%"
             )|>
  visInteraction(navigationButtons = TRUE)

# fill the full size of the browser window
interactive_graph$sizingPolicy$browser$fill <- TRUE

# save as a standalone html file in the current working directory
visSave(interactive_graph, file = "cosponsor_network.html", 
        selfcontained = TRUE, background = "white")
