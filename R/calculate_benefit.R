calculate_benefit <- function(net, rival, alpha, beta, eta, lambda) {
  # generates ES benefit values for the networks
  # net = output from the function network_create() for one landscape
  # rival = whether the service is rival (TRUE) or non-rival (FALSE)
  # alpha = rate of production of the potential ecosystem service per unit area at supply nodes
  # beta = influence of connected supply nodes on the rate of production of the potential ecosystem service
  # eta = number of beneficiaries per unit area at demand nodes
  # lambda = utility per unit of ecosystem service utilised

  # get the ES production at each of the supply nodes
  # get supply and demand ids - note here that supply/demand nodes are not considered, only supply or demand
  supply_ids <- which(net$network$node_type == 1)
  demand_ids <- which(net$network$node_type == 2)
  #calculate ES production for each supply node
  ss_links <- net$network$net_links[supply_ids, supply_ids]
  s_areas <- net$network$node_areas[supply_ids]
  supply_ES <- apply(as.matrix(1:length(supply_ids)), MARGIN = 1, FUN = function(x, ss_links, s_areas, alpha, beta) {if(!is.null(dim(ss_links))) {alpha * s_areas[x] + beta * sum(ss_links[x,] * s_areas)} else {alpha * s_areas[x] + beta * sum(ss_links * s_areas)}}, ss_links = ss_links, s_areas = s_areas, alpha = alpha, beta = beta)
  #calculate ES utility for each demand node
  ds_links <- net$network$net_links[demand_ids, supply_ids]
  print(dim(ds_links))
  d_areas <- net$network$node_areas[demand_ids]
  if (rival == FALSE) {
    # in this case the services are non-rival
    benefit_ES <- apply(as.matrix(1:length(demand_ids)), MARGIN = 1, FUN = function(x, supply_ES, ds_links, s_areas, eta, lambda) {if(!is.null(dim(ds_links))) {lambda * eta * d_areas[x] * sum(ds_links[x,] * supply_ES)} else {lambda * eta * d_areas[x] * sum(ds_links * supply_ES)}}, supply_ES = supply_ES, ds_links = ds_links, s_areas = s_areas, eta = eta, lambda = lambda)
  }
  else if (rival == TRUE) {
    # in this case the services are rival
    benefit_ES <- apply(as.matrix(1:length(demand_ids)), MARGIN = 1, FUN = function(x, supply_ES, ds_links, s_areas, eta, lambda) {  if(!is.null(dim(ds_links))) {lambda * eta * d_areas[x] * sum(apply(ds_links, MARGIN = 2, FUN = function(y, d_areas) {if (sum(y) > 0) {1 / sum(y * eta * d_areas)} else {0}}, d_areas = d_areas) * ds_links[x,] * supply_ES)} else {lambda * eta * d_areas[x] * sum(ds_links * supply_ES)}}, supply_ES = supply_ES, ds_links = ds_links, s_areas = s_areas, eta = eta, lambda = lambda)
}
  benefit <- sum(benefit_ES)
  output <- append(net$params, benefit)
  names(output)[length(output)] <- "benefit"

  return(output)
}
