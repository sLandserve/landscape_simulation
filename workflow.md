---
title: "Simulated Landscapes Workflow"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 2
---



In this document we can do the main analysis for the simulated landscapes examples, from simulating the landscapes through to analysing via equations and motifs. Any underlying functions can be saved in the `R` folder, and be called using `sourceDirectory("'R")`. The outline of the workflow is as follows:

1. Simulate landscapes with the following parameters:

      * `p_supply`: proportion of the landscape which is ES supply
      * `p_demand`: proportion of the landscape which is ES demand
      * `f_supply`: the fragmentation level (fractal dimension) of the ES supply (in the range [0, 1] with 1 being the most fragmented)
      * `f_demand`: the fragmentation level (fractal dimension) of the ES demand (in the range [0, 1] with 1 being the most fragmented)
      * `inter`: the interpersion between ES supply and demand (in the range [0, 1] with 1 being the least interspersed)

We will have landscapes for a range of each parameter, and for each parameter combination we will generate 100 replicates.

2. Generate a distance matrix and an attribute data frame from each simulated landscape. The distance matrix will contain Euclidean distances between every supply and demand patch within the landscape (supply-supply, demand-demand, supply-demand). The attribute table will contain the following columns:

      * `ID`: the patch identity to link back to the distance matrix
      * `patch_type`: whether the patch is supply or demand
      * `patch_area`: the size of the patch

    Here we also generate a discrete networks for the network analysis based on the following parameters (we assume no demand-demand links for now):

      * `ee_thresh`: distance threshold for ecological-ecological links (NULL if no ecological-ecological links)
      * `se_thresh`: distance threshold for social-ecological links

3. Calculate ES benefit for the landscape based on the network. This uses general equations for the generation of ES benefits based no the network (see 'benefit_functions.docx' for details). This produces a table with the ES benefits for each replicate. The parameters required are:

Next step is to generate the benefits based on the distance type node type metric. Note here that at present we discretise the network based on a distance threshold for the social-ecological and ecological-ecological links and then calculate the benefit based on this network.

      * `rival`: whether the services are rival (TRUE) or non-rival (FALSE)
      * `alpha`: the rate of production of the potential ecosystem service per unit area at supply nodes (value > 0)
      * `beta`: the influence of connected supply nodes (i.e., the ecological-ecological links) on the rate of production of the potential ecosystem service at supply nodes (value > 0)
      * `gamma`: marginal utility of the service at zero service used
      * `eta`: rate of decline of the marginal utility with increasing use of the service (value > 0)

4. Perform network analysis to characterise network structure

5. Model ES benefits as a function of landscape structure parameters

6. Model ES benefits as function of network structure parameters

# Simulate landscapes

We have created a function `ls_create` which simulates social-ecological landscapes for a given amount and fragmentation of supply and demand, and interspersion between the two. Our simulated landscapes are built up from three neutral landscape models (NLM):

- supply landscape: a fractal Brownian motion NLM where we control the amount (`p_supply`) and configuration (`f_supply`)
- demand landscape: a fractal Brownian motion NLM where we control the amount (`p_demand`) and configuration (`f_demand`)
- a gradient landscape: a planar gradient NLM

The final social-ecological landscape is then created using the following steps:

- merge supply and demand landscapes (equal weighting to both)
- merge the above landscape with the gradient landscape (here, the weighting controls the level of interspersion using the parameter `inter`:

- 1 --> social and ecological patches are as interspersed as the p/f parameters will allow
- 0 --> social and ecological patches are as segregated as the p/f parameters will allow)

For simplicity, we currently are not simulating supply/demand patches.

The initial simulated landscapes will use all combinations of the following range of parameters:

- `p_supply` and `p_demand`: fixed at 0.15
- `f_supply` and `f_demand`: 0, 0.25, 0.5, 0.75, 1 (from low to high fragmentation)
- `inter`: 0, 0.25, 0.5, 0.75, 1 (from low to high interspersion)



The below figure provides an illustration of one iteration of the landscapes:



# Generate distance matrices and networks

Next step is to create the distance matrix, and table which includes `ID`, `patch_code` (0 = neutral, 1 = supply, 2 = demand, 3 = supply/demand), `patch_area` for each simulated landscape. We will also include the parameters used to generate the landscape on the attribute table, for ease of analysis.

Here we also create a discrete social-ecological network based on the distance thresholds between supply-supply nodes (ecological-ecological links) and supply-demand links (social-ecological links). For this we need to define whether ecological-ecological links exist, and the distance thresholds for each type of link, as follows:  

- `ee_thresh`: distance threshold for ecological-ecological links (set to FALSE if not needed)
- `se_thresh`: distance threshold for social-ecological links

We also generate some simple network metrics here: (1) number of supply nodes, (2) number of demand nodes, (3) density of ecological-ecological (supply-supply) links, and (4) density of social-ecological (supply-demand) links.  

This is where we also generate the network format for fanmod. For the fanmod file outputs, there are 4 columns (as per the expected fanmod input for coloured vertices): int1 = id of node one, int2 = id of node two, int3 = type of node one, int4 = type of node two. Types are supply, demand, or supply/demand.



The below figure provides an illustration of one iteration of the networks (NOTE NEEDS TO FIXED):



# ES benefit calculation

Next step is to generate the benefits based on the distance type node type metric. Note here that at present we discretise the network based on a distance threshold for the social-ecological and ecological-ecological links and then calculate the benefit based on this network.

Here we assume stylised networks for generic services with flows between supply and demand (see benefit_functions.docx for detailed descriptions of the benefits functions). There are a number of parameters to specify:

- `rival`: whether the services are rival (TRUE) or non-rival (FALSE)
- `alpha`: the rate of production of the potential ecosystem service per unit area at supply nodes (value > 0)
- `beta`: the influence of connected supply nodes (i.e., the ecological-ecological links) on the rate of production of the potential ecosystem service at supply nodes (value > 0)
- `gamma`: marginal utility of the service at zero service used
- `eta`: rate of decline of the marginal utility with increasing use of the service (value > 0)


# Network analysis



# Modelling ES benefits based on landscape structure



# Modelling ES benefits based on network structure


