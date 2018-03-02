# Simulated Landscapes Workflow



In this document we can do the main analysis for the simulated landscapes examples, from simulating the landscapes through to analysing via equations and motifs. Any underlying functions can be saved in the `R` folder, and be called using `source(function_file.R)`. The outline of the workflow is as follows:

1. Simulate landscapes with the following parameters: 

      * `p_supply`: proportion of the landscape which is ES supply 
      * `p_demand`: proportion of the landscape which is ES demand
      * `f_supply`: the fragmentation level of the ES supply (in the range [0, 1] with 1 being the most fragmented)
      * `f_demand`: the fragmentation level of the ES demand (in the range [0, 1] with 1 being the most fragmented)

<!--Version for the ABM method, if i get it working:
      * `inter`: the interpersion between ES supply and demand (in the range [0, 1] with 1 being completely interspersed)
-->
We will have landscapes for a range of each parameter, and for each parameter combination we will generate 100 replicates. 


2. Generate a distance matrix and an attribute dataframe from each simulated landscape. The distance matrix will contain Euclidean distances between every supply and demand patch within the landscape (supply-supply, demand-demand, supply-demand). The attribute table will contain the following columns:
      * `ID`: the patch identity to link back to the distance matrix
      * `patch_type`: whether the patch is supply or demand
      * `patch_area`: the size of the patch
  
3. Calculate ES benefit for the landscape based on equations

4. Generate the network for the landscape and perform motif analysis

5. Compare the equations and motif analysis results

# Simulate landscapes
<!--Version for the ABM method, if i get it working:
We will use the function `nlm_es` which I have written to allow us to control the amount, fragmentation and interspersion of supply and demand. This function is an agent based landscape simulation and is heavily based on the `nlm_neigh` function in the `NLMR` package. 
-->

For now, we are generating the landscapes by simulating a supply layer and a demand layer using the mid-point displacement method, binarising the surfaces, then putting them together. If a cell is both supply and demand, this cell is then converted to demand. If we continue to use this method (e.g. if the ABM code I am working on which will allow us to incorporate interspersion doesn't work out) we will make this more nuanced. 
The initial simulated landscapes will use all combinations of the following range of parameters: 

- `p_supply` and `p_demand`: fixed at 0.15
- `f_supply` and `f_demand`: 0 to 1 in increments of 0.25

This gives us a total of 25 simulated landscapes. At the moment we have 1 of each landscape, but for analysis we should generate replicates (100?) of each. 



The below figure provides an illustration of one iteration of the landscapes: 

![](workflow_files/figure-html/plot_ls-1.png)<!-- -->

# Distance and attribute tables

Next step is to create the distance matrix, and table which includes `ID`, `patch_type`, `patch_area` for each simulated landscape. We will also include the parameters used to generate the landscape on the attribute table, for ease of analysis. 



# ES benefit from equations



# Network motif analysis



# Equation and motif comparison



