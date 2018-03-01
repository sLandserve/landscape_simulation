# Simulated Landscapes Workflow



In this document we can do the main analysis for the simulated landscapes examples, from simulating the landscapes through to analysing via equations and motifs. Any underlying functions can be saved in the `R` folder, and be called using `source(function_file.R)`. The outline of the workflow is as follows:

1. Simulate landscapes with the following parameters: 
      * `p_supply`: proportion of the landscape which is ES supply
      * `p_demand`: proportion of the landscape which is ES demand
      * `f_supply`: the fragmentation level of the ES supply (in the range [0, 1] with 1 being the least fragmented)
      * `f_demand`: the fragmentation level of the ES demand (in the range [0, 1] with 1 being the least fragmented)
      * `inter`: the interpersion between ES supply and demand (in the range [0, 1] with 1 being completely interspersed)
We will have landscapes for a range of each parameter, and for each parameter combination we will generate 100 replicates. 
  
2. Generate a distance matrix and an attribute dataframe from each simulated landscape. The distance matrix will contain Euclidean distances between every supply and demand patch within the landscape (supply-supply, demand-demand, supply-demand). The attribute table will contain the following columns:
      * `ID`: the patch identity to link back to the distance matrix
      * `patch_type`: whether the patch is supply or demand
      * `patch_area`: the size of the patch
  
3. Calculate ES benefit for the landscape based on equations

4. Generate the network for the landscape and perform motif analysis

5. Compare the equations and motif analysis results

# Simulate landscapes

# Distance and attribute tables

# ES benefit from equations

# Network motif analysis

# Equation and motif comparison