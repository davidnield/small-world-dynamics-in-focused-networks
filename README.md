# Small World Dynamics in Focused Networks

# Description
This project contains code that investigates "small world" dynamics using simulated network data. The first part of the code contains an original function which can be used to convert mixed bipartite network graphs (where individuals are tied to both individuals and focuses) into bipartite projections (where edges are added between all individuals tied to the same focus, and the focus is deleted). Following the function is a very basic demonstration of it at work. This functionality is surprisingly not native to igraph and from what I can tell from help pages and documentation, this is the first publicly available function for creating bipartite projections from mixed mode bipartite network graphs.

The remainder of the code replicates and extends Watts and Strogatz (1998) by demonstrating the power of a relatively small amount of random ties for creating small worlds. First by presenting the functional form of networks' average shortest path length for regular/lattice graphs, random graphs, and semi-random graphs (regular graphs with 10% of ties rewiring) at varying network sizes (an analysis not presented in the original paper. Then, the analysis in the original paper is replicating by presenting the functional form of networks' transitivity (or clustering coefficent) and average shortest path length at varying rewiring rates. Finally, I extend Watts and Strogatz by using my function to add focuses at regular intervals in the network (every 10, 20, and 100 nodes) and estimate the effects the introduction of focuses has on the "small world" dynamics.

# Dependencies
1. R version 3.4.1

# Files
## Data
All data are simulated and generated within the included R script.

## Code 
1. 01_small_world_dynamics.R: Creates mixed bipartite projection function. Extends Watts and Strogatz (1998) to varying levels of network size. Replicates the Watts and Strogatz plot for varying rewiring probabilities. Extends the Watts and Strogatz rewiring plot to compare the effects of adding focuses (10%, 10% with degree normalization, 5%, and 1%).

2. 02_small_world_dynamics_presentation.R: Contains the code necessary to generate all of the figures used in the presentation (including all 15 mentioned below) including some that are not part of the primary analysis (such as those used to illustrate network terminology). Warning: running this code will generate 15 pdfs into your working directory. With the exception of one extra graph, some added graphing parameters to make titles and labels larger, and Cairo pdf export lines of code, this script is identical to the one described above.

## Results
1. BasicNetworkPlot1.pdf: Basic 5 node, 4 edge network used to demonstrate network terminology in presentation

2. BasicNetworkPlot2.pdf: Basic 5 node, 5 edge network used to illustrate the concept of transitivity

3. BipartiteConcept.pdf: Bipartite projection of BasicNetworkPlot1 where node A is defined to be a focus, used to illustrate what focuses are and what a bipartite projection is.

4. RegularGraph.pdf: A 50 node example of a regular network or lattice

5. RegularCloseUp.pdf: A close-up look at the above graph to illustrate how the ties look.

6. RegularPathLength.pdf: Shows the functional form of regular graph's average shortest path length as network size increases.

7. RandomGraph.pdf: A 50 node example of an Erdos-Renyi random graph.

8. RandomPathLength.pdf: Shows the functional form of a random graoh's average shortest path length as network size increases.

9. SemiRandomGraph.pdf: A 50 node example of a regular network or lattice with 10% of edges randomly rewired.

10. SemiRandomPathLength.pdf: Shows the functional form of a semi-random graph's average shortest path length as network size increases. Note the similarity to the fully random graph despite only 10% of the ties being random.

11. WattsStrogatzPlot.pdf: Replicates the famous plot in Watts and Strogatz (1998). Average shortest path length rapidly decreases with a small amount of random rewiring while transitivity (or clustering) is not majorly affected until many ties are rewired.

12. FocusedWattsStrogatzPlot10.pdf: Adds focuses to Watts and Strogatz's network (every 10th node, or 10% of nodes, are classified as focuses and collapsed. The focused network begins with 1110 nodes to account for node deletion in the eventual projection) and compares the effects on average shortest path length and transitivity. Large difference before any rewiring, but difference quickly evaporate with even a small amount of rewiring.

13. FocusedWattsStrogatzPlot10Normalized.pdf: Adding focuses to the network increased the average degree of the network from 10 (from being connected to the 5 nearest neighbors on each side) to 12.22. This plot evens the playing field by giving the unfocused network an average degree of 12 (connected to the 6 nearest neighbors on each side) while keeping the degree of the focused network the same. This demonstrates that most of the initial difference is not due to the increase in average degree, but specific to how focuses change the structure of the network. But again, the differences quickly evaporate after a small amount of rewiring.

14. FocusedWattsStrogatzPlot5.pdf: Changes the interval of added focuses from every 10 nodes to every 20 nodes (or 5% of the nodes).

15. FocusedWattsStrogatzPlot1.pdf: Changes the interval of added focuses from every 10/20 nodes to every 100 nodes (or 1% of the nodes). Differences almost completely evaporate.

## Presentation
1. small_world_beamer.tex: When combined with the pdfs above and the .bib file and sty folder below, this tex file creates the pdf used in my presentation
2. small_world_bib.bib: BibTex file with the three references cited in the presentation
3. sty: Folder containing the style folders for the Metropolis theme
4. Small World Dynamics in Focused Networks.pdf: My final project presentation.

# More Information
Credit is due to Tim Marple, who helped me figure out the mixed bipartite projection problem. Much of the code in the function is his writing, I just put it in functional form. Credit is also due to the developers of igraph, whose demos and documentation made this project much easier than I initially thought it would be.

Please e-mail me at nield.dr@gmail.com if you have any questions.
