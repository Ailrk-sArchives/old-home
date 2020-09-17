-- tag note maching-learning unsupervised-learning
-- title Clustering
-- date 2020-09-11
-- source https://web.stanford.edu/~hastie/Papers/ESLII.pdf
;;
# Clustering

## Unsupervised learning
Algorithms that looking for patterns of the unlabeled data without human intervention.

## SSE: Sum of squared error.
This is a very common metric for assessing distance between points.

SSE: the sum of squared differences between each observatino and it's group's mean.  It is used as a measure of variation within a cluster. If all casese in a cluster are identical, in another word, points overlap at one poing, then sse will be zero.

```
      n
SSE = ∑ (xᵢ - avg x)²
     i=1
```

## Clustering

## KMeans

##### PS
All this machine learning methods have their theorotical goals and different implementation. The acutal implementation can be very different from their conceptual goal, os it is helpful to describe them in two different sections.

##### Conceptually
Kmean aims to choose entroids that minimize the intertia (within cluster sum of squares).

```
let C be set of K disjointed clusters
    xᵢ be the input data. {x₁, x₂, ... xᵢ}
    μⱼ be the mean of samples in cluster
    n is the number of input
for every j ∈ K, we want to find μⱼ that satisfy this constraint:
                   n
    J(μ₁ .. μₖ) =  ∑ min(||xᵢ - μⱼ||²), μⱼ ∈ C
                  i=0
```

This is the cost function, which is kinda different from pure function. With a normal pure func you take an argument and substitue it into the function body, the result of the evaluation is what you want to get.

But for the cost function, you can do similar thing: get a set of specific arguments and evaluate the function. But now the result of evaluation is not what your goal anymore. You want to find a specific set of arguments that can minimize the result of evaluation.

So really, the argument is the unknown, and we're trying to find the answer backward.

##### Implementation
- Mean of points means take average of the result of vector addition.
- It's a iterative algorithm with two steps.
- Some times the result will not convergent, and we need to set a tolerant threshold.

```
init K clusters centroids {μ₁, μ₂, ... , μₖ} randomly.
while {
}

// assign cluster centroids with cloest sample data points.
def assign {
    for i = 1 to m
       cᵢ := index of cluster centroids closet to xᵢ
}

// relocate cluster centroids with mean all assigned data points.
def step {
    for k=1 to K
       μₖ := mean of points assigned to cluster k
}
```

##### Optmization
Random initialization is possible to cause the resulting centroids end up at local optima and yield different solutiont from what we expect.

To solve this problem we try random initialization mutiple times, and pick the one with the lostest cost. (Yeah, brute force)

It's said this is not a big performance problem. Stuck at local optima typically happen when data set is small, since when data set is large the chance of data being stucked is much smaller. One can also dynamically adjust the amount of iterations based on sample size.

##### Choose number of cluster
Most common thing to do is to choose the number of clusters by hand...

There is even a heuristic method called elbow method to help you choose ncluster.

On another hand, the number of clusters can actually be chosen by your business logic! For example, if you want to partition a data set into 3 different groups so you can produce different versions of product, you know nclusters already!

## Hierarchy Agglomerative Clustering.
The grouping idea is like fast labeling algorithm, smaller cluster eventually merge to a big cluster.

complexity O(n³), very slow.

The clustering eventually produce ea dendrogram. It's actually a trie.

###### Procedure
Calculate the distance between each points and sort it. group the cloest one and repeat.


## K-Medoids
- Kmean is sensitive on outliner
- Kmedoids choose data points as centers, instead of like kmean use arbitrary points.
- O(k(n-k)²) -> O(n²)

##### Procedure

```
PAM algorithm:

1. Init k medoids.
2. Assign each data point to the closest medoid.
3. while sse decrease, for each non medoids o, swap each amedoids m and o until the sse is the current best. Then perform the best swap.
```

```
Voronoi iteration
1. Selecet initial medoids randomly
2. Iterate while the cost decrease:
    1. In each cluster, make the point the minimizes the sun of distance the medoid
    2. Reassing.
```
