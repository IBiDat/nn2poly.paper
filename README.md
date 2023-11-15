# NN2Poly: A polynomial representation for deep feed-forward artificial neural networks
This repository contains all the code needed to implement the simulations and figures used in the following paper: 

- Pablo Morala, J. Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar (2023).
  "NNN2Poly: A Polynomial Representation for Deep Feed-Forward Artificial Neural Networks."
  _IEEE Transactions on Neural Networks and Learning Systems_, (Early Access).
  doi: [10.1109/TNNLS.2023.3330328](https://doi.org/10.1109/TNNLS.2023.3330328)

### Abstract
Interpretability of neural networks (NNs) and their underlying theoretical behavior remain an open field of study even after the great success of their practical applications, particularly with the emergence of deep learning. In this work, NN2Poly is proposed: a theoretical approach to obtain an explicit polynomial model that provides an accurate representation of an already trained fully connected feed-forward artificial NN a multilayer perceptron (MLP). This approach extends a previous idea proposed in the literature, which was limited to single hidden layer networks, to work with arbitrarily deep MLPs in both regression and classification tasks. NN2Poly uses a Taylor expansion on the activation function, at each layer, and then applies several combinatorial properties to calculate the coefficients of the desired polynomials. Discussion is presented on the main computational challenges of this method, and the way to overcome them by imposing certain constraints during the training phase. Finally, simulation experiments as well as applications to real tabular datasets are presented to demonstrate the effectiveness of the proposed method.

### Package installation 
The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

NN2Poly is implemented in the [nn2poly](https://github.com/IBiDat/nn2poly) package. To install the version corresponding to these paper simulations please use the [paper-tnnls-2023](https://github.com/IBiDat/nn2poly/releases/tag/paper-tnnls-2023) pre-release. Furthermore, that version needed the now outdated [nn2poly.tools](https://github.com/IBiDat/nn2poly.tools) auxiliar package.

```r
# install.packages("remotes")
remotes::install_github("IBiDat/nn2poly@paper-tnnls-2023")
remotes::install_github("IBiDat/nn2poly.tools")
```

### License
This project is licensed under the terms of the MIT license. See the [LICENSE](LICENSE.md) file for license rights and limitations.

