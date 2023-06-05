# NN2Poly: A polynomial representation for deep feed-forward artificial neural networks
This repository contains all the code needed to implement the simulations and figures used in the following paper: 

* **Title**: "NN2Poly: A polynomial representation for deep feed-forward artificial neural networks".
* **Authors :** Pablo Morala (1,2), Jenny A. Cifuentes (3), Rosa E. Lillo (1,2), Iñaki Ucar (1,2).
* **Institutions**: (1) uc3m-Santander Big Data Institute, Universidad Carlos III de Madrid. Spain., (2) Department of Statistics, Universidad Carlos III de Madrid. Spain., (3) ICADE, Department of Quantitative Methods, Faculty of Economics and Business Administration, Universidad Pontificia Comillas. Spain.
* **Date:** December 2021, revised July 2022.
* **Keywords**: Polynomial Representation, Neural Networks, Machine Learning, Multilayer Perceptron, Interpretability, Multiset Partitions.
* **arXiv**: https://arxiv.org/abs/2112.11397
* **arXiv DOI**: https://doi.org/10.48550/arXiv.2112.11397

### Abstract
Interpretability of neural networks and their underlying theoretical behavior remain an open field of study even after the great success of their practical applications, particularly with the emergence of deep learning. In this work, NN2Poly is proposed: a theoretical approach to obtain an explicit polynomial model that provides an accurate representation of an already trained fully-connected feed-forward artificial neural network (a multilayer perceptron or MLP). This approach extends a previous idea proposed in the literature, which was limited to single hidden layer networks, to work with arbitrarily deep MLPs in both regression and classification tasks. The objective of this paper is to achieve this by using a Taylor expansion on the activation function, at each layer, and then using several combinatorial properties to calculate the coefficients of the desired polynomials. Discussion is presented on the main computational challenges of this method, and the way to overcome them by imposing certain constraints during the training phase. Finally, simulation experiments as well as an application to a real data set are presented to demonstrate the effectiveness of the proposed method.

### License
This project is licensed under the terms of the MIT license. See the [LICENSE](LICENSE.md) file for license rights and limitations.

