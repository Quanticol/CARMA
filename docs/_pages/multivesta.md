---
title: "Statistical Analysis of CARMA Models"
excerpt: "To provide further analysis options for CARMA models beyond what the plug-in offers, we have developed an interface to the MultiVeStA platform."
sitemap: true
layout: single
permalink: /multivesta.html
---

The Eclipse plug-in can be used to simulate models and thus gain an understanding of their behaviour.
To provide further analysis options for CARMA models beyond what the plug-in offers, we have developed an interface to the MultiVeStA platform, allowing CARMA users access to the model-checking capabilities offered by the software. MultiVeStA can be used to evaluate the expected value of quantities of interest in different kinds of systems through a simulation-based procedure. The expressions of interest are defined using the special-purpose MultiQuaTEx language. In the case of \carma, such quantities can involve the current time in the simulation, the value of a measure or the number of times an action has occurred. These give a user access to a rich set of queries to place on a model, and provide deeper insight than what could be gained by simulation alone.

There are two ways in which MultiVeStA has been integrated into the existing tools.
The first way is through the GUI of the Eclipse plug-in. In this case, the query creation is a guided process: a menu entry opens a wizard presenting options for different kinds of queries. Specifically, we allow the user to estimate the following types of quantities:
- time until the value of a measure becomes equal, less, or greater than a specified value;
- time until the value of a measure becomes equal, less, or greater than that of another measure;
- probability that an inequality condition holds between the values of two measures, or a measure and a concrete value, at one or more specified time points;
- number of times an action has occurred until one or more specified time points.

The offered options cover a large range of common situations; the user can, for example, ask questions like "What is the probability that the number of free taxis is greater than the number of travelling taxis at time \dots?" or "How long does it take, on average, until the number of waiting users becomes less than a desired threshold?". The queries are specified by selecting elements in the graphical interface, and the plug-in automatically generates the corresponding MultiQuaTEx expressions, invokes MultiVeStA and presents the evaluation results. This way, the user does not need to be familiar with the underlying query language.

<!--
\begin{figure}
\begin{minipage}{0.49\linewidth}
	\centering
\includegraphics[scale=0.5]{pictures/multivesta_allQueries.png}
\end{minipage}%
\hfill
\begin{minipage}{0.49\linewidth}
	\centering
\includegraphics[scale=0.5]{pictures/multivesta_probQuery.png}
\end{minipage}
\caption{Visual interface for creating MultiVeStA queries: main dialog (left) and sample wizard for creating a new query (right).}
\label{fig:carma-multivesta-figs}
\end{figure}
--->

The second way of accessing the MultiVeStA functionality is through the [command line interface](cli.html). In this case, the user must provide a file with the expressions to be evaluated. Additional parameters can be given to customise the default behaviour of the algorithm, such as by specifying the desired confidence level of the result.
In contrast with the plug-in integrated workflow, using the command line interface requires some familiarity with the MultiQuaTEx syntax, but in return offers access to an ever wider range of potential queries.
