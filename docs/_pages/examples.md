---
title: "Examples"
excerpt: "A set of examples that show how CaSL can be used."
sitemap: true
layout: single
permalink: /examples.html
---
{% include toc %}

## Bike Sharing System

We consider a [model](assets/code/BSS.carma) for a bike sharing system. In the system bikes are made available in a number of stations that are placed in various
areas of a city. Users that plan to use a bike for a short trip can pick up a bike at a suitable origin station and return it to any other station close to their planned destination.

One of the major issues in bike sharing systems is the availability and distribution of resources, both in terms of available bikes at the stations and in terms of available empty parking places in the stations.

In our scenario we assume that the city is partitioned in homogeneous zones and that all the
*stations* in the same zone  can be equivalently used by any user in that zone.


## Bike Sharing System 2

We consider another [model](assets/code/BSS2.carma) for a bike sharing system consisting of *N=1000* stations of capacity *K=30* and a fleet size of *sN* bikes, where *s* is the average number of bikes per station.
The model abstracts from  the actual distribution of bike stations in space (hence the attribute
``homogeneous'') and assumes that stations are randomly chosen by users.

A generic station is modelled by the component *HBSStation* with two attributes: *cp*, recording the
capacity (initialised to $K$ and constant in time); and *npb*, recording the number of
bikes parked in the station. The behaviour of the station is straightforward; it consists of a single state *Y*
since the relevant information is kept in attribute *npb*. The actions modelling *get*ting  and *ret*urning  bikes
have no  synchronisation requirement (they play the role of  ``internal'' actions in the process algebra sense). The rate of a *get* transition of an individual
station is constant. The rate of a *ret* transition is given by a constant multiplied by the number of bikes in circulation in the system
divided by the total number of stations  *N*. In the model, the  number of bikes in circulation
is kept in the global attribute *incirculation*.

## SIER Model

We consider the classical SIER [epidemic model](https://en.wikipedia.org/wiki/Epidemic_model). This model can be modelled in CARMA in different way:

+ [SIER1](assets/code/SIER1.carma)
+ [SIER2](assets/code/SIER2.carma)
+ [SIER3](assets/code/SIER3.carma)

## BUS Model

We consider here a [simple scenario](assets/code/SIER1.carma) composed by two routes, identified with the integers *1* and *2*. These routes connect *8* stops numbered from *0* to *7*. Route *1* is a *slow line* and connects all the stops in a sequential order. Differently, route *2* is a a *fast line* and only connects even stops. A special location numbered *-1* is also used to identify the bus depot. Route *1* starts at location *0*, while route *2* at location *4*.
