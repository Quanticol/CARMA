---
title: "CARMA Eclipse plugin: Quick Starting Guide"
excerpt: "A very short introduction to CARMA Eclipse plugin"
sitemap: true
layout: single
permalink: /startingguide.html
---

After the installation of the CARMA Eclipse Plug-In you can start specifying your first system in CARMA.

First of all a CARMA Project has to be created. Select *File -> New -> Other...*

![Eclise menu: new project](assets/images/newsoftware-297x300.jpg)

After that select *CARMA Project* under the *CARMA* category and click on button Next:

![New Project Wizard: New CARMA project](assets/images/newcarmaproject-300x284.png)

Give a name to the project and click on Finish.

![Details of a new CARMA Project](assets/images/projectdetails-300x286.png)

The *New CARMA Project Wizard* creates a new project in the current Workspace. In the project you can find a simple CARMA specification that is used to illustrate the basic structure of a *.carma* file:

![Exploring content of CARMA Project](assets/images/projectinnavigation.png)

**Warning:** The *New CARMA Project wizard* installs in the created project all the required Eclipse bundles. If you open or create a *.carma* file in a different project you can access to the syntax highlight features but you cannot use the simulation tool provided with CARMA.

Double-click on *Example.carma* to open the editor.

![Exploring content of CARMA Project](assets/images/carmaeditor-300x253.png)

Given a CARMA specification, the CARMA Eclipse Plug-in automatically generates the Java classes providing the machinery to simulate the model.

![Generated CARMA files](assets/images/generatedfile-300x183.png)

To access the simulation features, a user can use the Carma Simulation View:

![Eclipse menu: Show view -> Other](assets/images/showview-258x300.png)

![Open CARMA Simulation View](assets/images/openexperimentview-221x300.png)

In the view the following commands are available:

- ![Add new experiment](assets/images/add_obj.gif) Add a new experiment to the selected project;
- ![Delete the selected experiment](assets/images/delete_obj.gif) Delete the selected experiment;
- ![Edit selected experiment](assets/images/write_obj.gif) Edit selected experiment;
- ![Save all experiments](assets/images/saveall_edit.gif) Save all experiments;
- ![Save the experiments of selected projects](assets/images/save_edit.gif) Save the experiments of selected projects;
- ![Run selected experiment](assets/images/lrun_obj.gif) Run selected experiment;
- ![Copy selected experiment](assets/images/copy.gif) Copy selected experiment;
- ![Plot experiment results](assets/images/chart_line.gif) Plot experiment results;
- ![Save experiment results into CSV a file](assets/images/datasheet.gif) Save experiment results into CSV a file.

Carma Simulation View can be used to create new experiments

![Create a new simulation experiment](assets/images/create_experiment-300x94.png)

after that a dialogue box pops up to choose the model and the system to simulate and the appropriate values to execute the simulation:

![Create a new simulation experiment](assets/images/experimentdata.png)

The experiment is then created and added in the view:

![Create a new simulation experiment](assets/images/experimentdetails-300x240.png)

A simulation experiment can be selected and then executed:

![Select experiment and run simulation](assets/images/runsimulation.png)

![Progress of a simulation](assets/images/runsimulation-300x127.png)

When the simulation completes, collected results are reported in the view and can be plotted chart_line in the Experiment Results View or saved in a CSV file datasheet.

![Select experiment and run simulation](assets/images/simulationresult-300x142.png)

![Select experiment and run simulation](assets/images/simulationplot-300x141.png)

Experiments can be saved inside the project via buttons ![Save all experiments](assets/images/saveall_edit.gif) ![Save the experiments of selected projects](assets/images/save_edit.gif) to let them available for further analysis.
