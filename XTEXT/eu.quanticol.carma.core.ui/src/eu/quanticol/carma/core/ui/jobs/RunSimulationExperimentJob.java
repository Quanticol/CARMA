package eu.quanticol.carma.core.ui.jobs;


import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.SimulationMonitor;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.views.ExperimentResultsView;
import eu.quanticol.carma.core.ui.views.SimulationView;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;

public class RunSimulationExperimentJob extends Job {
	
//	private MeasureData[] measures;
	private SimulationEnvironment<CarmaSystem> sim;
	private SamplingCollection<CarmaSystem> sc;
//	private CarmaModel model;
//	private IResource resource;
	private SimulationSuiteElement experimentSuiteElement;
	private SimulationExperiment experiment;
	private SimulationView view;
	private String tag;
	private long startTime;
	private long endTime;
	
	public RunSimulationExperimentJob( SimulationView view , SimulationSuiteElement experimentSuiteElement ) {
		super("Simulation");
		this.experimentSuiteElement = experimentSuiteElement;
		this.view = view;
		
		this.experiment = experimentSuiteElement.getSimulationExperiment();
		
		this.sim = new SimulationEnvironment<CarmaSystem>( this.experiment.getCarmaModel().getFactory( experiment.getSystem() ) );
		this.sc = new SamplingCollection<CarmaSystem>();
		
		for(MeasureData measure : this.experiment.getMeasures()){
			this.sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(1+experiment.getSamplings(), this.experiment.getSimulationTime()/this.experiment.getSamplings() , this.experiment.getCarmaModel().getMeasure(measure.getMeasureName(),measure.getParameters())));
		}
		
		this.sim.setSampling(sc);
		DateFormat dateFormat1 = new SimpleDateFormat("dd/MM/yyyy  HH:mm:ss");
	    Calendar cal1 = Calendar.getInstance();
	    this.tag = dateFormat1.format(cal1.getTime());
	}
	
	public void simulate(IProgressMonitor monitor){
		monitor.beginTask("Simulation...", experiment.getReplications());
		this.startTime = System.currentTimeMillis();
		this.sim.simulate( new SimulationMonitor() {
			
			@Override
			public void startIteration(int i) {
				monitor.subTask("Replication "+i);
			}
			
			@Override
			public boolean isCancelled() {
				return monitor.isCanceled();
			}
			
			@Override
			public void endSimulation(int i) {
				monitor.worked(1);
			}
		} , experiment.getReplications(),experiment.getSimulationTime());
		this.endTime = System.currentTimeMillis();
		updateView();
	}
	
	public String getModelName(){
		return this.experiment.getResource().getName();
	}
	
	public SamplingCollection<CarmaSystem> getCollection(){
		return this.sc;
	}
	
	public String getSystem(){
		return this.experiment.getSystem();
	}
	
	public List<MeasureData> getMeasures(){
		return this.experiment.getMeasures();
	}
	
	public int getSamples(){
		return this.experiment.getSamplings();
	}
	
	public double getDeadline(){
		return this.experiment.getSimulationTime();
	}
	
	public int getIterations(){
		return this.experiment.getReplications();
	}
	
	public String toCSVString(){
		return "Model;"+this.getSystem()+";deadline;"+this.getDeadline()+";iterations;"+this.getIterations()+";samples;"+this.getSamples();
	}
	
	public void updateView(){
		
		Display.getDefault().asyncExec(new Runnable() 
		{ 
			public void run() {
				addSimulationResults();
			}
			
		});
	}

	protected void addSimulationResults() {
		double totalTime = endTime-startTime;
		experiment.addSimulationResult(new SimulationOutcome(tag, totalTime, totalTime/experiment.getReplications(), sc.getSimulationTimeSeries(getIterations())));
		view.refreshData();
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		simulate(monitor);
		return Status.OK_STATUS;
	}

}
