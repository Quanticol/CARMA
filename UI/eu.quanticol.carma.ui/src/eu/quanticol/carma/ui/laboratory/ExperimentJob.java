package eu.quanticol.carma.ui.laboratory;


import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.SimulationMonitor;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.ui.views.ExperimentResultsView;

public class ExperimentJob extends Job {
	
	private double deadline;
	private int iterations;
	private int samplings;
	private String system;
	private String modelName;
	private String[] measures;
	private SimulationEnvironment<CarmaSystem> sim;
	private SamplingCollection<CarmaSystem> sc;
	private CarmaModel model;
	
	public ExperimentJob(double deadline, int samplings, int iterations, String system, String[] measures, CarmaModel model, String modelName) {
		super("Simulation");
		this.deadline = deadline;
		this.samplings = samplings;
		this.iterations = iterations;
		this.system = system;
		this.measures = measures;
		this.model = model;
		this.modelName = modelName;
		
		this.sim = new SimulationEnvironment<CarmaSystem>( this.model.getFactory( this.system ) );
		this.sc = new SamplingCollection<CarmaSystem>();
		
		for(String measure : this.measures){
			this.sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(1+this.samplings , this.deadline/this.samplings , this.model.getMeasure(measure)));
		}
		
		this.sim.setSampling(sc);
		
	}
	
	public void simulate(IProgressMonitor monitor){
		monitor.beginTask("Simulation...", iterations);
		this.sim.simulate( new SimulationMonitor() {
			
			@Override
			public void startIteration(int i) {
				monitor.subTask("Iteration "+i);
			}
			
			@Override
			public boolean isCancelled() {
				return monitor.isCanceled();
			}
			
			@Override
			public void endSimulation(int i) {
				monitor.worked(1);
			}
		} , iterations,deadline);
		
		updateView();
	}
	
	public String getModelName(){
		return this.modelName;
	}
	
	public SamplingCollection<CarmaSystem> getCollection(){
		return this.sc;
	}
	
	public String[] getMeasures(){
		return this.measures;
	}
	
	public int getSamples(){
		return this.samplings;
	}
	
	public double getDeadline(){
		return this.deadline;
	}
	
	public int getIterations(){
		return this.iterations;
	}
	
	public String toCSVString(){
		return "Model;"+this.modelName+";deadline;"+this.getDeadline()+";iterations;"+this.getIterations()+";samples;"+this.getSamples();
	}
	
	public void updateView(){
		
		ExperimentJob job = this;
		Display.getDefault().asyncExec(new Runnable() 
		{ 
			public void run() {
				try {
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(ExperimentResultsView.ID);
					ExperimentResultsView.update(job);
				} catch (PartInitException e) {
					e.printStackTrace();
				}
			}
		});
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		simulate(monitor);
		return Status.OK_STATUS;
	}

}
