package laboratory;


import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;

public class ExperimentJob extends Job {
	
	private int deadline;
	private int iterations;
	private int simulations;
	private String system;
	private String[] measures;
	private SimulationEnvironment<CarmaSystem> sim;
	private SamplingCollection<CarmaSystem> sc;
	private CarmaModel model;
	
	public ExperimentJob(int deadline, int iterations, int simulations, String system, String[] measures, CarmaModel model) {
		super("Simulation");
		this.deadline = deadline;
		this.iterations = iterations;
		this.simulations = simulations;
		this.system = system;
		this.measures = measures;
		this.model = model;
		
		this.sim = new SimulationEnvironment<CarmaSystem>( this.model.getFactory( this.system ) );
		this.sc = new SamplingCollection<CarmaSystem>();
		
		for(String measure : this.measures){
			this.sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(this.deadline+1, 1.0, this.model.getMeasure(measure)));
		}
		
		this.sim.setSampling(sc);
		
	}
	
	public void simulate(IProgressMonitor monitor){
		monitor.beginTask("simulation", simulations);
		for(int i = 0; i < simulations; i++){
			monitor.worked(1);
			this.sim.simulate(iterations,deadline);
		}
	}
	
	public SamplingCollection<CarmaSystem> getCollection(){
		return this.sc;
	}
	
	public String[] getMeasures(){
		return this.measures;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		simulate(monitor);
		return Status.OK_STATUS;
	}

}
