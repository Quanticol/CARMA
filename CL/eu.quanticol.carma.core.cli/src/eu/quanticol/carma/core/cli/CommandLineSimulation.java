package eu.quanticol.carma.core.cli;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;

import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.SimulationMonitor;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;

public class CommandLineSimulation implements Callable<Object>, Runnable {
	/**
	 * Experiment name.
	 */
	private String name;

	/**
	 * Specific name for this subtask, if applicable.
	 */
	private String taskName;
	
	/**
	 * System to use in the simulation.
	 */
	private String system;
	
	/**
	 * Number of replications in the simulation
	 */
	private int replications;
	
	/**
	 * Simulation time.
	 */
	private double simulationTime;
	
	/**
	 * Number of samplings.
	 */
	private int samplings;
	
	/**
	 * Measures to collect in the simulation
	 */
	private List<MeasureData> measures;
	
	private List<SimulationOutcome> results;

	private CarmaModel model;
	
	private String modelLocation;
	
	private Long seed;
	

	public CommandLineSimulation(String name, CarmaModel model, String system, int replications,
			double simulationTime, int samplings, List<MeasureData> measures, String modelName) {
		super();
		this.name = name;
		this.model = model;
		this.system = system;
		this.replications = replications;
		this.simulationTime = simulationTime;
		this.samplings = samplings;
		this.measures = measures;
		this.results = new LinkedList<>();
		this.taskName = null;
		this.seed = null;
		this.modelLocation = modelName;
	}
	
	public CommandLineSimulation(String name, CarmaModel model, String system, int replications,
			double simulationTime, int samplings, List<MeasureData> measures) {
		this(name,model,system,replications,simulationTime,samplings,measures,"[no name]");
	}
	
	public CommandLineSimulation copy() {
		return new CommandLineSimulation(name,model,system,replications,simulationTime,samplings,
				measures,modelLocation);
	}
	
	public void addSimulationResult( SimulationOutcome result ) {
		results.add(result);
	}
	
	public void clearResults() {
		results = new LinkedList<>();
	}
	
	public void removeResult( int i ) {
		results.remove(i);
	}
	
	public SimulationOutcome getResult( int i ) {
		return results.get(i);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}

	/**
	 * @return the replications
	 */
	public int getReplications() {
		return replications;
	}

	/**
	 * @return the simulationTime
	 */
	public double getSimulationTime() {
		return simulationTime;
	}

	/**
	 * @return the samplings
	 */
	public int getSamplings() {
		return samplings;
	}


	/**
	 * @return The list of measures.
	 */
	public List<MeasureData> getMeasures() {
		return measures;
	}

	public CarmaModel getCarmaModel() {
		return model;
	}
	
	public boolean check() {
		if (model==null) {
			return false;
		}
		if (model.getFactory(system) == null) {
			return false;
		}
		return true;
	}
	
	public List<SimulationOutcome> getResults() {
		return results;
	}

	public void setCarmaModel(CarmaModel model) {
		this.model = model;
	}
	
	public String getModelLocation() {
		return modelLocation;
	}

	public void setReplications(int replications) {
		this.replications = replications;
	}
	
	public void setSimulationTime(double simulationTime) {
		this.simulationTime = simulationTime;
	}
	
	public void setTaskName(String taskName) {
		this.taskName = taskName;
	}
	
	public void setSeed(long seed) {
		this.seed = new Long(seed);
	}
	
	public void setResults(List<SimulationOutcome> results) {
		this.results = results;
	}
	
	public void execute() {
		execute(true);
	}
	
	public void execute(boolean reportProgress) {
		// set up simulation environment
		SimulationEnvironment<CarmaSystem> sim =
				new SimulationEnvironment<CarmaSystem>(model.getFactory(system));
		if (seed != null) {
			sim.seed(seed);
		}
		SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
		for(MeasureData measure : measures){
			sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(1+samplings,
					simulationTime/samplings,
					model.getMeasure(measure.getMeasureName(),measure.getParameters())));
		}
		sim.setSampling(sc);
		
		DateFormat dateFormat1 = new SimpleDateFormat("dd/MM/yyyy  HH:mm:ss");
	    String tag = dateFormat1.format(Calendar.getInstance().getTime());
		double startTime = System.currentTimeMillis();
		
		// run the simulation and collect the results
		//sim.simulate(replications, simulationTime);
	    sim.simulate(new SimulationMonitor() {
	    	private double target = simulationTime * replications;
	    	private int i = 0;
	    	private final double ratio = 0.1;
	    	private double nextStop = ratio * target;
	    	private double timeWhole = 0;
	    	
			@Override
			public void startIteration(int i) {
				if (reportProgress)
					System.out.print("Replication "+(i+1));
			}
			
			@Override
			public void update(double t) {
				if (reportProgress && timeWhole + t >= nextStop) {
					System.out.println();
					if (taskName != null) {
						System.out.print("[" + taskName + "] ");
					}
					// Note: this might not always give the right percentage,
					// if more than ratio*target has passed since the last message
					// but this should only be the case in very short runs.
					System.out.println((10 * ++i) + "% completed");
					nextStop += ratio * target;
				}
			}
			
			@Override
			public boolean isCancelled() {
				return false;
			}

			@Override
			public void endSimulation(int i) {
				timeWhole += simulationTime;
			}
			
	    }, replications, simulationTime);
	    if (reportProgress)
	    	System.out.println();
	    double endTime = System.currentTimeMillis();
	    double totalTime = endTime-startTime;
		addSimulationResult(new SimulationOutcome(tag, totalTime, totalTime/replications,
				sc.getSimulationTimeSeries(replications)));   
	}
	
	public Void call() {
		execute();
		return null;
	}
	
	public void run() {
		execute();
	}

}