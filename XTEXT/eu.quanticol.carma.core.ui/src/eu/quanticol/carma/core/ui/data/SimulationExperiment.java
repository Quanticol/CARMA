/**
 * 
 */
package eu.quanticol.carma.core.ui.data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;

import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class SimulationExperiment {

	/**
	 * Experiment name.
	 */
	private String name;

	/**
	 * Resource used to load the referenced CarmaModel
	 */
	private IResource resource;
	
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
	
	private LinkedList<SimulationOutcome> results;

	private CarmaModel model;

	public SimulationExperiment(String name, IResource resource, CarmaModel model, String system, int replications,
			double simulationTime, int samplings, List<MeasureData> measures) {
		super();
		this.name = name;
		this.resource = resource;
		this.model = model;
		this.system = system;
		this.replications = replications;
		this.simulationTime = simulationTime;
		this.samplings = samplings;
		this.measures = measures;
		this.results = new LinkedList<>();
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
	 * @return the sourceModel
	 */
	public IResource getResource() {
		return resource;
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

	public SimulationExperiment copy() {
		return new SimulationExperiment(name+" (copy)", resource, model, system, replications, simulationTime, samplings, measures);
	}
	
	public List<SimulationOutcome> getResults() {
		return results;
	}


	public void setCarmaModel(CarmaModel model) {
		this.model = model;
	}

}
