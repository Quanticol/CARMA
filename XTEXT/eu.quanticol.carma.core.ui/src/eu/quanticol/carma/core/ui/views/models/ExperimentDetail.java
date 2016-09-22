/**
 * 
 */
package eu.quanticol.carma.core.ui.views.models;

/**
 * @author loreti
 *
 */
public class ExperimentDetail {
	
	public static enum DetailType {		
		MODEL ,
		SIMULATIONTIME ,
		REPLICATIONS ,
		SAMPLINGS ,
		SYSTEM;				
	}
	
	private SimulationSuiteElement parent;
	
	private DetailType type;
	
	private String info;
	
	public ExperimentDetail( SimulationSuiteElement parent, DetailType type , String info ) {
		this.type = type;
		this.info = info;
		this.parent = parent;
	}

	public DetailType getType() {
		return type;
	}

	public String getInfo() {
		return getLabel()+" "+info;
	}

	public static ExperimentDetail modelDetail(SimulationSuiteElement simulationSuiteElement) {
		return new ExperimentDetail(
			simulationSuiteElement , 
			DetailType.MODEL ,
			simulationSuiteElement.getSimulationExperiment().getResource().getName()
		);
	}

	public static ExperimentDetail systemDetail(SimulationSuiteElement simulationSuiteElement) {
		return new ExperimentDetail(
			simulationSuiteElement , 
			DetailType.SYSTEM ,
			simulationSuiteElement.getSimulationExperiment().getSystem()
		);
	}

	public static ExperimentDetail simulationTimeDetail(SimulationSuiteElement simulationSuiteElement) {
		return new ExperimentDetail(
			simulationSuiteElement , 
			DetailType.SIMULATIONTIME ,
			simulationSuiteElement.getSimulationExperiment().getSimulationTime()+""
		);
	}

	public static ExperimentDetail replicationsDetail(SimulationSuiteElement simulationSuiteElement) {
		return new ExperimentDetail(
			simulationSuiteElement , 
			DetailType.REPLICATIONS ,
			simulationSuiteElement.getSimulationExperiment().getReplications()+""
		);
	}

	public static ExperimentDetail samplingsDetail(SimulationSuiteElement simulationSuiteElement) {
		return new ExperimentDetail(
			simulationSuiteElement , 
			DetailType.SAMPLINGS ,
			simulationSuiteElement.getSimulationExperiment().getSamplings()+""
		);
	}
	
	public SimulationSuiteElement getParent() {
		return parent;
	}
	
	private String getLabel() {
		switch (this.type) {
		case MODEL:
			return "Model:";
		case REPLICATIONS:
			return "Number of replications:";
		case SAMPLINGS:
			return "Number of samplings:";
		case SIMULATIONTIME:
			return "Simulation time:";
		case SYSTEM:
			return "System:";
		default:
			return "Unknown";
		}
	}
}
