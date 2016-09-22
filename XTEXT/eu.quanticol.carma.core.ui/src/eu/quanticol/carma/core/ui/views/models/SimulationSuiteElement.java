/**
 * 
 */
package eu.quanticol.carma.core.ui.views.models;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IResource;

import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.views.MeasureNode;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti
 *
 */
public class SimulationSuiteElement {
	
	private ProjectSimulationSuite parent;
	
	private SimulationExperiment experiment;
	
	private LinkedList<Object> details;
	
	public SimulationSuiteElement( ProjectSimulationSuite parent , SimulationExperiment experiment ) {
		this.parent = parent;
		this.experiment = experiment;
		computeDetails();
	}

	private void computeDetails() {
		details = new LinkedList<>();
		details.add( ExperimentDetail.modelDetail( this ) );
		details.add( ExperimentDetail.systemDetail(this) );
		details.add( ExperimentDetail.simulationTimeDetail(this));
		details.add( ExperimentDetail.replicationsDetail(this));
		details.add( ExperimentDetail.samplingsDetail(this));
		details.add( new MeasureNode(this));
	}

	public SimulationExperiment getSimulationExperiment() {
		return experiment;
	}
	
	public ProjectSimulationSuite getParent() {
		return parent;
	}

	public Object[] getElements() {
		return details.toArray();
	}

	public void setExperiment(SimulationExperiment simulationExperiment) {
		this.experiment = simulationExperiment;
		computeDetails();
	}

	public List<SimulationOutcome> getExperimentTags() {
		return experiment.getResults();
	}

	public void refresh(IResource r, CarmaModel m) {
		if (experiment.getResource().equals(r)) {
			experiment.setCarmaModel( m );
		}
	}
	
	public boolean check() {
		return experiment.check();
	}

}
