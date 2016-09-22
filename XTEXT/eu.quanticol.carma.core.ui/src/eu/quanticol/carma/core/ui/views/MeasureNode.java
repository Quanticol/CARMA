/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.LinkedList;

import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.views.models.ExperimentDetail;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.core.ui.views.models.ExperimentDetail.DetailType;

/**
 * @author loreti
 *
 */
public class MeasureNode {
	
	private SimulationSuiteElement parent;
	
	private LinkedList<MeasureDataNode> elements;
	
	public MeasureNode( SimulationSuiteElement parent ) {
		this.parent = parent;
		this.elements = new LinkedList<>();
		for( MeasureData md: parent.getSimulationExperiment().getMeasures() ) {
			this.elements.add( new MeasureDataNode(this,md));
		}
	}

	public Object[] getElements() {
		return elements.toArray();
	}
	
	public SimulationSuiteElement getParent() {
		return parent;
	}


}
