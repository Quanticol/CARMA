/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import eu.quanticol.carma.core.ui.views.models.ExperimentDetail;
import eu.quanticol.carma.core.ui.views.models.ProjectSimulationSuite;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.core.ui.views.models.WorkspaceSimulationSuite;

/**
 * @author loreti
 *
 */
public class SimulationViewContentProvider implements ITreeContentProvider {
	
	public SimulationViewContentProvider( ) {
		
	}
	
	@Override
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof WorkspaceSimulationSuite) {
			return ((WorkspaceSimulationSuite) inputElement).getElements();
		} else {
			return new Object[] {};
		}
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof ProjectSimulationSuite) {
			return ((ProjectSimulationSuite) parentElement).getElements();
		}
		if (parentElement instanceof SimulationSuiteElement) {
			return ((SimulationSuiteElement) parentElement).getElements();
		}
		if (parentElement instanceof MeasureNode) {
			return ((MeasureNode) parentElement).getElements();
		}
		return null;
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof SimulationSuiteElement) {
			return ((SimulationSuiteElement) element).getParent();
		}
		if (element instanceof ExperimentDetail) {
			return ((ExperimentDetail) element).getParent();
		}
		if (element instanceof MeasureNode) {
			return ((MeasureNode) element).getParent();
		}
		if (element instanceof MeasureDataNode) {
			return ((MeasureDataNode) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof ProjectSimulationSuite) {
			return true;
		}
		if (element instanceof SimulationSuiteElement) {
			return true;
		}
		if (element instanceof MeasureNode) {
			return true;
		}
		return false;
	}

}
