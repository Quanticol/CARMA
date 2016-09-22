/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import eu.quanticol.carma.core.ui.data.SimulationOutcome;

/**
 * @author loreti
 *
 */
public class SimulationResultContentProvider implements IStructuredContentProvider {

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
	 */
	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
	 */
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub

	}

	@Override
	public Object[] getElements(Object inputElement) {
        if (inputElement instanceof Object[]) {
			return (Object[]) inputElement;
		}
        if (inputElement instanceof Collection) {
			return ((Collection) inputElement).toArray();
		}
        if (inputElement instanceof SimulationOutcome) {
        	SimulationOutcome simulationOutcome = (SimulationOutcome) inputElement;
        	return new String[] { simulationOutcome.getStartingTime() , simulationOutcome.getTotalTime()+"ms" , simulationOutcome.getAverageTime()+"ms" };
        }
        return new Object[0];
	}

}
