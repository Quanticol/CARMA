/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import eu.quanticol.carma.core.ui.ExtendedCARMAActivator;
import eu.quanticol.carma.core.ui.views.models.ExperimentDetail;
import eu.quanticol.carma.core.ui.views.models.ProjectSimulationSuite;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;

/**
 * @author loreti
 *
 */
public class SimulationViewLabelProvider extends LabelProvider {

	private final Image projectSuiteImage = ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_PRJ_SUITE_ID).createImage();
	private final Image experimentElementImage = ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_EXPERIMENT_ELEMENT_ID).createImage();
	private final Image experimentElementErrorImage = ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_EXPERIMENT_ELEMENT_FAIL_ID).createImage();
	
	@Override
	public Image getImage(Object element) {
		if (element instanceof ProjectSimulationSuite) {
			return projectSuiteImage;
		}
		if (element instanceof SimulationSuiteElement) {
			
			if (((SimulationSuiteElement) element).check()) {
				return experimentElementImage;				
			} else {
				return experimentElementErrorImage;
			}
		}
		return super.getImage(element);
	}

	@Override
	public String getText(Object element) {
		if (element instanceof ProjectSimulationSuite) {
			return ((ProjectSimulationSuite) element).getProject().getName();
		}
		if (element instanceof SimulationSuiteElement) {
			return ((SimulationSuiteElement) element).getSimulationExperiment().getName();
		}
		if (element instanceof ExperimentDetail) {
			return ((ExperimentDetail) element).getInfo();
		}
		if (element instanceof MeasureNode) {
			return "Measures";
		}
		if (element instanceof MeasureDataNode) {
			return ((MeasureDataNode) element).getMd().toString();
		}
		return "MANCA";

	}

}
