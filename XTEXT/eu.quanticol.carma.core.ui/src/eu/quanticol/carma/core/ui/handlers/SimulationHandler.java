/**
 * 
 */
package eu.quanticol.carma.core.ui.handlers;

import java.util.HashMap;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.handlers.HandlerUtil;

import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.core.ui.jobs.ExperimentJob;
import eu.quanticol.carma.core.ui.views.NewExperimentDialog;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.core.ui.wizard.SimulationWizard;

/**
 * @author loreti
 *
 */
public class SimulationHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		CarmaUiUtil util = new CarmaUiUtil();
		HashMap<IResource, CarmaModel> models = util.getActiveModels();
		if (models.size() == 0) {
			 MessageBox messageBox = new MessageBox(HandlerUtil.getActiveShell(event), SWT.ICON_ERROR | SWT.OK );
			 messageBox.setText("Error...");
	        messageBox.setMessage("Please open a CARMA specification file!");
	        messageBox.open();
			return false;
		} else {
//		    WizardDialog wizardDialog = new WizardDialog(HandlerUtil.getActiveShell(event),
//		    	      new SimulationWizard());
//		    	    if (wizardDialog.open() == Window.OK) {
//		    	      System.out.println("Ok pressed");
//		    	    } else {
//		    	      System.out.println("Cancel pressed");
//		    	      }
//    	    return true;
			NewExperimentDialog simulationDataDialog = new NewExperimentDialog(models, HandlerUtil.getActiveShell(event));
			
			if (simulationDataDialog.open()==Window.OK) {
				ExperimentJob experiment = new ExperimentJob( simulationDataDialog.getSimulationExperiment() );
				
				//update the Lab View
				//updateView();
				
				experiment.setUser(true);
				experiment.schedule();
				
				return true;
			}
			
			return false;
		
		}
		
	}
	
}
