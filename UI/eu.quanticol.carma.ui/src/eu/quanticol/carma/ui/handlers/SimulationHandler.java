/**
 * 
 */
package eu.quanticol.carma.ui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.handlers.HandlerUtil;

import eu.quanticol.carma.ui.wizards.SimulationWizard;

/**
 * @author loreti
 *
 */
public class SimulationHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
//	    WizardDialog wizardDialog = new WizardDialog(HandlerUtil.getActiveShell(event),
//	    	      new SimulationWizard());
//	    	    if (wizardDialog.open() == Window.OK) {
//	    	      System.out.println("Ok pressed");
//	    	    } else {
//	    	      System.out.println("Cancel pressed");
//	    	      }
	    return true;
	}
	
}
