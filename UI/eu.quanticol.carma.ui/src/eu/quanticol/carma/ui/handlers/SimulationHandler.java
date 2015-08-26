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

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.ui.wizards.SimulationWizard;

/**
 * @author loreti
 *
 */
public class SimulationHandler extends AbstractHandler {

	
//	CarmaUiUtil util = new CarmaUiUtil();
//	
//	@Override
//	public Object execute(ExecutionEvent event) throws ExecutionException {
//		System.out.println("HANDLER EXECUTION!!!!!");
//		HashMap<IResource,CarmaModel> map = util.getActiveModels();
//		for (IResource r: map.keySet()) {
//			CarmaModel m = map.get(r);
//			System.out.println("FILE: "+r.getName());
//			System.out.println("SYSTEMS: ");
//			String[] systems = m.getSystems();
//			String[] measures = m.getMeasures();
//			for( int i=0 ; i<systems.length ; i++ ) {
//				System.out.println("- "+systems[i]);
//			}
//			System.out.println("MEASURES: ");
//			for( int i=0 ; i<measures.length ; i++ ) {
//				System.out.println("- "+measures[i]);
//			}
//		}
//	    return true;
//	}
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

//		System.out.println("HANDLER EXECUTION!!!!!");
//		HashMap<IResource,CarmaModel> map = util.getActiveModels();
//		for (IResource r: map.keySet()) {
//			CarmaModel m = map.get(r);
//			System.out.println("FILE: "+r.getName());
//			System.out.println("SYSTEMS: ");
//			String[] systems = m.getSystems();
//			String[] measures = m.getMeasures();
//			for( int i=0 ; i<systems.length ; i++ ) {
//				System.out.println("- "+systems[i]);
//			}
//			System.out.println("MEASURES: ");
//			for( int i=0 ; i<measures.length ; i++ ) {
//				System.out.println("- "+measures[i]);
//			}
//		}
//
		
//	    WizardDialog wizardDialog = new WizardDialog(HandlerUtil.getActiveShell(event),
//	    	      new SimulationWizard());
//	    	    if (wizardDialog.open() == Window.OK) {
//	    	      System.out.println("Ok pressed");
//	    	    } else {
//	    	      System.out.println("Cancel pressed");
//	    	      }
//
//=======
		
	    WizardDialog wizardDialog = new WizardDialog(HandlerUtil.getActiveShell(event),
	    	      new SimulationWizard());
	    	    if (wizardDialog.open() == Window.OK) {
	    	      System.out.println("Ok pressed");
	    	    } else {
	    	      System.out.println("Cancel pressed");
	    	      }
	    return true;
	}
	
}
