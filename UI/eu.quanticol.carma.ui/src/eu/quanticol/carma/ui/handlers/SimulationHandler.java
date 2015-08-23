/**
 * 
 */
package eu.quanticol.carma.ui.handlers;

import java.util.HashMap;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IResource;

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti
 *
 */
public class SimulationHandler extends AbstractHandler {

	
	CarmaUiUtil util = new CarmaUiUtil();
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		System.out.println("HANDLER EXECUTION!!!!!");
		HashMap<IResource,CarmaModel> map = util.getActiveModels();
		for (IResource r: map.keySet()) {
			CarmaModel m = map.get(r);
			System.out.println("FILE: "+r.getName());
			System.out.println("SYSTEMS: ");
			String[] systems = m.getSystems();
			String[] measures = m.getMeasures();
			for( int i=0 ; i<systems.length ; i++ ) {
				System.out.println("- "+systems[i]);
			}
			System.out.println("MEASURES: ");
			for( int i=0 ; i<measures.length ; i++ ) {
				System.out.println("- "+measures[i]);
			}
		}
	    return true;
	}
	
}
