package eu.quanticol.carma.core.ui;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.core.resources.IProject;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.wizard.IProjectCreator;

import com.google.inject.Inject;

import eu.quanticol.carma.core.ui.CarmaUiUtil;

/**
 * 
 */

/**
 * @author loreti
 *
 */
public class TestHandler extends AbstractHandler {

	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		CarmaUiUtil util = new CarmaUiUtil();
		Iterable<IProject> projects = util.getCarmaProjects();
		for (IProject iProject : projects) {
			System.out.println(util.getProjectModels(iProject));
		}
		return true;
	}

	

}
