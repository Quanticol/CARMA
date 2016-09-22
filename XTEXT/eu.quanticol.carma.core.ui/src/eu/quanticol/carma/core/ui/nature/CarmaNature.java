/**
 * 
 */
package eu.quanticol.carma.core.ui.nature;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

/**
 * @author loreti
 *
 */
public class CarmaNature implements IProjectNature {

	public final static String ID = "eu.quanticol.carma.core.ui.nature.CarmaNature";
	
	private IProject project;
	
	@Override
	public void configure() throws CoreException {
	}

	@Override
	public void deconfigure() throws CoreException {
	}

	@Override
	public IProject getProject() {
		return project;
	}

	@Override
	public void setProject(IProject project) {
		this.project = project;
	}

}
