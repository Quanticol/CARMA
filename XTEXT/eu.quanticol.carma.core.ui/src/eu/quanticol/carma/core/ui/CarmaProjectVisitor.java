/**
 * 
 */
package eu.quanticol.carma.core.ui;

import java.util.LinkedList;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * @author loreti
 *
 */
public class CarmaProjectVisitor implements IResourceVisitor {

	private LinkedList<IResource> carmaModels;
	
	public CarmaProjectVisitor() {
		this.carmaModels = new LinkedList<>();
	}
	
	@Override
	public boolean visit(IResource resource) throws CoreException {
		if (resource.isAccessible()&&(!resource.isHidden())&&(!resource.isDerived())) {
			//FIXME: Use a different way to collect language extension.
			if ((resource.getFileExtension()!=null)&&("carma".equals(resource.getFileExtension()))) {
				carmaModels.add(resource);
			} else {
				return true;
			}
		}
		return false;
	}

	public LinkedList<IResource> getCarmaModels() {
		return carmaModels;
	}
}
