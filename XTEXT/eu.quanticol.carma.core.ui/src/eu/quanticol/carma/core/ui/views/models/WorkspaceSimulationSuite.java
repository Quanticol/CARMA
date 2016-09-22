/**
 * 
 */
package eu.quanticol.carma.core.ui.views.models;

import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.views.SimulationTrace;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti
 *
 */
public class WorkspaceSimulationSuite {

	private LinkedList<ProjectSimulationSuite> suites;

	public WorkspaceSimulationSuite( LinkedList<ProjectSimulationSuite> suites ) {
		this.suites = suites;
	}
	
	public int size() {
		return suites.size();
	}
	
	public ProjectSimulationSuite get( int i ) {
		return suites.get(i);
	}

	public Object[] getElements() {
		return suites.toArray();
	}
	
	public LinkedList<ProjectSimulationSuite> getProjectSuites() {
		return suites;
	}

	public void addProjectSimulationSuite(ProjectSimulationSuite suite) {
		suites.add(suite);
	}

	public boolean removeProject(IProject project) {
		return suites.removeIf( (ProjectSimulationSuite ps) -> ps.getProject().equals(project));
	}

	public void doRefreshResource(IProject p, IResource r, CarmaModel m) {
		for (ProjectSimulationSuite suite : suites) {
			if (suite.getProject().equals(p)) {
				suite.refreshResource( r , m );
			}
		}
	}

	public void addExperiment(SimulationExperiment experiment) {
		Iterator<ProjectSimulationSuite> iterator = suites.iterator();
		IProject project = experiment.getResource().getProject();
		boolean found = false;
		while (iterator.hasNext()&&!found) {
			ProjectSimulationSuite suite = iterator.next();
			if (suite.getProject().equals(project)) {
				found = true;
				suite.add(experiment);
			}
		}
	}

}
