/**
 * 
 */
package eu.quanticol.carma.core.ui.views.models;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.LinkedList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.core.ui.views.SimulationTrace;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti
 *
 */
public class ProjectSimulationSuite {
	
	private IProject project;
	
	private LinkedList<SimulationSuiteElement> experiments;
	
	
	public ProjectSimulationSuite( IProject project , LinkedList<SimulationExperiment> experiments ) {
		this.project = project;
		init( experiments );
	}
	
	private void init(LinkedList<SimulationExperiment> experiments) {
		
		this.experiments = new LinkedList<>();
		for (SimulationExperiment se : experiments) {
			this.experiments.add(new SimulationSuiteElement(this, se));
		}
				
	}
	
	public Object[] getElements() {
		return experiments.toArray();
	}

	public IProject getProject() {
		return project;
	}

	public void add(SimulationExperiment simulationExperiment) {
		experiments.add(new SimulationSuiteElement(this,simulationExperiment));
	}

	public void remove(SimulationSuiteElement simulationSuiteElement) {
		experiments.remove(simulationSuiteElement);
	}

	public Iterable<? extends SimulationSuiteElement> getSimulationSuiteElements() {
		return experiments;
	}

	public void refreshResource(IResource r, CarmaModel m) {
		for (SimulationSuiteElement simulationSuiteElement : experiments) {
			simulationSuiteElement.refresh( r , m );
		}
	}
	
}
