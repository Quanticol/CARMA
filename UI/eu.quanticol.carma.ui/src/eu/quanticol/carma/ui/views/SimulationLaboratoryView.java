package eu.quanticol.carma.ui.views;


import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.*;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.SWT;

import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.ui.laboratory.ExperimentJob;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class SimulationLaboratoryView extends ViewPart {
	
	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "eu.quanticol.carma.ui.views.SimulationLaboratoryView";
	
	private static ArrayList<ExperimentJobWidget> results;
	private static Composite parent;

	/**
	 * The constructor.
	 */
	public SimulationLaboratoryView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		FillLayout parentLayout = new FillLayout();
		parentLayout.type = SWT.VERTICAL;
		SimulationLaboratoryView.parent = parent;
		parent.setLayout(parentLayout);
		
		update(null);
	}
	
	public static void update(ExperimentJob experimentJob){
		
		ArrayList<ExperimentJobWidget> neu = new ArrayList<ExperimentJobWidget>();
		
		if(results != null)
			for(ExperimentJobWidget w : results){
				neu.add(w);
			}
		
		if(experimentJob != null)
			neu.add(new ExperimentJobWidget(parent, experimentJob));

		
		results = neu;
		
		//need to refresh the parent
		parent.layout();
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}

}