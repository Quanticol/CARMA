package eu.quanticol.carma.ui.views;


import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.*;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.SWT;

import eu.quanticol.carma.simulator.CarmaSystem;


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

public class ExperimentResultsView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "eu.quanticol.carma.ui.views.ExperimentResultsView";

	private static ArrayList<ResultViewerWidget> results;
	private static SamplingCollection<CarmaSystem> collection;
	private static int samples;
	private static String[] measures;
	private static Composite parent;
	
	/**
	 * The constructor.
	 */
	public ExperimentResultsView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialise it.
	 */
	public void createPartControl(Composite parent) {
		GridLayout layout = new GridLayout(2, false);
		parent.setLayout(layout);
		ExperimentResultsView.parent = parent;
		results = new ArrayList<ResultViewerWidget>();
		
		if(measures != null){
			for(int i = 0; i < measures.length; i++){
				results.add(new ResultViewerWidget(parent, ((StatisticSampling<CarmaSystem>) collection.get(i)), samples));
			}
		}

	}
	
	public static void update(){
		if(results != null){
			results = new ArrayList<ResultViewerWidget>();
		}
		
		if(ExperimentResultsView.measures != null){
			for(int i = 0; i < measures.length; i++){
				results.add(new ResultViewerWidget(parent, ((StatisticSampling<CarmaSystem>) collection.get(i)), samples));
			}
		}
		
		parent.layout();
	}
	
	public static void update(String[] measures, SamplingCollection<CarmaSystem> collection, int samples){
		ExperimentResultsView.measures = measures;
		ExperimentResultsView.collection = collection;
		ExperimentResultsView.samples = samples;
		update();
		
	}
	

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}
}