package eu.quanticol.carma.ui.views;


import java.util.ArrayList;

import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.ui.laboratory.ExperimentJob;
import eu.quanticol.carma.ui.wizards.SaveAsCSVWizard;

public class ExperimentResultsView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "eu.quanticol.carma.ui.views.ExperimentResultsView";

	private static ExperimentJob experimentJob;
	private static ArrayList<ResultViewerWidget> results;
	private static SamplingCollection<CarmaSystem> collection;
	private static int samples;
	private static String[] measures;
	private static Composite container;
	private static Composite parent;
	private Action action1;
	
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
		FillLayout parentLayout = new FillLayout();
		ExperimentResultsView.parent = parent;
		parent.setLayout(parentLayout);
		
		update(false);
		makeActions();
		contributeToActionBars();

	}
	
	private void makeActions() {
		action1 = new Action() {
			public void run() {
				startWizard();
			}
		};
		action1.setText("Save as CSV...");
		action1.setToolTipText("Save these tables to a CSV file");
		action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_ETOOL_SAVE_EDIT));
	}
	
	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}
	
	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(action1);
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
	}
	
	private void startWizard() {
		try {
			IWizard wizard = new SaveAsCSVWizard(ExperimentResultsView.experimentJob);
			WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
			dialog.open();
		} catch (NullPointerException e) {
			e.printStackTrace();
		}
	}
	
	//The experimentJob contains all the meta data
	public static void update(ExperimentJob experimentJob){
		ExperimentResultsView.experimentJob = experimentJob;
		update(experimentJob.getMeasures(), experimentJob.getCollection(), experimentJob.getSamples());
		
	}
	
	public static void update(String[] measures, SamplingCollection<CarmaSystem> collection, int samples){
		ExperimentResultsView.measures = measures;
		ExperimentResultsView.collection = collection;
		ExperimentResultsView.samples = samples;
		update(true);
		
	}
	
	public static void update(boolean flag){
		
		if(flag)
			ExperimentResultsView.container.dispose();
		GridLayout containerLayout = new GridLayout(2, false);
		ExperimentResultsView.container = new Composite(ExperimentResultsView.parent, SWT.NONE);
		container.setLayout(containerLayout);
		
		results = new ArrayList<ResultViewerWidget>();
		
		if(ExperimentResultsView.measures != null){
			for(int i = 0; i < measures.length; i++){
				results.add(new ResultViewerWidget(container, ((StatisticSampling<CarmaSystem>) collection.get(i)), samples));
			}
		}
		
		//refresh with new data
		container.layout();
		//need to refresh the parent too
		parent.layout();
	}
	

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}
}