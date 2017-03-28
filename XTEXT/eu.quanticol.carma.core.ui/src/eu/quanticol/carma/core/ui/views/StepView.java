package eu.quanticol.carma.core.ui.views;
import java.util.ArrayList;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.RandomGeneratorRegistry;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.ResourceUtil;
import org.eclipse.xtext.ui.editor.XtextEditor;

import com.google.inject.Inject;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.core.ui.CarmaUiUtil;


public class StepView extends ViewPart {

	public static final String ID = "eu.quanticol.carma.ui.views.StepView";
	@Inject CarmaUiUtil util;
	
	private Composite container;
	private Action refreshAction;
//	private ListViewer componentsTable;
	private ListViewer processesTable;
	private Label timeLabel;
	private Text storeText;

	@Override
	public void createPartControl(Composite parent) {
		container = new Composite(parent, SWT.BORDER);
		makeActions();

		// listen for changes to editor?
		
		makeTables(container);
		makeText(container);
	    	    
		// Load the model currently in the editor
		// Very rough, needs more checks!
		retrieveSystem();
		
        refreshTables(false);
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}
	
	private void makeTables(Composite parent) {
		componentsTable = new TableViewer(parent,SWT.BORDER | SWT.V_SCROLL);
		componentsTable.setContentProvider(ArrayContentProvider.getInstance());
		processesTable.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((CarmaProcess) element).toString();
			}
		});
		
		actionsTable = new ListViewer(parent,SWT.BORDER | SWT.V_SCROLL);
		actionsTable.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((WeightedElement<Activity>) element).getElement().getName();
			}
		});
		
		
		componentsTable.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent arg0) {
				IStructuredSelection selection = componentsTable.getStructuredSelection();
				CarmaComponent selected =  (CarmaComponent) selection.getFirstElement();
					setContentAndSelectFirst(selected.getProcesses(), processesTable);
			}
		});
		processesTable.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent arg0) {
				CarmaProcess selected =  (CarmaProcess) selection.getFirstElement();
					//debug("Set input of actionsTable.");
			}
		});	
	}

	private void refreshTables(boolean reset) {
		if (system != null) {
			componentsTable.setInput(system.getCollective());
		}
	}
		
	private void refreshTime() {
		if (system == null)
			timeText.setText("0");
		else
			timeText.setText(String.format("%.5f",time));
	}
	
	private void makeButtons() {
	private void doSimulationStep() {
		if (system == null) {
			error("Please select a system for simulation.");
		}
		else {
			// if a component is not selected, pick one at random
			int componentIndex = getSelectionOrRandom(componentsTable);
			CarmaComponent c = system.getCollective().get(componentIndex);			
			// and similarly for processes and actions:
			CarmaProcess p = c.getProcesses().get(getSelectionOrRandom(processesTable));
			int actionIndex = getSelectionOrRandom(actionsTable);
			Activity a = p.getActivities(system).getAll().get(actionIndex).getElement();
			RandomGenerator r = RandomGeneratorRegistry.getInstance().get();
			
			double dt = (rate > 0) ? (1.0 / rate) * Math.log(1 / (r.nextDouble())) : 0;
			a.execute(r);
//				componentsTable.setSelection(
//						new StructuredSelection(componentsTable.getElementAt(componentIndex)),true);
			time += dt;
			refreshTime();
		}
	}
		
	private int getSelectionOrRandom(Table table) {
		if (table.getSelectionCount() != 0)
			return table.getSelectionIndex();
		else {
			return RandomGeneratorRegistry.getInstance().get().nextInt(table.getItemCount());
		}
	}
	
	private int getSelectionOrRandom(ListViewer viewer) {
		if (!viewer.getSelection().isEmpty()) {
			return viewer.getList().getSelectionIndex();
		else {
			return RandomGeneratorRegistry.getInstance().get().nextInt(viewer.getList().getItemCount());
		}
		//or just:
		//return getSelectionOrRandom(viewer.getTable());
	}
	
	private void makeText(Composite parent) {
//		if (storeText != null)
//			storeText.dispose();
		storeText = new Text(parent,SWT.MULTI | SWT.READ_ONLY | SWT.V_SCROLL);
	}
	
	private void makeTime(Composite parent) {
		if (timeLabel == null) {
			timeLabel = new Label(parent,SWT.NONE);
	}
	
	private void refreshStore() {
		// Display the store of the selected component, if any:
		CarmaComponent c = (CarmaComponent) componentsTable.getStructuredSelection().getFirstElement();
			CarmaStore store = c.getStore();
			for (String attr : store.getAttributes()) {
				sb.append(attr + " : " + store.get(attr, Object.class) + "\n");
			}
			storeText.setText(sb.toString());
	}
	
	private void retrieveSystem() {
		IEditorReference[] editors = PlatformUI.getWorkbench().getActiveWorkbenchWindow().
				getActivePage().getEditorReferences();
		IResource resource = null;
		try {
			resource = ResourceUtil.getResource(editors[0].getEditorInput());
		}
		catch (PartInitException e) {
			e.printStackTrace(System.err);
		}
		CarmaModel model = util.getActiveModels().get(resource);

//		int i = 0;
//		boolean found = false;
//		//TODO at least some checks that we have indeed found an editor
//		while (!found) {
//			if (editors[i].getEditor(false) instanceof XtextEditor)
//				found = true;
//			i++;
//		}
//		CarmaModel model = util.loadModel((XtextEditor) editors[i]);
		try {
			String systemName = model.getSystems()[0];
			system = model.getFactory(systemName).getModel();
		}
		catch (NullPointerException e) {
			system = null;
		}
	}
}