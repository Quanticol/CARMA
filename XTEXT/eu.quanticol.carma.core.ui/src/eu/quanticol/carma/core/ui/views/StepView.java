package eu.quanticol.carma.core.ui.views;
import java.util.ArrayList;import java.util.List;import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.RandomGeneratorRegistry;
import org.cmg.ml.sam.sim.util.WeightedElement;import org.eclipse.core.resources.IResource;import org.eclipse.jface.action.Action;import org.eclipse.jface.resource.FontRegistry;import org.eclipse.jface.viewers.ArrayContentProvider;import org.eclipse.jface.viewers.CellLabelProvider;import org.eclipse.jface.viewers.ColumnLabelProvider;import org.eclipse.jface.viewers.DecoratingLabelProvider;import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider;import org.eclipse.jface.viewers.IColorProvider;import org.eclipse.jface.viewers.ILabelDecorator;import org.eclipse.jface.viewers.ILabelProviderListener;import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;import org.eclipse.jface.viewers.ITableColorProvider;import org.eclipse.jface.viewers.ITableFontProvider;import org.eclipse.jface.viewers.ITableLabelProvider;import org.eclipse.jface.viewers.LabelProvider;import org.eclipse.jface.viewers.ListViewer;import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;import org.eclipse.jface.viewers.StyledCellLabelProvider;import org.eclipse.jface.viewers.StyledString;import org.eclipse.jface.viewers.StyledString.Styler;import org.eclipse.jface.viewers.TableViewer;import org.eclipse.jface.viewers.TableViewerColumn;import org.eclipse.jface.viewers.Viewer;import org.eclipse.jface.viewers.ViewerCell;import org.eclipse.jface.viewers.ViewerFilter;import org.eclipse.swt.SWT;import org.eclipse.swt.custom.StyleRange;import org.eclipse.swt.graphics.Color;import org.eclipse.swt.graphics.Font;import org.eclipse.swt.graphics.Image;import org.eclipse.swt.graphics.TextStyle;import org.eclipse.swt.layout.GridData;import org.eclipse.swt.layout.GridLayout;import org.eclipse.swt.layout.RowData;import org.eclipse.swt.layout.RowLayout;import org.eclipse.swt.widgets.Composite;import org.eclipse.swt.widgets.Display;import org.eclipse.swt.widgets.Group;import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.ResourceUtil;import org.eclipse.ui.model.WorkbenchLabelProvider;import org.eclipse.ui.part.ViewPart;
import org.eclipse.xtext.ui.editor.XtextEditor;

import com.google.inject.Inject;

import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaProcess;
import eu.quanticol.carma.simulator.CarmaStore;
import eu.quanticol.carma.simulator.CarmaSystem;
import eu.quanticol.carma.core.ui.CarmaUiUtil;import eu.quanticol.carma.core.ui.ExtendedCARMAActivator;


public class StepView extends ViewPart {

	public static final String ID = "eu.quanticol.carma.ui.views.StepView";
	@Inject CarmaUiUtil util;
	
	private Composite container;
	private Action refreshAction;	private Action simulateAction;	private ListViewer actionsTable;
//	private ListViewer componentsTable;	private TableViewer componentsTable;
	private ListViewer processesTable;//	private Label componentsLabel;//	private Label processesLabel;//	private Label actionsLabel;
	private Label timeLabel;	private Text timeText;
	private Text storeText;	private Text globalStoreText;	private Text historyText;
	private CarmaSystem system;	private double time = 0;	
	@Override
	public void createPartControl(Composite parent) {
		container = new Composite(parent, SWT.BORDER);		//container.setLayout(new FillLayout(SWT.HORIZONTAL));		container.setLayout(new GridLayout(4,true));
		makeActions();		makeButtons();

		// listen for changes to editor?
				makeTime(container);		makeLabels(container);
		makeTables(container);		makeHistory(container);
		makeText(container);//		Group historyGroup = new Group(parent,SWT.NONE);//		historyGroup.setLayout(new GridLayout(1,true));//		makeHistory(historyGroup);		
	    	    
		// Load the model currently in the editor
		// Very rough, needs more checks!
		retrieveSystem();
		
        refreshTables(false);
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}
		private void makeLabels(Composite parent) {		Label componentsLabel = new Label(parent,SWT.NONE);		componentsLabel.setText("Components");		componentsLabel.setLayoutData(new GridData());				Label processesLabel = new Label(parent,SWT.NONE);		processesLabel.setText("Processes");				Label actionsLabel = new Label(parent,SWT.NONE);		actionsLabel.setText("Actions");				Label historyLabel = new Label(parent,SWT.NONE);		historyLabel.setText("History");		}	
	private void makeTables(Composite parent) {
		componentsTable = new TableViewer(parent,SWT.BORDER | SWT.V_SCROLL);
		componentsTable.setContentProvider(ArrayContentProvider.getInstance());		TableViewerColumn col = new TableViewerColumn(componentsTable, SWT.NONE);		col.getColumn().setText("ComponentName");		col.getColumn().pack(); //necessary, otherwise column does not appear (or use setWidth)		// for styled text, we can use something like:		//col.setLabelProvider(new DelegatingStyledCellLabelProvider(new MyDelegatingProvider()));		// otherwise, this shows unformatted text:		col.setLabelProvider(new ColumnLabelProvider() {			@Override			public String getText(Object element) {				return ((CarmaComponent) element).getName();			}		});		// only show components that are not killed		// not actually needed? (killed components are removed from the system		// after every actions)		componentsTable.addFilter(new ViewerFilter() {			@Override			public boolean select(Viewer viewer, Object parent, Object element) {				return !((CarmaComponent) element).isKilled();			}		});		GridData componentsData = new GridData(SWT.FILL,SWT.FILL,true,true);		componentsData.heightHint = 100;		componentsTable.getControl().setLayoutData(componentsData);				processesTable = new ListViewer(parent,SWT.BORDER | SWT.V_SCROLL);		processesTable.setContentProvider(ArrayContentProvider.getInstance());
		processesTable.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((CarmaProcess) element).toString();
			}
		});		GridData processesData = new GridData(SWT.FILL,SWT.FILL,true,true);		processesData.heightHint = 100;		processesTable.getControl().setLayoutData(processesData);
		
		actionsTable = new ListViewer(parent,SWT.BORDER | SWT.V_SCROLL);		actionsTable.setContentProvider(ArrayContentProvider.getInstance());
		actionsTable.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((WeightedElement<Activity>) element).getElement().getName();
			}
		});		GridData actionsData = new GridData(SWT.FILL,SWT.FILL,true,true);		actionsData.heightHint = 100;		actionsTable.getControl().setLayoutData(actionsData);
		
		
		componentsTable.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent arg0) {
				IStructuredSelection selection = componentsTable.getStructuredSelection();
				CarmaComponent selected =  (CarmaComponent) selection.getFirstElement();				if (selected != null) {					actionsTable.setInput(null);
					setContentAndSelectFirst(selected.getProcesses(), processesTable);					//debug("Set input of processesTable.");					refreshStore();				}//				else {//					processesTable.setInput(StructuredSelection.EMPTY);//					actionsTable.setInput(StructuredSelection.EMPTY);//				}
			}
		});
		processesTable.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent arg0) {				IStructuredSelection selection = processesTable.getStructuredSelection();
				CarmaProcess selected =  (CarmaProcess) selection.getFirstElement();				if (selected != null) {					setContentAndSelectFirst(selected.getActivities(system).getAll(),actionsTable);
					//debug("Set input of actionsTable.");				}//				else {//					actionsTable.setInput(null);//				}
			}
		});	
	}

	private void refreshTables(boolean reset) {
		if (system != null) {			//componentsTable.getTable().setFocus();
			componentsTable.setInput(system.getCollective());			if (reset) {				processesTable.setInput(null);				actionsTable.setInput(null);				storeText.setText("");				globalStoreText.setText("");			}			//componentsTable.refresh();			//debug("Set the input of componentsTable.");
		}
	}
		
	private void refreshTime() {
		if (system == null)
			timeText.setText("0");
		else
			timeText.setText(String.format("%.5f",time));
	}
	
	private void makeButtons() {		IActionBars bar = getViewSite().getActionBars();		bar.getToolBarManager().add(refreshAction);		bar.getToolBarManager().add(simulateAction);	}	private void makeActions() {		refreshAction = new Action() {			public void run() {				retrieveSystem();				refreshTables(true);				time = 0;				refreshTime();			}		};		refreshAction.setImageDescriptor(				ExtendedCARMAActivator.getInstance().getImageRegistry()					.getDescriptor(ExtendedCARMAActivator.IMG_REFRESH_ID));		refreshAction.setText("Refresh");		refreshAction.setToolTipText("Refresh the model");				simulateAction = new Action() {			public void run() {				doSimulationStep();			}		};		simulateAction.setImageDescriptor(				ExtendedCARMAActivator.getInstance().getImageRegistry()					.getDescriptor(ExtendedCARMAActivator.IMG_RUN_EXERIMENT_ID));		simulateAction.setText("Simulation Step");		simulateAction.setToolTipText("Take a simulation step");	}	
	private void doSimulationStep() {
		if (system == null) {
			error("Please select a system for simulation.");
		}
		else {			CarmaComponent c = (CarmaComponent) componentsTable.getStructuredSelection().getFirstElement();			if (c == null) {				// pick one randomly				double max = 0;				for (CarmaComponent comp : system.getCollective()) {					if (comp.getActivities(system).getTotalWeight() > max) {						max = comp.getActivities(system).getTotalWeight();						c = comp;					}				}				if (max == 0) {					error("All components have 0 weight. Simulation cannot proceed.");					return;				}			}			else {				if (c.getActivities(system).getTotalWeight() == 0) {					error("The chosen component has 0 weight. Simulation cannot proceed.");					return;				}			}						// Choose a process (as selected, or by weight)			CarmaProcess p = (CarmaProcess) processesTable.getStructuredSelection().getFirstElement();			if (p == null) {				double max = 0;				for (CarmaProcess proc : c.getProcesses()) {					if (proc.getActivities(system).getTotalWeight() > max) {						max = proc.getActivities(system).getTotalWeight();						p = proc;					}				}				if (max == 0) {					error("All processess have 0 weight. Choose another component.");					return;				}			}			else {				if (p.getActivities(system).getTotalWeight() == 0) {					error("The chosen process has 0 weight. Simulation cannot proceed.");					return;				}			}						WeightedElement<Activity> we = ((WeightedElement<Activity>) actionsTable.					getStructuredSelection().getFirstElement()); 			if (we == null) {				double max = 0;				for (WeightedElement<Activity> el : p.getActivities(system).getAll()) {					if (el.getTotalWeight() > max) {						max = el.getTotalWeight();						we = el;					}				}				if (max == 0) {					MessageBox msgbox = new MessageBox(getViewSite().getShell(),SWT.ICON_ERROR);					msgbox.setMessage("All actions have 0 weight. Choose another process.");					msgbox.open();					return;				}			}			Activity a = we.getElement();			double rate = we.getTotalWeight();						/*
			// if a component is not selected, pick one at random
			int componentIndex = getSelectionOrRandom(componentsTable);
			CarmaComponent c = system.getCollective().get(componentIndex);						
			// and similarly for processes and actions:
			CarmaProcess p = c.getProcesses().get(getSelectionOrRandom(processesTable));
			int actionIndex = getSelectionOrRandom(actionsTable);
			Activity a = p.getActivities(system).getAll().get(actionIndex).getElement();
			RandomGenerator r = RandomGeneratorRegistry.getInstance().get();			// sample a time for this step			//double rate = p.getActivities(system).getAll().get(actionIndex).getTotalWeight();			*/
						RandomGenerator r = RandomGeneratorRegistry.getInstance().get();
			double dt = (rate > 0) ? (1.0 / rate) * Math.log(1 / (r.nextDouble())) : 0;
			a.execute(r);			writeToHistory(c,p,a);			//debug("Executing " + a.getName());			if (c.isKilled()) {				refreshTables(true);			}			else {				refreshStore();				refreshTables(false);				componentsTable.setSelection(StructuredSelection.EMPTY);
//				componentsTable.setSelection(
//						new StructuredSelection(componentsTable.getElementAt(componentIndex)),true);				processesTable.setSelection(StructuredSelection.EMPTY);				actionsTable.setSelection(StructuredSelection.EMPTY);			}			refreshStore();
			time += dt;
			refreshTime();
		}
	}	private void error(String msg) {		MessageBox msgbox = new MessageBox(getViewSite().getShell(),SWT.ICON_ERROR);		msgbox.setText("Error!");		msgbox.setMessage(msg);		msgbox.open();	}
			private void writeToHistory(CarmaComponent c, CarmaProcess p, Activity a) {		historyText.append("Action " + a.getName() +				" of process " + p.toString() +				" on component " +	c.getName() + ".\n");	}	
	private int getSelectionOrRandom(Table table) {
		if (table.getSelectionCount() != 0)
			return table.getSelectionIndex();
		else {
			return RandomGeneratorRegistry.getInstance().get().nextInt(table.getItemCount());
		}
	}
	
	private int getSelectionOrRandom(ListViewer viewer) {
		if (!viewer.getSelection().isEmpty()) {
			return viewer.getList().getSelectionIndex();		}
		else {
			return RandomGeneratorRegistry.getInstance().get().nextInt(viewer.getList().getItemCount());
		}
		//or just:
		//return getSelectionOrRandom(viewer.getTable());
	}
	
	private void makeText(Composite parent) {
//		if (storeText != null)
//			storeText.dispose();
		storeText = new Text(parent,SWT.MULTI | SWT.READ_ONLY | SWT.V_SCROLL);		GridData textGridData = new GridData(SWT.FILL,SWT.FILL,true,true);		textGridData.horizontalSpan = 2;		textGridData.heightHint = 100;		storeText.setLayoutData(textGridData);				globalStoreText = new Text(parent,SWT.MULTI | SWT.READ_ONLY | SWT.V_SCROLL);		GridData globalStoreTextGridData = new GridData(SWT.FILL,SWT.FILL,true,true);		globalStoreTextGridData.horizontalSpan = 1;		globalStoreTextGridData.heightHint = 100;		globalStoreText.setLayoutData(globalStoreTextGridData);
	}
	
	private void makeTime(Composite parent) {
		if (timeLabel == null) {
			timeLabel = new Label(parent,SWT.NONE);			timeLabel.setText("Time:");			GridData timeLabelGridData = new GridData(SWT.RIGHT,SWT.CENTER,false,false);			timeLabelGridData.horizontalSpan = 2;			timeLabel.setLayoutData(timeLabelGridData);		}		if (timeText == null) {			timeText = new Text(parent,SWT.READ_ONLY);			timeText.setText("");			GridData timeTextGridData = new GridData();			timeTextGridData.horizontalAlignment = SWT.FILL;			timeTextGridData.horizontalSpan = 2;			timeText.setLayoutData(timeTextGridData);		}
	}		private void makeHistory(Composite parent) {		historyText = new Text(parent,SWT.MULTI | SWT.V_SCROLL);		historyText.setText("");		GridData historyData = new GridData(SWT.FILL,SWT.FILL,true,true);		historyData.heightHint = 100;		historyData.verticalSpan = 3;		historyText.setLayoutData(historyData);	}
	
	private void refreshStore() {		StringBuilder sb = new StringBuilder();
		// Display the store of the selected component, if any:
		CarmaComponent c = (CarmaComponent) componentsTable.getStructuredSelection().getFirstElement();		if (c != null) {			sb.append(c.getName());			if (c.getLocation() != null) {				sb.append(" at location " + c.getLocation().getName());			}			sb.append(":\n");
			CarmaStore store = c.getStore();
			for (String attr : store.getAttributes()) {
				sb.append(attr + " : " + store.get(attr, Object.class) + "\n");
			}
			storeText.setText(sb.toString());		}		// And similarly for the global store:		sb = new StringBuilder();		sb.append("Global:\n");		CarmaStore global = system.getGlobalStore();		for (String attr : global.getAttributes()) {			sb.append(attr + " : " + global.get(attr, Object.class) + "\n");		}		globalStoreText.setText(sb.toString());
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
		}//		if (system != null)//			debug("Got system with " + system.getCollective().size() + " components.");//		else//			debug("System is null.");
	}		private void debug(String msg) {		System.err.println(msg);	}		private void setContentAndSelectFirst(Object content, ListViewer viewer) {		viewer.setInput(content);//		Uncomment if we want to select the first element in the viewer (may be more//		convenient for user, but doesn't allow for random selection of actions)//		Object firstElement = viewer.getElementAt(0);//		if (firstElement != null) {//			viewer.setSelection(new StructuredSelection(firstElement),true);	//		}	}			// To be used for styling TableViewer cell contents//	private class MyDelegatingProvider//		implements DelegatingStyledCellLabelProvider.IStyledLabelProvider{////		@Override//		public void addListener(ILabelProviderListener arg0) {//		}////		@Override//		public void dispose() {//			// TODO Auto-generated method stub//		}////		@Override//		public boolean isLabelProperty(Object arg0, String arg1) {//			return false;//		}////		@Override//		public void removeListener(ILabelProviderListener arg0) {//		}////		@Override//		public Image getImage(Object arg0) {//			return null;//		}////		@Override//		public StyledString getStyledText(Object element) {//			String text = ((CarmaComponent) element).getName();//			StyledString styled = new StyledString(text);//			Styler styler = new Styler() {//				@Override//				public void applyStyles(TextStyle textStyle) {//					textStyle.strikeout = true;//					textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_RED);//				}//			};//			styled.setStyle(0, styled.length(), styler);//			return styled;//		}//////		@Override////		public void update(ViewerCell cell) {////			StyledString styled = getStyledText(cell.getElement());////			cell.setText(styled.getString());////			cell.setStyleRanges(styled.getStyleRanges());////		}//	}		// Another attempt for styling TableViewer contents (does not work?)//	private class MyStyledLabelProvider extends StyledCellLabelProvider {//		public void update(ViewerCell cell) {//			debug("Called LabelProvider.update()");//			String text = ((CarmaComponent) cell.getElement()).getName() + "!";//			cell.setText(text);//			//List<StyleRange> styleRanges = new ArrayList<StyleRange>();//			StyleRange range = new StyleRange();//			range.strikeout = true;//			range.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_RED);//			range.start = 0;//			range.length = text.length();//			//styleRanges.add(new StyleRange(style));//			StyleRange[] styleRanges = {new StyleRange(range)};//			cell.setStyleRanges(styleRanges);//			super.update(cell);//		}//	}	
}
