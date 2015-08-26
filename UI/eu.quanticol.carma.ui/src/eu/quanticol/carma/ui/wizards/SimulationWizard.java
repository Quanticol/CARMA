/**
 * 
 */
package eu.quanticol.carma.ui.wizards;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti, williams
 *
 */
public class SimulationWizard extends Wizard {

	ClassLoader parentClassLoader;
	URLClassLoader classLoader;
	Class<?> clazz;
	Object carmaModel;
	
	private ArrayList<WizardPage> wizardPageList = new ArrayList<WizardPage>();
	
	protected SelectModelPage modelPage;
	protected SelectSystemPage systemPage;
	
	protected int modelChoice = -1;
	protected HashMap<IResource, CarmaModel> models;
	protected LinkedList<CarmaModel> carmaModels;
	
	protected CarmaUiUtil util = new CarmaUiUtil();
	
	public SimulationWizard() {
		super();
		this.models = util.getActiveModels();
		setNeedsProgressMonitor(true);
	}
	
	@Override
	public void addPages() {
		modelPage = new SelectModelPage();
		addPage(modelPage);
		wizardPageList.add(modelPage);
		systemPage = new SelectSystemPage();
		addPage(systemPage);
		wizardPageList.add(systemPage);
	}
	
	public IWizardPage getNextPage(IWizardPage page){
		if(page == modelPage){
			systemPage = new SelectSystemPage();
			addPage(systemPage);
			return systemPage;
		}
		
		return super.getNextPage(null);
	}

	@Override
	public boolean performFinish() {
		return false;
	}
	
	@Override
	public boolean canFinish(){
		
		boolean finished = true;
		
		for(WizardPage w : this.wizardPageList){
			finished = finished && w.isPageComplete();
		}
		
		return finished;
		
	}
	
	
	public class SelectModelPage extends WizardPage {
		
		private Composite container;

		public SelectModelPage() {
			super("Select Model");
			setTitle("Select Model");
			setDescription("Select Model");
		}

		@Override
		public void createControl(Composite parent) {
			this.container = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout();
		    container.setLayout(layout);
		    layout.numColumns = 2;
		    Label label1 = new Label(container, SWT.NONE);
		    label1.setText("Model:");
		    checkPage();
		    Combo combo = new Combo(container, SWT.NONE);
		    carmaModels = new LinkedList<CarmaModel>();
		    for (IResource model : models.keySet()) {
				combo.add(model.getName());
				carmaModels.add(models.get(model));
			}
		    combo.addListener(SWT.Selection, new Listener() {
				public void handleEvent(Event event) {
					modelChoice = combo.getSelectionIndex();
					checkPage();
				}
			});
		    setControl(container);
			    
public class SimulationWizard extends Wizard {

	CarmaUiUtil util = new CarmaUiUtil();
	
	private ArrayList<WizardPage> wizardPageList = new ArrayList<WizardPage>();
	
	protected SelectModelAndSystemPage modelAndSystemPage;
	protected SelectMeasuresPage measuresPage;
	protected SetDeadlineAndIterations deadlineAndIterations;
	
	protected int modelChoice = -1;
	protected int systemChoice = -1;
	HashMap<IResource,CarmaModel> models;
	ArrayList<IResource> resources = new ArrayList<IResource>();
	
	public SimulationWizard() {
		super();
		this.models = util.getActiveModels();
		this.resources.addAll(models.keySet());
		setNeedsProgressMonitor(true);
	}
	
	@Override
	public void addPages() {
		
		modelAndSystemPage = new SelectModelAndSystemPage();
		addPage(modelAndSystemPage);
		wizardPageList.add(modelAndSystemPage);
		
		measuresPage = new SelectMeasuresPage();
		addPage(measuresPage);
		wizardPageList.add(measuresPage);
		
		deadlineAndIterations = new SetDeadlineAndIterations();
		addPage(deadlineAndIterations);
		wizardPageList.add(deadlineAndIterations);
	}

	@Override
	public boolean performFinish() {
		
		ExperimentJob experiment = new ExperimentJob(this.deadlineAndIterations.getDeadline(), 
				this.deadlineAndIterations.getIterations(), 
				this.deadlineAndIterations.getSimulations(), 
				this.modelAndSystemPage.getSystem(), 
				this.measuresPage.getMeasures(), 
				this.modelAndSystemPage.getModel());
		
		experiment.setUser(true);
		experiment.schedule();
		
		return true;
	}
	
	@Override
	public boolean canFinish(){
		
		boolean finished = true;
		
		for(WizardPage w : this.wizardPageList){
			finished = finished && w.isPageComplete();
		}
		
		return finished;
		
	}
	
	
	public class SelectModelAndSystemPage extends WizardPage {
		
		private Composite container;
		private String[] labels;
		private Label modelLabel;
		private Label systemLabel;
		private Combo modelCombo;
		private Combo systemCombo;

		public SelectModelAndSystemPage() {
			super("Select Model and System");
			setTitle("Select Model and System");
			setDescription("Select Model and System");
		}

		@Override
		public void createControl(Composite parent) {
			container = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout();
		    container.setLayout(layout);
		    layout.numColumns = 2;
		    
		    drawPage();
		    
		    setControl(container);
			    

		}
		
		public void drawPage(){
			
		    modelLabel = new Label(container, SWT.NONE);
		    modelLabel.setText("Model:");
			modelCombo = new Combo(container, SWT.NONE);
			
			systemLabel = new Label(container, SWT.NONE);
		    systemLabel.setText("System:");
			systemCombo = new Combo(container, SWT.NONE);
			
		    for (IResource res : resources) {
		    	modelCombo.add(res.getName().split("\\.")[0]);
			}
			
			if(modelChoice > -1){
				labels = models.get(resources.get(modelChoice)).getSystems();
				modelCombo.setText(resources.get(modelChoice).getName().split("\\.")[0]);
			} else
		    	labels = new String[] {""};
			
		    for (String label : labels) {
		    	systemCombo.add(label);
			}

		    modelCombo.addListener(SWT.Selection, new Listener() {
				public void handleEvent(Event event) {
					modelChoice = modelCombo.getSelectionIndex();
					refreshPage();
					checkPage();
				}
			});
		    
		    systemCombo.addListener(SWT.Selection, new Listener() {
				public void handleEvent(Event event) {
					systemChoice = systemCombo.getSelectionIndex();
					checkPage();
				}
			});
		    
			
		}
		
		public void refreshPage(){
			
			modelLabel.dispose();
			modelCombo.dispose();
			
			systemLabel.dispose();
			systemCombo.dispose();
			
			drawPage();
		    
		    container.layout(true);
		}
		
		public void checkPage(){
			
			
			if(modelChoice == -1){
				setPageComplete(false);
				return;
			}
			if(systemChoice == -1){
				setPageComplete(false);
				return;
			}
			setPageComplete(true);
			measuresPage.updatePage();
		}
		
		public String getSystem(){
			return labels[systemChoice];
		}
		
		public CarmaModel getModel(){
			return models.get(resources.get(modelChoice));
		}
		
	}
	
	public class SelectMeasuresPage extends WizardPage {
		
		private class MeasuresProvider extends LabelProvider implements ITableLabelProvider {

			@Override
			public Image getColumnImage(Object element, int columnIndex) {
				return null;
			}

			@Override
			public String getColumnText(Object element, int columnIndex) {
				return (String) element;
			}
			
		}
		
		private Composite container;
		private CheckboxTableViewer viewer;

		public SelectMeasuresPage() {
			super("Select Measures");
			setTitle("Select Measures");
			setDescription("Select Measures");
		}

		@Override
		public void createControl(Composite parent) {
			container = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout();
		    container.setLayout(layout);

		    createViewer(container);
		    
		    setControl(container);
			    

		}
		
		public void updatePage(){
			
			
			viewer.setInput(getViewerInput());
			
			container.layout();
		}
		
		public void createViewer(Composite parent){
			
			GridData gridData = new GridData(GridData.FILL_BOTH);
		    gridData.horizontalSpan = 2;
			
			viewer = CheckboxTableViewer.newCheckList(parent, SWT.NONE);
			viewer.getTable().setLayoutData(gridData);
			viewer.setContentProvider(new ArrayContentProvider());
			viewer.setLabelProvider(getProvider());
			viewer.setInput(getViewerInput());
			viewer.addCheckStateListener(new ICheckStateListener() {

				public void checkStateChanged(CheckStateChangedEvent event) {
					checkPage();
				}

			});
			checkPage();
			parent.pack();
		}
		
		ITableLabelProvider getProvider() {
			return new MeasuresProvider();
		}
		
		Object getViewerInput() {
			if(modelChoice > -1)
				return models.get(resources.get(modelChoice)).getMeasures();
			else
				return new String[] {"null"};
		}
		
		public void checkPage(){
			if(viewer.getCheckedElements().length < 1){
				setPageComplete(false);
				return;
			}
			setPageComplete(true);
		} 
		
		public String[] getMeasures(){
			
			String[] measures = new String[viewer.getCheckedElements().length];
			
			for(int i = 0; i < viewer.getCheckedElements().length; i++){
				measures[i] = (String) viewer.getCheckedElements()[i];
			}
			
			return measures; 
		}
		
	}
	
	public class SetDeadlineAndIterations extends WizardPage {
		
		private Composite container;
		private PositiveIntegerConfigurationText deadline;
		private PositiveIntegerConfigurationText iterations;
		private PositiveIntegerConfigurationText simulations;
		private Label deadlineLabel;
		private Label iterationsLabel;
		private Label simulationsLabel;
		
		public class PositiveIntegerConfigurationText {
			
			Text control;
			protected String propertyValue;
			
			public PositiveIntegerConfigurationText(String defaultValue) {
				propertyValue = defaultValue;
			}
			
			public Control createControl(Composite parent) {
				control = new Text(parent, SWT.BORDER | SWT.RIGHT);
				updateControl();
				control.addListener(SWT.Modify, new Listener() {

					public void handleEvent(Event event) {
						setValue(((Text) control).getText());
						checkPage();
					}

				});
				return control;
			}
			
			public
			void setValue(String value) {
				this.propertyValue = value;
			}
			
			
			public void updateControl() {
				((Text) control).setText("" + getValue());
			}
			
			public Object getValue() {
				try {
					return Integer.parseInt(this.propertyValue);
				} catch (NumberFormatException e) {
					return null;
				}
			}
			
			public boolean isValid() {
				try {
					int i = Integer.parseInt(this.propertyValue);
					return i > 0;
				} catch (NumberFormatException e) {
					return false;
				}
			}
		}

		}
		
		public void checkPage(){
			if(modelChoice == -1){
				setPageComplete(false);
				return;
			}
			setupClass();
			setPageComplete(true);
			
		}
		
		public void setupClass(){
			
			carmaModel = carmaModels.get(modelChoice);
//			Model model = models.get(modelChoice);
//			String[] classPathEntries;
//			String projectName = model.eResource().getURI().segments()[1];
//			
//			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
//			for(IProject project: projects){
//				if(projectName.equals(project.getName())){
//					
//					try{
//						project.open(null /* IProgressMonitor */);
//						IJavaProject javaProject = JavaCore.create(project);
//						classPathEntries = JavaRuntime.computeDefaultRuntimeClassPath(javaProject);
//						
//						
//						
//						List<URL> urlList = new ArrayList<URL>();
//						for (int i = 0; i < classPathEntries.length; i++) {
//							System.out.println(classPathEntries[i]);
//							String entry = classPathEntries[i];
//							IPath path = new Path(entry);
//							URL url = path.toFile().toURI().toURL();
//							urlList.add(url);
//						}
//						
//						parentClassLoader = project.getClass().getClassLoader();
//						URL[] urls = (URL[]) urlList.toArray(new URL[urlList.size()]);
//						classLoader = new URLClassLoader(urls, parentClassLoader);
//						
//						clazz = classLoader.loadClass("ms."+model.eResource().getURI().lastSegment().split("\\.")[0]);
//						carmaModel = (Object) clazz.newInstance();
//						
//					} catch (CoreException | MalformedURLException | ClassNotFoundException | InstantiationException | IllegalAccessException | ClassCastException e){
//						System.out.println(e);
//					}
//				}
//			}
//			
//
		}
		
	}
	
	

		

	public class SelectSystemPage extends WizardPage {
		
		private Composite container;
		private TableViewer viewer;

		public SelectSystemPage() {
			super("Select System");
			setTitle("Select System");
			setDescription("Select System");
		}

		@Override
		public void createControl(Composite parent) {
			this.container = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout();
		    container.setLayout(layout);

		    createViewer(container);
		    
		    setControl(container);
			    

		}
		
		public void createViewer(Composite parent){
			viewer = new TableViewer(parent,SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
			createColumns(parent, viewer);
			final Table table = viewer.getTable();
			table.setHeaderVisible(true);
			table.setLinesVisible(true);
			
			viewer.setContentProvider(new ArrayContentProvider());
			if(modelChoice > -1){
					System.out.println(carmaModel.getClass().getName());
			}
			
		    GridData gridData = new GridData();
		    gridData.verticalAlignment = GridData.FILL;
		    gridData.horizontalSpan = 2;
		    gridData.grabExcessHorizontalSpace = true;
		    gridData.grabExcessVerticalSpace = true;
		    gridData.horizontalAlignment = GridData.FILL;
		    viewer.getControl().setLayoutData(gridData);
		}
		
		private void createColumns(final Composite parent, final TableViewer viewer) {
			String[] titles = { "System", "Selected"};	
			int[] bounds = {100, 100};
			
		    TableViewerColumn col = createTableViewerColumn(titles[0], bounds[0], 0);
		    col.setLabelProvider(new ColumnLabelProvider() {
			    @Override
			    public String getText(Object element) {
			    	return (String) element;
			    }
		    });
			
		    col = createTableViewerColumn(titles[1], bounds[1], 1);
		    col.setLabelProvider(new ColumnLabelProvider() {
			    @Override
			    public String getText(Object element) {
			    	return (String) element;
			    }
		    });
		}
		
		public void checkPage(){
			if(modelChoice == -1){
				setPageComplete(false);
				return;
			}
			setPageComplete(true);
			
		}  
		
		private TableViewerColumn createTableViewerColumn(String title, int bound, final int colNumber) {
			final TableViewerColumn viewerColumn = new TableViewerColumn(viewer,SWT.NONE);
			final TableColumn column = viewerColumn.getColumn();
	        column.setText(title);
	        column.setWidth(bound);
	        column.setResizable(true);
	        column.setMoveable(true);
	        return viewerColumn;
        }
		
	}

		@Override
		public void createControl(Composite parent) {
			container = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout();
		    container.setLayout(layout);
		    layout.numColumns = 2;
		    
		    drawPage(container);
		    
		    setControl(container);
			
		}
		
		public void drawPage(Composite container){
			
			deadlineLabel = new Label(container, SWT.NONE);
			deadlineLabel.setText("Deadline:");
			deadline = new PositiveIntegerConfigurationText("50");
			deadline.createControl(container);
			
			iterationsLabel = new Label(container, SWT.NONE);
			iterationsLabel.setText("Iterations:");
			iterations = new PositiveIntegerConfigurationText("100");
			iterations.createControl(container);
			
			simulationsLabel = new Label(container, SWT.NONE);
			simulationsLabel.setText("Simulations:");
			simulations = new PositiveIntegerConfigurationText("10");
			simulations.createControl(container);
			
		}
		
		public void checkPage(){
			if(!deadline.isValid() || !iterations.isValid()){
				setPageComplete(false);
				setErrorMessage("Configuration not valid");
				return;
			}
			setErrorMessage(null);
			setPageComplete(true);
		} 
		
		public int getDeadline(){
			return (int) this.deadline.getValue();
		}
		
		public int getIterations(){
			return (int) this.iterations.getValue();
		}
		
		public int getSimulations(){
			return (int) this.simulations.getValue();
		}
		
	}
	
}