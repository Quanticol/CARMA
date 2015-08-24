/**
 * 
 */
package eu.quanticol.carma.ui.wizards;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti, williams
 *
 */
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
		
	}
	
	public class SetDeadlineAndIterations extends WizardPage {
		
		private Composite container;
		private PositiveIntegerConfigurationText deadline;
		private PositiveIntegerConfigurationText iterations;
		private Label deadlineLabel;
		private Label iterationsLabel;
		
		
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

		protected SetDeadlineAndIterations() {
			super("Set deadline and iterations");
			setTitle("Set deadline and iterations");
			setDescription("Set deadline and iterations");
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
			
		}
		
		public void checkPage(){
			setPageComplete(true);
		}  
		
	}
	
}