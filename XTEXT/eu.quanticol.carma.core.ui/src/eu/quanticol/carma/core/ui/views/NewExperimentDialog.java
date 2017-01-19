/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.views.models.ProjectSimulationSuite;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.simulator.CarmaModel;

/**
 * @author loreti, williams
 *
 */
public class NewExperimentDialog extends TitleAreaDialog {

	private static final String DEFAULT_MESSAGE = "Create a new simulation experiment";
	
	private static final String SIMULATION_TIME_ERROR = "Simulation time must be a numeric value!";

	private static final String REPLICATIONS_ERROR = "Number of simulation replications must be an integer value!";

	private static final String SAMPLING_ERROR = "Number of samplings must be an integer value!";

	private HashMap<IResource, CarmaModel> models;

	private static HashMap<IResource,SimulationExperiment> cache = new HashMap<IResource, SimulationExperiment>();
	
	private Combo modelSelection;

	private Combo systemSelection;

	private int currentModelIndex = -1;

	private ArrayList<IResource> resources;

	private Composite area;

	private Composite container;

	private Text simulationTimeField;

	private Text replicationsField;

	private Text samplingsField;

	private CarmaModel selectedModel;
	
	private ArrayList<MeasureData> measureData = new ArrayList<>();

	private Composite measureContainer;

	private Combo measureSelection;

	private List measureTable;

	private IResource selectedResource;

	private boolean withError;

	private Text experimentNameField;

	private boolean withName;

	private SimulationExperiment experiment;
	
	public NewExperimentDialog(HashMap<IResource, CarmaModel> models , Shell parentShell) {
		this(models,parentShell,false,null);
	}

	public NewExperimentDialog(HashMap<IResource, CarmaModel> models , Shell parentShell , boolean withName) {
		this(models,parentShell,withName,null);
	}

	public NewExperimentDialog(HashMap<IResource, CarmaModel> models , Shell parentShell, boolean withName , SimulationExperiment experiment ) {
		super(parentShell);
	    this.models = models;
	    this.resources = new ArrayList<>(); 
	    this.resources.addAll( models.keySet() );
	    this.withName = withName;
	    this.experiment = experiment;
	}
	
	@Override
	public void create() {
	    super.create();
	    setTitle("New simulation experiment...");
	    setMessage(DEFAULT_MESSAGE, IMessageProvider.INFORMATION);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		area = (Composite) super.createDialogArea(parent);
	    container = new Composite(area, SWT.NONE);
	    container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
	    GridLayout layout = new GridLayout(2, false);
	    container.setLayout(layout);
	
	    if (withName) {
	    	createExperimentNameArea(container);
	    }	    
	    createModelSelectionArea(container);
	    createSystemSelectionArea(container);
	    createSimulationTimeArea(container);
	    createReplicationsArea(container);
	    createSamplingsArea(container);
	    createMeasureArea(container);
	    if (experiment != null) {
	    	retrieveData();
	    }
	    return area;
	}

	private void createExperimentNameArea(Composite container) {
	    Label lbtSimulationTime = new Label(container, SWT.NONE);
	    lbtSimulationTime.setText("Experiment name:");

	    GridData dataSimulationTime = new GridData();
	    dataSimulationTime.grabExcessHorizontalSpace = true;
	    dataSimulationTime.horizontalAlignment = GridData.FILL;

	    experimentNameField = new Text(container, SWT.BORDER | SWT.LEFT);;	
	    experimentNameField.setLayoutData(dataSimulationTime);
	}

	private void createMeasureArea(Composite container) {
		measureContainer = new Composite(container, SWT.NONE);
		measureContainer.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
	    GridLayout layout = new GridLayout(4, false);
	    measureContainer.setLayout(layout);
	    GridData dataMeasureArea = new GridData();
	    dataMeasureArea.grabExcessHorizontalSpace = true;
	    dataMeasureArea.horizontalAlignment = GridData.FILL;
	    dataMeasureArea.horizontalSpan = 2;
	    measureContainer.setLayoutData(dataMeasureArea);
	    
	    Label measureLabel = new Label(measureContainer, SWT.NONE);
	    measureLabel.setText("Measure:");

	    GridData dataMeasureSelection = new GridData();
	    dataMeasureSelection.grabExcessHorizontalSpace = true;
	    dataMeasureSelection.horizontalAlignment = GridData.FILL;

	    measureSelection = new Combo(measureContainer, SWT.BORDER);	
	    if (selectedModel != null) {
		    measureSelection.setItems(selectedModel.getMeasures());
	    } 
	    
	    measureSelection.setLayoutData(dataMeasureSelection);
	    Button addButton = new Button(measureContainer, SWT.PUSH);
	    addButton.setText("Add");

	    addButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				doAddMeasure();
			}
		});
	    
	    Button removeButton = new Button(measureContainer, SWT.PUSH);
	    removeButton.setText("Remove");
	    
	    removeButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				doRemoveMeasure();
			}
		});
	 
	    GridData dataMeasureTable = new GridData();
	    dataMeasureTable.grabExcessHorizontalSpace = true;
	    dataMeasureTable.grabExcessVerticalSpace = true;
	    dataMeasureTable.horizontalAlignment = GridData.FILL;
	    dataMeasureTable.verticalAlignment = GridData.FILL;
	    dataMeasureTable.horizontalSpan = 4;
	    
	    measureTable = new List(measureContainer, SWT.SINGLE | SWT.H_SCROLL
	            | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
	    
	    measureTable.setLayoutData(dataMeasureTable);

	}

	protected void doRemoveMeasure() {
		int selected = measureTable.getSelectionIndex();
		if (selected != -1) {
			measureData.remove(selected);
			measureTable.remove(selected);
		}
	}

	protected void doAddMeasure() {
		if (measureSelection.getSelectionIndex()>=0) {
			String measure = selectedModel.getMeasures()[measureSelection.getSelectionIndex()];
			String[] parameters = selectedModel.getMeasureParameters(measure);
			if (parameters.length>0) {
				MeasureParametersDialog mpd = new MeasureParametersDialog(
						this.getShell() ,
						selectedModel.getMeasureParameters(measure) ,
						selectedModel.getParametersType(measure)
					);
					
					if (mpd.open()==OK) {
						measureSelection.clearSelection();
						for (Map<String,Object> pars : mpd.getParameters()) {
							MeasureData newData = new MeasureData(measure, pars);
							measureData.add(newData);
							measureTable.add(newData.toString());
						}
						measureContainer.layout();
					};
			} else {
				MeasureData newData = new MeasureData(measure, new HashMap<>());
				measureSelection.clearSelection();
				measureData.add(newData);
				measureTable.add(newData.toString());
				measureContainer.layout();
			}
		}
	}

	private void createModelSelectionArea(Composite container) {
	    Label lbtModelSelection = new Label(container, SWT.NONE);
	    lbtModelSelection.setText("Model:");

	    GridData dataModelSelection = new GridData();
	    dataModelSelection.grabExcessHorizontalSpace = true;
	    dataModelSelection.horizontalAlignment = GridData.FILL;

	    modelSelection = new Combo(container, SWT.BORDER);	
	    for (IResource r : resources) {
	    	modelSelection.add(r.getName());
		}
	    modelSelection.setLayoutData(dataModelSelection);
	    modelSelection.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				doRegisterModelSelection();
			}
		});
	}

	private void createSystemSelectionArea(Composite container) {
	    Label lbtSystemSelection = new Label(container, SWT.NONE);
	    lbtSystemSelection.setText("System:");

	    GridData dataSystemSelection = new GridData();
	    dataSystemSelection.grabExcessHorizontalSpace = true;
	    dataSystemSelection.horizontalAlignment = GridData.FILL;

	    systemSelection = new Combo(container, SWT.BORDER);	
	    systemSelection.setLayoutData(dataSystemSelection);
	    modelSelection.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				doRegisterSystemSelection();
			}
		});
	    doRefreshSystemList();
	}

	private void createSimulationTimeArea(Composite container) {
	    Label lbtSimulationTime = new Label(container, SWT.NONE);
	    lbtSimulationTime.setText("Simulation Time:");

	    GridData dataSimulationTime = new GridData();
	    dataSimulationTime.grabExcessHorizontalSpace = true;
	    dataSimulationTime.horizontalAlignment = GridData.FILL;

	    simulationTimeField = new Text(container, SWT.BORDER | SWT.LEFT);;	
	    simulationTimeField.setLayoutData(dataSimulationTime);
	    simulationTimeField.addModifyListener( new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				checkFieldsData();				
			}
			
		});
	}

	private void createReplicationsArea(Composite container) {
	    Label lbtReplications = new Label(container, SWT.NONE);
	    lbtReplications.setText("Replications:");

	    GridData dataReplications = new GridData();
	    dataReplications.grabExcessHorizontalSpace = true;
	    dataReplications.horizontalAlignment = GridData.FILL;

	    replicationsField = new Text(container, SWT.BORDER | SWT.LEFT);;	
	    replicationsField.setLayoutData(dataReplications);
	    replicationsField.addModifyListener( new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				checkFieldsData();				
			}
			
		});
	}

	private void createSamplingsArea(Composite container) {
	    Label lbtSamplings = new Label(container, SWT.NONE);
	    lbtSamplings.setText("Samplings:");

	    GridData dataSamplings = new GridData();
	    dataSamplings.grabExcessHorizontalSpace = true;
	    dataSamplings.horizontalAlignment = GridData.FILL;

	    samplingsField = new Text(container, SWT.BORDER | SWT.LEFT);;	
	    samplingsField.setLayoutData(dataSamplings);
	    samplingsField.addModifyListener( new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				checkFieldsData();				
			}
			
		});
	}

	
	protected void checkFieldsData() {
		boolean withError = false;
		String infoMessage = "";
		String msg;
		msg = simulationTimeField.getText();
		if (!msg.isEmpty()) {
			try {
				Double.parseDouble(msg);
			} catch (NumberFormatException e) {
				withError = true;
				infoMessage = SIMULATION_TIME_ERROR;
			}
		}
		msg = replicationsField.getText();
		if (!msg.isEmpty()) {
			try {
				Integer.parseInt(msg);
			} catch (NumberFormatException e) {
				withError = true;
				infoMessage = REPLICATIONS_ERROR;
			}
		}
		msg = samplingsField.getText();
		if (!msg.isEmpty()) {
			try {
				Integer.parseInt(msg);
			} catch (NumberFormatException e) {
				withError = true;
				infoMessage = SAMPLING_ERROR;
			}
		}
		if (withError) {
			setMessage(infoMessage,IMessageProvider.ERROR);
		} else {
			setMessage(DEFAULT_MESSAGE,IMessageProvider.INFORMATION);
		}
	}

	private void doRefreshSystemList() {
		systemSelection.removeAll();
		int idx = modelSelection.getSelectionIndex();
		if (idx<0) {			
			systemSelection.setEnabled(false);
		} else {
			String[] systems = selectedModel.getSystems();
			systemSelection.setItems(systems);
			systemSelection.setEnabled(true);
		}
		systemSelection.layout();
		container.layout();
	}

	private void doRefreshMeasureList( boolean checkCache ) {
		measureSelection.removeAll();
		measureData = new ArrayList<>();
		measureTable.removeAll();
		if (selectedModel==null) {			
			measureSelection.setEnabled(false);
		} else {
			String[] measures = selectedModel.getMeasures();
			measureSelection.setItems(measures);
			measureSelection.setEnabled(true);
			if (checkCache) {
				SimulationExperiment cached = cache.get(selectedResource);
				if (cached != null) {
					simulationTimeField.setText(cached.getSimulationTime()+"");
					replicationsField.setText(cached.getReplications()+"");
					samplingsField.setText(cached.getSamplings()+"");
					for (MeasureData m : cached.getMeasures()) {
						if (selectedModel.checkParameterType(m.getMeasureName(), m.getParameters())) {
							measureData.add(m);	
							measureTable.add(m.toString());
						}
					}
				}
			}
		}
		measureSelection.layout();
		measureContainer.layout();
		container.layout();
	}

	protected void doRegisterSystemSelection() {
		// TODO Auto-generated method stub
		
	}

	protected void doRegisterModelSelection() {
		int idx = modelSelection.getSelectionIndex();
		if (idx != currentModelIndex) {
			if (measureData.isEmpty()||MessageDialog.openQuestion(modelSelection.getShell(), "Confirm...", "All measures will be lost. Do you want to continue?")) {				
				this.currentModelIndex = idx;
				if (currentModelIndex<0) {
					selectedModel = null;
				} else {
					selectedResource = resources.get(currentModelIndex);
					selectedModel = models.get(selectedResource);
				}
				doRefreshSystemList();
				doRefreshMeasureList( true );
			}
		}
	}

	public MeasureData[] getMeasures() {
		return measureData.toArray(new MeasureData[measureData.size()]);
	}

	public CarmaModel getModel() {
		return selectedModel;
	}

	public String getModelName() {
		return selectedResource.getName();
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	@Override
	protected void okPressed() {
		if ((!withError)&&(allDataProvided())) {
			saveInput();
			if (!measureData.isEmpty()||
					MessageDialog.openQuestion(this.getShell(), "Confirm...", "No measure has been selected. Do you want continue?")
			) {
				super.okPressed();
			}
		} else {
			if (withError) {
				MessageDialog.openError(this.getShell(), "Error...", "Provided data are not correct!");
			} else {
				MessageDialog.openError(this.getShell(), "Error...", "Fill all the required data!");
			}
		}
	}

	private boolean allDataProvided() {
		return (modelSelection.getSelectionIndex()!=-1)&&
				(systemSelection.getSelectionIndex()!=-1)&&
				((!withName)||(!experimentNameField.getText().isEmpty()))&&
				(!simulationTimeField.getText().isEmpty())&&
				(!replicationsField.getText().isEmpty())&&
				(!samplingsField.getText().isEmpty());
	}

	private void saveInput() {
		double simulationTime = Double.parseDouble(simulationTimeField.getText());
		int samplings = Integer.parseInt(samplingsField.getText());
		int replications = Integer.parseInt(replicationsField.getText());
		String system = systemSelection.getItem(systemSelection.getSelectionIndex());
		String name = (withName?experimentNameField.getText():"<Unnamed>");
		experiment = new SimulationExperiment(name, selectedResource, selectedModel, system ,replications, simulationTime, samplings, measureData);
		cache.put(selectedResource, experiment);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#setMessage(java.lang.String, int)
	 */
	@Override
	public void setMessage(String newMessage, int newType) {		
		if (newType == IMessageProvider.ERROR) {
			withError = true;
		} else {
			withError = false;
		}
		super.setMessage(newMessage, newType);
	}

	public IResource getIResource() {
		return selectedResource;
	}

	public SimulationExperiment getSimulationExperiment() {
		return experiment;
	}

	private void retrieveData() {
		if (withName) {
			experimentNameField.setText(experiment.getName());
		}
		int idx = resources.indexOf(experiment.getResource());
		if (idx>=0) {
			modelSelection.select(idx);
			currentModelIndex = idx;
			selectedResource = resources.get(currentModelIndex);
			selectedModel = models.get(selectedResource);
			doRefreshSystemList();
			doRefreshMeasureList( false );
			int systemIndex = getIndexOfSystem( experiment.getSystem() );
			if (systemIndex>=0) {
				systemSelection.select(systemIndex);
			}
			simulationTimeField.setText(experiment.getSimulationTime()+"");
			replicationsField.setText(experiment.getReplications()+"");
			samplingsField.setText(experiment.getSamplings()+"");
			for (MeasureData m : experiment.getMeasures()) {
				if (selectedModel.checkParameterType(m.getMeasureName(), m.getParameters())) {
					measureData.add(m);	
					measureTable.add(m.toString());
				}
			}
		}
	}

	private int getIndexOfSystem(String system) {
		for( int i=0 ; i<systemSelection.getItemCount() ; i++ ) {
			if (systemSelection.getItem(i).equals(system)) {
				return i;
			}
		}
		return -1;
	}

}
