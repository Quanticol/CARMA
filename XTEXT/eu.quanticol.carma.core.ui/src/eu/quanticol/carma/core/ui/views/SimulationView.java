/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashSet;
import java.util.LinkedList;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.google.inject.Inject;

import eu.quanticol.carma.core.ui.CarmaUiUtil;
import eu.quanticol.carma.core.ui.ExtendedCARMAActivator;
import eu.quanticol.carma.core.ui.data.SimulationExperiment;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.core.ui.jobs.RunSimulationExperimentJob;
import eu.quanticol.carma.core.ui.nature.CarmaNature;
import eu.quanticol.carma.core.ui.views.models.ExperimentDetail;
import eu.quanticol.carma.core.ui.views.models.ProjectSimulationSuite;
import eu.quanticol.carma.core.ui.views.models.SimulationSuiteElement;
import eu.quanticol.carma.core.ui.views.models.WorkspaceSimulationSuite;

/**
 * @author loreti
 *
 */
public class SimulationView extends ViewPart {
	
	public static final String ID = "eu.quanticol.carma.ui.views.SimulationView";

	@Inject CarmaUiUtil util;

	private Action addExperiment;
	private Action removeExperiment;
	private Action editExperiment;
	
	private Action runExperiment;
	
	private Action copyExperiment;
	
	private Action plotExperimentData;
	
	private Action exportExperimentData;

	private Action saveLaboratory;

	private Action saveAllLaboratory;

	private TreeViewer suiteViewer;
	
	private TableViewer resultViewer;

	private WorkspaceSimulationSuite workSpaceSimulationSuite;

	private Action showExperimentData;
	
	@Override
	public void createPartControl(Composite parent) {
		Composite container = new Composite(parent, SWT.NONE );
		container.setLayout(new FillLayout(SWT.HORIZONTAL));
		Group experimentsGroup = new Group(container, SWT.NONE);
		experimentsGroup.setText("Experiments");
		experimentsGroup.setLayout(new FillLayout());
		Group resultsGroup = new Group(container, SWT.BORDER);
		resultsGroup.setText("Results");
		resultsGroup.setLayout(new FillLayout());
		createSuiteViewer( experimentsGroup );
		createResultViewer( resultsGroup );
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();	
		enableDisableActions();
		util.registerChangeListener( new IResourceChangeListener() {
			
			@Override
			public void resourceChanged(IResourceChangeEvent event) {
				doHandleChangeEvent( event );
			}
		});
	}

	protected void doHandleChangeEvent(IResourceChangeEvent event) {
		if (event.getType()==IResourceChangeEvent.POST_CHANGE) {
			System.out.println("HERE!");
			IResourceDelta delta = event.getDelta();
			HashSet<IProject> toRefresh = new HashSet<>();
			IResourceDeltaVisitor visitor = new IResourceDeltaVisitor() {
				
				@Override
				public boolean visit(IResourceDelta delta) throws CoreException {
					if (delta.getKind() == IResourceDelta.ADDED) {
						if (delta.getResource() instanceof IProject) {
							IProject project = (IProject) delta.getResource();
							if (project.hasNature(CarmaNature.ID)) {
								Display.getDefault().asyncExec(new Runnable() 
								{ 
									public void run() {
										doAddProjectToWorkspaceSimulationSuite( project );
									}
									
								});
							}
							return false;
						}
					}
					if (delta.getKind() == IResourceDelta.REMOVED) {
						if (delta.getResource() instanceof IProject) {
							IProject project = (IProject) delta.getResource();
							Display.getDefault().asyncExec(new Runnable() 
							{ 
								public void run() {
									doRemoveProjectFromWorkspaceSimulationSuite( project );
								}
								
							});
							return false;
						}
						String ext = delta.getResource().getFileExtension();
						if ((ext != null)&&(ext.equals("carma"))) {
							IResource r = delta.getResource();
							IProject p = r.getProject();
							Display.getDefault().asyncExec(new Runnable() 
							{ 
								public void run() {
									doRefreshResource(p, r);
								}
								
							});
							return false;
						}
					}
					if (delta.getKind() == IResourceDelta.CHANGED) {
						if (delta.getResource() instanceof IProject) {
							if ((delta.getFlags() & IResourceDelta.OPEN)!=0) {
								IProject project = (IProject) delta.getResource();
								if (project.isOpen()) {
									if (project.hasNature(CarmaNature.ID)) {
										Display.getDefault().asyncExec(new Runnable() 
										{ 
											public void run() {
												doAddProjectToWorkspaceSimulationSuite( project );
											}
											
										});
									}
								} else {
									Display.getDefault().asyncExec(new Runnable() 
									{ 
										public void run() {
											doRemoveProjectFromWorkspaceSimulationSuite( project );
										}
										
									});
								}
								return false;
							}
						} else {
							String ext = delta.getResource().getFileExtension();
							System.out.println(delta.getResource().getName());
							if ((ext != null)&&((ext.equals("carma")||ext.equals("class")))) {
								IResource r = delta.getResource();
								IProject p = r.getProject();
//								Display.getDefault().asyncExec(new Runnable() 
//								{ 
//									public void run() {
//										doRefreshResource(p, r);
//									}
//									
//								});
//								return false;
								toRefresh.add(p);
							}
						}
					}
					return true;
				}
			};
			try {
				delta.accept(visitor);
				for (IProject iProject : toRefresh) {
					workSpaceSimulationSuite.doRefreshProcessResources( iProject , r -> util.loadCarmaModel(iProject, r));
				}
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	protected void doRefreshResource(IProject p, IResource r) {
		workSpaceSimulationSuite.doRefreshResource( p , r , util.loadCarmaModel(p, r));
		//suiteViewer.setInput(workSpaceSimulationSuite);
		suiteViewer.refresh();
	}

	protected void doAddProjectToWorkspaceSimulationSuite(IProject project) {
		workSpaceSimulationSuite.addProjectSimulationSuite( util.loadProjectSimulationSuite(project) );
		//suiteViewer.setInput(workSpaceSimulationSuite);
		suiteViewer.refresh();
	}

	protected void doRemoveProjectFromWorkspaceSimulationSuite(IProject project) {
		if (workSpaceSimulationSuite.removeProject( project )) {
//			suiteViewer.setInput(workSpaceSimulationSuite);			
			suiteViewer.refresh();

		}
	}


	private void createSuiteViewer(Group experimentsGroup) {
		suiteViewer = new TreeViewer(experimentsGroup, SWT.H_SCROLL | SWT.V_SCROLL);
		suiteViewer.setContentProvider(new SimulationViewContentProvider());
		suiteViewer.setLabelProvider(new SimulationViewLabelProvider());
		try {
			workSpaceSimulationSuite = util.getWorkspaceSimulationSuite();
			suiteViewer.setInput(workSpaceSimulationSuite);
			suiteViewer.addSelectionChangedListener( new ISelectionChangedListener() {
				
				@Override
				public void selectionChanged(SelectionChangedEvent event) {
					enableDisableActions();
					showSimulationResults();
				}
				
			});
		} catch (Exception e) {
			e.printStackTrace(); //FIXME!
		}
	}

	private void createResultViewer(Group resultsGroup) {
		resultViewer = new TableViewer(resultsGroup, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		resultViewer.setContentProvider(ArrayContentProvider.getInstance());
		resultViewer.setInput(new LinkedList<>());
		TableViewerColumn column = createTableViewerColumn("Date", 200, 0);
		column.setLabelProvider(new ColumnLabelProvider() {
			 @Override
             public String getText(Object element) {
                     return ((SimulationOutcome) element).getStartingTime();
             }
		});
		column = createTableViewerColumn("Total time", 100, 1);
		column.setLabelProvider(new ColumnLabelProvider() {
			 @Override
             public String getText(Object element) {
                     return ((SimulationOutcome) element).getTotalTime()+"ms";
             }
		});
		column = createTableViewerColumn("Average time", 100, 1);
		column.setLabelProvider(new ColumnLabelProvider() {
			 @Override
             public String getText(Object element) {
                     return ((SimulationOutcome) element).getAverageTime()+"ms";
             }
		});
		resultViewer.addSelectionChangedListener( new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				enableDisableActions();
			}
			
		});

	}

    private TableViewerColumn createTableViewerColumn(String title, int bound, final int colNumber) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(resultViewer,
                        SWT.NONE);
        final TableColumn column = viewerColumn.getColumn();
        column.setText(title);
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(false);
        return viewerColumn;
    }	
	
	protected void showSimulationResults() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			if (selected.getSegmentCount()>=2) {
				SimulationSuiteElement simulationSuiteElement = (SimulationSuiteElement) selected.getSegment(1);
				resultViewer.setInput(simulationSuiteElement.getExperimentTags());
			} else {
				resultViewer.setInput(new LinkedList<>());
			}
		} else {
			resultViewer.setInput(new LinkedList<>());
		}
	}

	protected void enableDisableActions() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		saveAllLaboratory.setEnabled(workSpaceSimulationSuite.size()>0);
		//addExperiment.setEnabled(workSpaceSimulationSuite.size()>0);
		if (paths.length == 0) {
			addExperiment.setEnabled(false);
			//addExperiment.setEnabled(workSpaceSimulationSuite.size()>0);
			removeExperiment.setEnabled(false);
			editExperiment.setEnabled(false);
			runExperiment.setEnabled(false);
			copyExperiment.setEnabled(false);
			saveLaboratory.setEnabled(false);
		} else {
			addExperiment.setEnabled(true);
			Object element = paths[0].getLastSegment();
			boolean experimentSelected = (element instanceof ExperimentDetail)||(element instanceof SimulationSuiteElement);
			removeExperiment.setEnabled(experimentSelected);
			editExperiment.setEnabled(experimentSelected);
			runExperiment.setEnabled(experimentSelected);
			copyExperiment.setEnabled(experimentSelected);
			saveLaboratory.setEnabled(true);			
		}
		SimulationOutcome result = (SimulationOutcome) resultViewer.getStructuredSelection().getFirstElement();
		if (result != null) {
			plotExperimentData.setEnabled(true);
			exportExperimentData.setEnabled(true);
		} else {
			plotExperimentData.setEnabled(false);
			exportExperimentData.setEnabled(false);
		}
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}

	private void makeActions() {
		makeAddExperimentAction();
		makeRemoveExperimentAction();
		makeEditExperimentAction();
		makeRunExperimentAction();
		makePlotExperimentAction();
		makeCopyExperimentAction();
		makeExportExperimentAction();
		makeSaveLaboratoryAction();
		makeSaveAllLaboratoryAction();
		makeShowExperimentDataAction();
	}
	
	private void makeAddExperimentAction() {
		addExperiment = new Action() {
			public void run() {
				doAddExperiment();
			}
		};
		addExperiment.setText("Add Experiment");
		addExperiment.setToolTipText("Add an experiment to a simulation suite.");
		addExperiment.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_ADD_EXERIMENT_ID)
		);
//		addExperiment.setEnabled(false);
	}

	protected void doAddExperiment() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			ProjectSimulationSuite projectSuite = ((ProjectSimulationSuite) selected.getFirstSegment());
			NewExperimentDialog dialog = new NewExperimentDialog(util.getProjectModels(projectSuite.getProject()), suiteViewer.getControl().getShell(),true);
			if (dialog.open()==Window.OK) {
				projectSuite.add( dialog.getSimulationExperiment() );	
				suiteViewer.refresh();
			}
		}
	}

	private void makeRemoveExperimentAction() {
		removeExperiment = new Action() {
			public void run() {
				doRemoveLaboratory();
			}
		};
		removeExperiment.setText("Remove Experiment");
		removeExperiment.setToolTipText("Remove an experiment from a simulation suite.");
		removeExperiment.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_DELETE_EXERIMENT_ID)
		);
	}

	private void makeEditExperimentAction() {
		editExperiment = new Action() {
			public void run() {
				doEditExperiment();
			}
		};
		editExperiment.setText("Edit Experiment");
		editExperiment.setToolTipText("Edit an experiment.");
		editExperiment.setImageDescriptor(
				ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_EDIT_EXERIMENT_ID)
		);
	}

	protected void doEditExperiment() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			if (selected.getSegmentCount()>=2) {
				ProjectSimulationSuite projectSuite = (ProjectSimulationSuite) selected.getSegment(0);
				SimulationSuiteElement simulationSuiteElement = (SimulationSuiteElement) selected.getSegment(1);
				NewExperimentDialog dialog = new NewExperimentDialog(util.getProjectModels(projectSuite.getProject()), suiteViewer.getControl().getShell(),true,simulationSuiteElement.getSimulationExperiment() );
				if (dialog.open()==Window.OK) {
					simulationSuiteElement.setExperiment( dialog.getSimulationExperiment() );
					suiteViewer.refresh();
				}
			}
		}
	}

	private void makeRunExperimentAction() {
		runExperiment = new Action() {
			public void run() {
				doRunSimulationExperiment();
			}
		};
		runExperiment.setText("Run Experiment");
		runExperiment.setToolTipText("Run an experiment.");
		runExperiment.setImageDescriptor(
				ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_RUN_EXERIMENT_ID)
		);
	}

	protected void doRunSimulationExperiment() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			if (selected.getSegmentCount()>=2) {
				SimulationSuiteElement simulationSuiteElement = (SimulationSuiteElement) selected.getSegment(1);
				RunSimulationExperimentJob experiment = new RunSimulationExperimentJob(this, simulationSuiteElement);
				
				//update the Lab View
				//updateView();
				
				experiment.setUser(true);
				experiment.schedule();
			}
		}

	}

	private void makePlotExperimentAction() {
		plotExperimentData = new Action() {
			public void run() {
				SimulationOutcome result = (SimulationOutcome) resultViewer.getStructuredSelection().getFirstElement();
				if (result != null) {
					Display.getDefault().asyncExec(new Runnable() { 
						public void run() {				
							try {								
								ExperimentResultsView view = (ExperimentResultsView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( ExperimentResultsView.ID );
								LinkedList<SimulationTrace> data = new LinkedList<>();
								for (SimulationTimeSeries series : result.getCollectedData()) {
									data.add( new SimulationTrace(series) );
								}
								view.showData( data );
							} catch (PartInitException e) {
								MessageDialog.openError(PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Internal error...", e.getMessage());
							}				
						}
					});
				}
			}
		};
		plotExperimentData.setText("Plot Experiment Data");
		plotExperimentData.setToolTipText("Plot experiment data.");
		plotExperimentData.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_PLOT_EXERIMENT_ID)
		);
	}

	private void makeCopyExperimentAction() {
		copyExperiment = new Action() {
			public void run() {
				doCopyExperiment();
			}
		};
		copyExperiment.setText("Duplicate experiment");
		copyExperiment.setToolTipText("Duplicate experiment.");
		copyExperiment.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_COPY_EXERIMENT_ID)
		);
	}

	protected void doCopyExperiment() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			if (selected.getSegmentCount()>=2) {
				ProjectSimulationSuite projectSuite = (ProjectSimulationSuite) selected.getSegment(0);
				SimulationSuiteElement simulationSuiteElement = (SimulationSuiteElement) selected.getSegment(1);
				projectSuite.add( simulationSuiteElement.getSimulationExperiment().copy() );
				suiteViewer.refresh();
			}
		}
	}

	private void makeExportExperimentAction() {
		exportExperimentData = new Action() {
			public void run() {
				doExportExperimentData();
			}
		};
		exportExperimentData.setText("Export Data");
		exportExperimentData.setToolTipText("Export Data in a CSV file.");
		exportExperimentData.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_EXPORT_EXERIMENT_ID)
		);
	}

	private void makeShowExperimentDataAction() {
		showExperimentData = new Action() {
			public void run() {
				doShowExperimentData();
			}
		};
		showExperimentData.setText("Show Data");
		showExperimentData.setToolTipText("Show data collected in the simulation.");
		showExperimentData.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_SHOW_EXPERIMENT_DATA_ID)
		);
	}


	protected void doShowExperimentData() {
		SimulationOutcome result = (SimulationOutcome) resultViewer.getStructuredSelection().getFirstElement();
		if (result != null) {
			ShowDataDialog dialog = new ShowDataDialog( suiteViewer.getControl().getShell() , result );
			dialog.open();
		}
	}

	protected void doExportExperimentData() {
		try {
			TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
			
			if ((paths != null)&&(paths.length>0)) {
				TreePath selected = paths[0];			
				ProjectSimulationSuite projectSuite = (ProjectSimulationSuite) selected.getFirstSegment();
				IFolder folder = projectSuite.getProject().getFolder("output");
				if (!folder.exists()) {
					folder.create(true, true,null);
				}
				SimulationOutcome result = (SimulationOutcome) resultViewer.getStructuredSelection().getFirstElement();
				if (result != null) {
//					DirectoryDialog dialog = new DirectoryDialog(suiteViewer.getControl().getShell(), SWT.SHEET);
//					dialog.setMessage("Select the location");
//					String locationstr = dialog.open();
//					if (locationstr != null) {
//							File parent = new File(locationstr);
							for (SimulationTimeSeries serie : result.getCollectedData()) {
								StringWriter sWriter = new StringWriter();
								PrintWriter writer = new PrintWriter(sWriter);
//								PrintWriter writer = new PrintWriter(new File(parent, serie.getName()+(result.getStartingTime().replace(' ', '_').replace('/', '_'))+".csv"));
								serie.writeToCSV(writer);
								writer.close();
								IFile outputFile = projectSuite.getProject().getFile("output/"+serie.getName()+(result.getStartingTime().replace(' ', '_').replace('/', '_'))+".csv");
								if (outputFile.exists()) {
									outputFile.setContents( new ByteArrayInputStream(sWriter.toString().getBytes()) , true , false , null );
								} else {
									outputFile.create( new ByteArrayInputStream(sWriter.toString().getBytes()) , true , null);
								}
							}
							MessageDialog.openInformation(
									suiteViewer.getControl().getShell(),
									"Info...",
									"Data saved!");										
//					}
				}
			}					
		} catch (CoreException e) {
			MessageDialog.openError(
					suiteViewer.getControl().getShell(),
					"Error...",
					e.getMessage());					
		}

	}

	private void makeSaveLaboratoryAction() {
		saveLaboratory = new Action() {
			public void run() {
				doSaveLaboratory();
			}
		};
		saveLaboratory.setText("Save simulation suite");
		saveLaboratory.setToolTipText("Save Simulation Suite.");
		saveLaboratory.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_SAVE_SUITE_ID)
		);
	}

	protected void doSaveLaboratory() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			ProjectSimulationSuite projectSuite = (ProjectSimulationSuite) selected.getFirstSegment();
			util.saveProjectSimulationSuite( projectSuite );
		}

	}

	protected void doSaveAllLaboratory() {
		util.saveWorkspaceSimulationSuite( workSpaceSimulationSuite );
	}

	protected void doRemoveLaboratory() {
		TreePath[] paths = suiteViewer.getStructuredSelection().getPaths();
		if ((paths != null)&&(paths.length>0)) {
			TreePath selected = paths[0];			
			if (selected.getSegmentCount()>=2) {
				ProjectSimulationSuite projectSuite = (ProjectSimulationSuite) selected.getSegment(0);
				SimulationSuiteElement simulationSuiteElement = (SimulationSuiteElement) selected.getSegment(1);
				if (showConfirmMessage("Deleting simulation experiment...", "Selected simulation experiment will be deleted. Do you want to proceed?")) {
					projectSuite.remove( simulationSuiteElement );	
					suiteViewer.refresh();
				}
			}
		}
	}

	private void makeSaveAllLaboratoryAction() {
		saveAllLaboratory = new Action() {
			public void run() {
				doSaveAllLaboratory();
			}
		};
		saveAllLaboratory.setText("Save All Simulation Suites");
		saveAllLaboratory.setToolTipText("Save all simulation suite.");
		saveAllLaboratory.setImageDescriptor(
			ExtendedCARMAActivator.getInstance().getImageRegistry().getDescriptor(ExtendedCARMAActivator.IMG_SAVEALL_SUITE_ID)
		);
	}

	
	private void showMessage(String message) {
		MessageDialog.openInformation(
			suiteViewer.getControl().getShell(),
			"Model View",
			message);
	}
	
	private boolean showConfirmMessage( String title , String message ) {
		return MessageDialog.openConfirm(suiteViewer.getControl().getShell(), title, message);
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				SimulationView.this.fillContextMenu(manager);
			}
		});
//		Menu menu = menuMgr.createContextMenu(viewer.getControl());
//		viewer.getControl().setMenu(menu);
//		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(addExperiment);
		manager.add(removeExperiment);
		manager.add(editExperiment);
		manager.add(runExperiment);
		manager.add(plotExperimentData);
		manager.add(copyExperiment);
		manager.add(exportExperimentData);
		manager.add(saveLaboratory);
		manager.add(saveAllLaboratory);
//		manager.add(new Separator());
//		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(addExperiment);
		manager.add(removeExperiment);
		manager.add(editExperiment);
		manager.add(runExperiment);
		manager.add(plotExperimentData);
		manager.add(copyExperiment);
		manager.add(exportExperimentData);
		manager.add(saveLaboratory);
		manager.add(saveAllLaboratory);
//		manager.add(action2);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(addExperiment);
		manager.add(removeExperiment);
		manager.add(editExperiment);
		manager.add(runExperiment);
		manager.add(plotExperimentData);
		manager.add(copyExperiment);
		manager.add(exportExperimentData);
		manager.add(saveLaboratory);
		manager.add(saveAllLaboratory);
//		manager.add(action2);
	}

	private void hookDoubleClickAction() {
//		viewer.addDoubleClickListener(new IDoubleClickListener() {
//			public void doubleClick(DoubleClickEvent event) {
//				doubleClickAction.run();
//			}
//		});
	}

	public void refreshData() {
		showSimulationResults();
		this.suiteViewer.refresh();
		this.resultViewer.refresh();
	}

	public void addExperiment(SimulationExperiment experiment) {
		this.workSpaceSimulationSuite.addExperiment( experiment );
		refreshData();
	}
	
}
