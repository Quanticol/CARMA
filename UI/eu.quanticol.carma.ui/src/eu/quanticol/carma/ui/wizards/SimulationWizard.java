/**
 * 
 */
package eu.quanticol.carma.ui.wizards;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IProject;
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
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.ui.util.CarmaUiUtils;

/**
 * @author loreti
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
	protected LinkedList<Model> models;
	
	public SimulationWizard() {
		super();
		this.models = CarmaUiUtils.getActiveModels();
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
		    for (Model model : models) {
				combo.add(model.eResource().getURI().lastSegment().split("\\.")[0]);
			}
		    combo.addListener(SWT.Selection, new Listener() {
				public void handleEvent(Event event) {
					modelChoice = combo.getSelectionIndex();
					checkPage();
				}
			});
		    setControl(container);
			    

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
			
			Model model = models.get(modelChoice);
			String[] classPathEntries;
			String projectName = model.eResource().getURI().segments()[1];
			
			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			for(IProject project: projects){
				if(projectName.equals(project.getName())){
					
					try{
						project.open(null /* IProgressMonitor */);
						IJavaProject javaProject = JavaCore.create(project);
						classPathEntries = JavaRuntime.computeDefaultRuntimeClassPath(javaProject);
						
						
						
						List<URL> urlList = new ArrayList<URL>();
						for (int i = 0; i < classPathEntries.length; i++) {
							System.out.println(classPathEntries[i]);
							String entry = classPathEntries[i];
							IPath path = new Path(entry);
							URL url = path.toFile().toURI().toURL();
							urlList.add(url);
						}
						
						parentClassLoader = project.getClass().getClassLoader();
						URL[] urls = (URL[]) urlList.toArray(new URL[urlList.size()]);
						classLoader = new URLClassLoader(urls, parentClassLoader);
						
						clazz = classLoader.loadClass("ms."+model.eResource().getURI().lastSegment().split("\\.")[0]);
						carmaModel = (Object) clazz.newInstance();
						
					} catch (CoreException | MalformedURLException | ClassNotFoundException | InstantiationException | IllegalAccessException | ClassCastException e){
						System.out.println(e);
					}
				}
			}
			

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

}
