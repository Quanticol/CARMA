package eu.quanticol.carma.ui.views;

import java.util.ArrayList;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import eu.quanticol.carma.ui.laboratory.ExperimentJob;

public class ExperimentJobWidget {
	

	private TableViewer viewer;
	private ArrayList<ExperimentJob> ejbs;
	
	public ExperimentJobWidget(Composite parent, ExperimentJob experimentJob) {
		
		FillLayout containerLayout = new FillLayout();
		Composite container = new Composite(parent, SWT.NONE);
		container.setLayout(containerLayout);
		
		viewer = new TableViewer(container, SWT.MULTI | SWT.H_SCROLL
		    | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		createColumns(container, viewer);
		final Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		ejbs = new ArrayList<ExperimentJob>();
		ejbs.add(experimentJob);
		
		viewer.setContentProvider(new ArrayContentProvider());
		viewer.setInput(ejbs);

		GridData gridData = new GridData();
		gridData.verticalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		viewer.getControl().setLayoutData(gridData);
		
	}
	
	private String getList(String[] strings){
		String toReturn = "";
		if(strings.length > 0)
			toReturn = strings[0];
		if(strings.length > 1)
			toReturn = toReturn + " \n";
			for(int i = 1; i < strings.length; i++)
				toReturn = toReturn + strings[i] + " \n";
		return toReturn;
	}
	
	private void createColumns(final Composite parent, final TableViewer viewer) {
		String[] titles = { "Model", "System", "Measures", "Deadline", "Iterations", "Sampling" };
		int[] bounds = { 100, 100, 100, 100, 100, 100 };
		
		// first column is for the first name
		TableViewerColumn col = createTableViewerColumn(titles[0], bounds[0], 0);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
			    return ej.getModelName();
			}
		});
		
		col = createTableViewerColumn(titles[1], bounds[1], 1);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
				return ej.getSystem();
			}
		});
		
		col = createTableViewerColumn(titles[2], bounds[2], 2);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
				//TODO
				return getList(ej.getMeasures());
			}
		});
		
		col = createTableViewerColumn(titles[3], bounds[3], 3);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
				return ej.getDeadline()+"";
			}
		});
		
		col = createTableViewerColumn(titles[4], bounds[4], 5);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
				return ej.getIterations()+"";
			}
		});
		
		
		col = createTableViewerColumn(titles[5], bounds[5], 5);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				ExperimentJob ej = (ExperimentJob) element;
				return ej.getSamples()+"";
			}
		});
	}
	
	private TableViewerColumn createTableViewerColumn(String title, int bound, final int colNumber) {
		final TableViewerColumn viewerColumn = new TableViewerColumn(viewer,
		    SWT.NONE);
		final TableColumn column = viewerColumn.getColumn();
		column.setText(title);
		column.setWidth(bound);
		column.setResizable(true);
		column.setMoveable(true);
		return viewerColumn;
	}
}
