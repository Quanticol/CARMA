package eu.quanticol.carma.ui.views;

import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import eu.quanticol.carma.simulator.CarmaSystem;

public class ResultViewerWidget {
	

	private TableViewer viewer;
	private ResultsProvider rp;
	
	public ResultViewerWidget(Composite parent, StatisticSampling<CarmaSystem> ss, int sampling) {
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
		    | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		createColumns(parent, ss, viewer);
		final Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		rp = new ResultsProvider(ss, sampling);
		
		viewer.setContentProvider(new ArrayContentProvider());
		viewer.setInput(rp.getResults());

		GridData gridData = new GridData();
		gridData.verticalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		viewer.getControl().setLayoutData(gridData);
	}
	
	private void createColumns(final Composite parent, StatisticSampling<CarmaSystem> ss, final TableViewer viewer) {
		String[] titles = { ss.getSimulationTimeSeries().get(0).getName(), "Time", "Mean", "Standard Deviation" };
		int[] bounds = { 100, 100, 100, 100 };
		
		// first column is for the first name
		TableViewerColumn col = createTableViewerColumn(titles[0], bounds[0], 0);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
			    return "";
			}
		});
		
		col = createTableViewerColumn(titles[1], bounds[1], 1);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Result r = (Result) element;
				return r.getT()+"";
			}
		});
		
		col = createTableViewerColumn(titles[2], bounds[2], 2);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Result r = (Result) element;
				return r.getM()+"";
			}
		});
		
		col = createTableViewerColumn(titles[3], bounds[3], 3);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Result r = (Result) element;
				return r.getSd()+"";
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
