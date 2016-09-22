package eu.quanticol.carma.core.ui.views;


import java.util.LinkedList;
import java.util.List;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.nebula.visualization.xygraph.figures.IXYGraph;
import org.eclipse.nebula.visualization.xygraph.figures.ToolbarArmedXYGraph;
import org.eclipse.nebula.visualization.xygraph.figures.Trace;
import org.eclipse.nebula.visualization.xygraph.figures.XYGraph;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

public class ExperimentResultsView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "eu.quanticol.carma.core.ui.views.ExperimentResultsView";
	private static IXYGraph xyGraph;
	private Composite parent;
	private LinkedList<Trace> traces;

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
		this.parent = parent;
		FillLayout parentLayout = new FillLayout();
		parent.setLayout(parentLayout);
		createGraphContent( parent );
	}
	
	
	

	private static void createGraphContent( Composite parent ) {
		Canvas graphPanel = new Canvas(parent, SWT.NONE);
		// use LightweightSystem to create the bridge between SWT and draw2D
		final LightweightSystem lws = new LightweightSystem(graphPanel);

		// create a new XY Graph.
		xyGraph = new XYGraph();
		xyGraph.getPrimaryXAxis().setShowMajorGrid(true);
		xyGraph.getPrimaryXAxis().setAutoScale(true);
		xyGraph.getPrimaryYAxis().setShowMajorGrid(true);
		xyGraph.getPrimaryYAxis().setAutoScale(true);

		ToolbarArmedXYGraph toolbarArmedXYGraph = new ToolbarArmedXYGraph(xyGraph);

		//xyGraph.setTitle("Simple Toolbar Armed XYGraph Example");
		// set it as the content of LightwightSystem
		lws.setContents(toolbarArmedXYGraph);

//		xyGraph.primaryXAxis.setShowMajorGrid(true);
//		xyGraph.primaryYAxis.setShowMajorGrid(true);
//		xyGraph.primaryXAxis.setAutoScale(true);
//		xyGraph.primaryYAxis.setAutoScale(true);
//		xyGraph.primaryXAxis.setAutoScaleThreshold(0.0);
//		xyGraph.primaryYAxis.setAutoScaleThreshold(0.0);
		
		// create a trace data provider, which will provide the data to the
		// trace.
//		if(ExperimentResultsView.measures != null){
////				xyGraph.primaryXAxis.setRange(0, experimentJob.getDeadline());
////				xyGraph.primaryYAxis.setRange(0,100);
//				LinkedList<SimulationTimeSeries> data = experimentJob.getCollection().getSimulationTimeSeries();
//				for (SimulationTimeSeries serie : data) {
//					SimulationTrace traceDataProvider = new SimulationTrace(serie,experimentJob.getIterations());
//
//					// create the trace
//					Trace trace = new Trace(serie.getName(), xyGraph.primaryXAxis, xyGraph.primaryYAxis, traceDataProvider);
//
//					// set trace property
//					trace.setPointStyle(PointStyle.XCROSS);
//					trace.setErrorBarEnabled(true);
//					trace.setYErrorBarType(ErrorBarType.BOTH);
//					trace.setXErrorBarType(ErrorBarType.NONE);
//
//					// add the trace to xyGraph
//					xyGraph.addTrace(trace);
//				}
//		}

	}
	
	public void showData( List<SimulationTrace> timeSeries ) {
		clearTraces();
		for (SimulationTrace data: timeSeries) {
			Trace trace = new Trace(data.getName(), xyGraph.getPrimaryXAxis(), xyGraph.getPrimaryYAxis(), data ) ;
			xyGraph.addTrace( trace );
			traces.add(trace);
		}
		parent.redraw();
	}
	

	private void clearTraces() {
		if (traces != null) {
			for (Trace trace : traces) {
				xyGraph.removeTrace(trace);
			}
		}
		traces = new LinkedList<Trace>(  );
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}

}