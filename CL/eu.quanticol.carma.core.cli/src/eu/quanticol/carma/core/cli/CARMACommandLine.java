package eu.quanticol.carma.core.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics;
import org.apache.commons.math3.stat.descriptive.StatisticalSummary;
import org.apache.commons.math3.stat.descriptive.StatisticalSummaryValues;
import org.apache.commons.math3.stat.descriptive.SummaryStatistics;
import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;

import eu.quanticol.carma.core.ModelLoader;
import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.simulator.CarmaModel;

public class CARMACommandLine {
	static String experimentFile;
	static String outputFolder = null;
	static boolean fileEnded = false;
	static boolean verbose = true;
	static boolean multithreaded = false;
	static int nThreads = 1;
	private static long timeElapsed = 0;
	
	private static void parseSimulationArguments(String[] args) {
		boolean unrecognised = false;
		if (args.length == 0 || "-help".equals(args[0]) || "-h".equals(args[0])) {
			printHelp();
			System.exit(0);
		}
		experimentFile = args[0];
		for (int i = 1; i < args.length; i++) {
			switch(args[i]) {
			case "-o":
			case "-out":
			case "-output":
				if (i+1 <= args.length) {
					outputFolder = args[++i];
				}
				else
					System.out.println("Output flag was used but no output folder given.");
				break;
			case "-m":
			case "-multi":
			case "-multithreaded":
				multithreaded = true;
				try {
					nThreads = Integer.parseInt(args[++i]);
					if (nThreads <= 0) {
						System.out.println("Number of threads must be at least 1.");
						nThreads = 1;
					}
				}
				catch (Exception e) {
					System.out.println("Could not understand number of cores (" + args[i] +
							"). Running single-threaded instead.");
					multithreaded = false;
				}
				break;
			case "-quiet":
			case "-q":
				verbose = false;
				break;
			default: {
				System.out.println("Unrecognised option: " + args[i] + " (ignoring).");
				unrecognised = true;
			}
			}
		}
		if (unrecognised)
			printHelp();
	}
	
	private static List<CommandLineSimulation> readExperimentsFile(String filename)
		throws IOException {
		List<CommandLineSimulation> experiments = new ArrayList<CommandLineSimulation>();
		BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(filename));
		}
		catch (Exception e) {
			System.out.println("Error when reading experiments file (details below). Exiting.");
			System.out.println("Error encountered:");
			e.printStackTrace();
			return experiments;
		}
		while (!fileEnded) {
			CommandLineSimulation sim = readSingleExperiment(reader);
			if (sim != null)
				experiments.add(sim);
			else if (!fileEnded)
				System.out.println("Disregarding experiment.");
		}
		reader.close();
		report("Read " + experiments.size() + " experiment specification(s).");
		return experiments;
	}
	
	private static CommandLineSimulation readSingleExperiment(BufferedReader reader) throws IOException {
		String name;
		// disregard any empty lines between the previous experiment and this, also ensuring
		// that there is still an experiment description left (ie we are not at EOF)
		do {
			name = reader.readLine();
			if (name == null) {
				fileEnded = true;
				return null;
			}
		} while (name.isEmpty());
		String modelName = reader.readLine();
		CarmaModel model = getCARMAModel(modelName);
		if (model == null) {
			System.out.println("Could not read model.");
			return null;
		}
		String system = reader.readLine();
		int reps = Integer.parseInt(reader.readLine());
		double stopTime = Double.parseDouble(reader.readLine());
		int samplings = Integer.parseInt(reader.readLine());
		List<MeasureData> measures = readMeasures(reader,model);
		
		CommandLineSimulation sim = new CommandLineSimulation(name, model, system, reps,
				stopTime, samplings, measures);
		return sim;
	}
	
	private static CarmaModel getCARMAModel(String name) {
		if (name.endsWith(".java")) {
			warn("the compiled file " + name + 
					" may be out of date. Consider using the original CARMA model.");
			return loadCARMAModelFromJava(name);
		}
		else if (name.endsWith(".carma")) {
			return loadCARMAModelFromCARMA(name);
		}
		else {
			System.out.println("Could not read model file. Please specify either a .carma or .java file.");
			return null;
		}
	}

	private static CarmaModel loadCARMAModelFromCARMA(String name) {
		try {
			ModelLoader loader = new ModelLoader();
			return loader.load(name);
		} catch (Exception e) {
			System.out.println("Problem when loading model from file " + name + ":");
			e.printStackTrace();
			return null;
		}
	}
	
	private static CarmaModel loadCARMAModelFromJava(String name) {
		Path modelPath = Paths.get(name);
		String modelName = modelPath.getFileName().toString();
		// filename of log just replaces .java extension with .log
		String logName = modelName.substring(0,modelName.lastIndexOf(".")) + ".log";
		OutputStream stream = null;
		try {
			stream = new FileOutputStream(logName);
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			System.exit(-1);
		}
		
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		int res = compiler.run(null, null, stream, name, "-source","1.8");
		if(res==0) {
			report("Java file successfully compiled.");
		}
		else {
			System.out.println("Compilation failed: " + res + ". See log file for details.");
			return null;
		}
		
		// Load and instantiate compiled class.
		CarmaModel model = null;
		try {
			//avoid having null parent if the path is relative and doesn't specify a parent
			File root = modelPath.toAbsolutePath().getParent().toFile();
			URL rootUrl = root.getParentFile().toURI().toURL();
			
			//TODO can this be done more elegantly?
			// (both manipulating the classloader, and assuming that the package name is ms)
			URLClassLoader classLoader = URLClassLoader.newInstance(new URL[] { rootUrl },
					CarmaModel.class.getClassLoader());
			String className = "ms." + modelName.replace(".java", "");
			Class<?> cls = Class.forName(className, true, classLoader);
			
			Object inst = cls.newInstance();
			model = (CarmaModel) inst;
			return model;
		} catch (MalformedURLException | ClassNotFoundException | InstantiationException | IllegalAccessException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private static List<MeasureData> readMeasures(BufferedReader reader, CarmaModel model)
		throws IOException {
		List<String> allMeasures = Arrays.asList(model.getMeasures());
		List<MeasureData> measureList = new ArrayList<MeasureData>();
		String measureSpec = reader.readLine();
		while (measureSpec != null && !measureSpec.isEmpty()) {
			// measure specifications should be written as:
			// measureName (if no parameters are required)
			// or: measureName : parName = parValue
			// or: measureName : parName1 = parValue1, parName2 = parValue2, ... 
			String[] parts = measureSpec.split(":");
			String measureName = parts[0].trim();
			if (!allMeasures.contains(measureName)) {
				System.out.println("Measure " + measureName + " not found (ignoring).");
			}
			else if (parts.length > 2) {
				System.out.println("Invalid measure specification " + measureSpec + "(ignoring)");
			}
			else {
				// parse the specification 
				HashMap<String,Object> measurePars = new HashMap<String,Object>();
				if (parts.length == 2) {
					// the measure has parameters, read them one by one
					String[] parSpecs = parts[1].split(",");
					Map<String,Class<?>> parClasses = model.getParametersType(measureName);
					for (String parSpec : parSpecs) {
						int eqInd = parSpec.indexOf("=");
						String parName = parSpec.substring(0, eqInd).trim();
						String parValue = parSpec.substring(eqInd+1).trim();
						if (!parClasses.containsKey(parName)) {
							System.out.println("Unrecognised parameter " + parName +
									" for measure " + measureName + "  (ignoring).");
							continue;
						}
						Object valueToPut;
						if (parClasses.get(parName).equals(Integer.class))
							valueToPut = Integer.parseInt(parValue);
						else
							valueToPut = Double.parseDouble(parValue);
						measurePars.put(parName, valueToPut);
					}
				}
				// add the measure if all the necessary parameters have been given
				if (measurePars.size() != model.getMeasureParameters(measureName).length)
					System.out.println("Measure " + measureName + " requires more parameters.");
				else
					measureList.add(new MeasureData(measureName,measurePars));
			}
			measureSpec = reader.readLine();
		}
		if (measureSpec == null) {
			fileEnded = true;
		}
		return measureList;
	}
	
	private static void performSimulation() {
		// Set up experiments:
		List<CommandLineSimulation> allSims = null;
		try {
			allSims = readExperimentsFile(experimentFile);
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		// Run each experiment:
		for (CommandLineSimulation sim : allSims) {
			report("\nStarting experiment " + sim.getName() + ".");
			long startTime = System.currentTimeMillis();
			// perform simulation...
			if (!multithreaded) {
				sim.execute(verbose);
			}
			else {
				sim = performMultithreadedSimulation(sim);
			}
			long stopTime = System.currentTimeMillis();
			timeElapsed  += stopTime - startTime;
			// ...and save output
			writeOutput(sim);
			report("Finished experiment.");
		}

	}

	private static CommandLineSimulation performMultithreadedSimulation(CommandLineSimulation sim) {
		// split simulation and assign to each thread
		List<CommandLineSimulation> subtasks = new ArrayList<CommandLineSimulation>();
		int replicationsEach = sim.getReplications() / nThreads;
		CommandLineSimulation subtask = sim.copy();
		subtask.setReplications(replicationsEach);
		for(int i = 0; i < nThreads - 1; i++) {
			subtasks.add(subtask.copy());
		}
//				int replicationsRemaining = replicationsEach + sim.getReplications() % nThreads;
		subtask.setReplications(replicationsEach + sim.getReplications() % nThreads);
		subtasks.add(subtask);
		
		// create threads / assign to cores
		//TODO consider workStealingPool instead?
		ExecutorService executor = Executors.newFixedThreadPool(nThreads);
		// Perhaps:
		//calls = subtasks.stream().map(t -> makeCallable(t))
		//		.collect(Collectors.toList());
		// or simply:
		List<? extends Callable<Object>> calls = new ArrayList<Callable<Object>>();
		calls = subtasks;
		try {
			executor.invokeAll(calls);
		}
		catch (InterruptedException e) {
			System.out.println("Error encountered while executing subtasks:");
			e.printStackTrace();
			System.out.println("Quitting.");
			System.exit(-1);
		}
		executor.shutdown();
		
		// merge results				
		int nResults = sim.getMeasures().size();
		List<SimulationTimeSeries> allResults = new ArrayList<SimulationTimeSeries>(nResults);

		for (int i = 0; i < nResults; i++) {
			//create aggregate
			int nPoints = sim.getSamplings() + 1;
			List<List<SummaryStatistics>> allStatistics =
					new ArrayList<List<SummaryStatistics>>(nPoints);
			//allStatistics(j,k) will hold results for the k-th thread at the j-th sampling time
			for (int j = 0; j < nPoints; j++) {
				allStatistics.add(new ArrayList<SummaryStatistics>());
			}
			for (CommandLineSimulation task : subtasks) {
				StatisticalSummary[] data = task.getResult(0).getCollectedData().get(i).getData();
				for (int j = 0; j < data.length; j++) {
					allStatistics.get(j).add((SummaryStatistics) data[j]);
				}
			}
			
			StatisticalSummaryValues[] aggregate = new StatisticalSummaryValues[nPoints];
			for (int j = 0; j < aggregate.length; j++) {
				aggregate[j] = AggregateSummaryStatistics.aggregate(allStatistics.get(j));
			}
			
			String name = subtasks.get(0).getResult(0).getCollectedData().get(i).getName();
			double dt = sim.getSimulationTime() / sim.getSamplings();
			allResults.add(new SimulationTimeSeries(name,dt,sim.getReplications(),aggregate));
		}
		
		// and create the final summary simulation
		CommandLineSimulation finalSim = sim.copy();
		//TODO change starting, total and average time in constructor of outcome?
		SimulationOutcome outcome = new SimulationOutcome("",0,0,allResults);
		List<SimulationOutcome> finalResult = new ArrayList<SimulationOutcome>(1);
		finalResult.add(outcome);
		finalSim.setResults(finalResult);
		report("Aggregated results");
		return finalSim;
	}
	
	private static void writeOutput(CommandLineSimulation sim) {
		//TODO Check that this never happens
		if (sim.getResults().size() > 1) {
			System.out.println("Too many results: " + sim.getResults().size());
			return;
		}
		if (sim.getResults() == null || sim.getResults().isEmpty()) {
			System.out.println("No results found.");
			return;
		}
		SimulationOutcome result = sim.getResult(0);
		Path outputBase;
		if (outputFolder == null)
			outputBase = Paths.get("results").resolve(sim.getName());
		else
			outputBase = Paths.get(outputFolder).resolve("results").resolve(sim.getName());
		if (Files.exists(outputBase)) {
			warn("folder for experiment " + sim.getName() + " already exists.");
		}
		try {
			Files.createDirectories(outputBase);
			//		} catch (FileAlreadyExistsException e) {
			//			System.out.println("Warning: folder for experiment " + sim.getName() + 
			//					" already exists.");
		} catch (IOException e) {
			System.out.println("Could not create output folder: " + outputBase + ".");
			//e.printStackTrace();
			return;
		}

		for (SimulationTimeSeries ts : result.getCollectedData()) {
			Path measureFile = outputBase.resolve(ts.getName());
			if (Files.exists(measureFile)) {
				warn("will overwrite file " + measureFile + ".");
			}
			PrintWriter writer = null;
			try {
				writer = new PrintWriter(measureFile.toFile());
				ts.writeToCSV(writer);
			} catch (FileNotFoundException e) {
				System.err.println("Could not create file: " + measureFile + ".");
			} finally {
				if (writer != null)
					writer.close();
			}
		}
		report("Wrote experiment results at " + outputBase.toAbsolutePath() + ".");	
	}
	
	private static void report(String msg) {
		if (verbose) {
			System.out.println(msg);
		}
	}
	
	private static void warn(String msg) {
		System.out.println("Warning: " + msg);
	}
	
	private static void printHelp() {
		String helpMessage = "Usage: java CARMACommandLine <experiments_file> "
					+ "[-output <output_directory>] [-quiet] [-multithreaded N]\n\n"
					+ "Optional parameters:\n"
					+ "-output (-out, -o) : specify a location to store experiment results.\n"
					+ "-quiet (-q)        : only print warning and error messages.\n"
					+ "-multithreaded (-m): spread the task into the specified number of threads.\n";
		
		System.out.println(helpMessage);
	}
	
	public static void main(String[] args) {
		// read arguments
		// based on the arguments, choose the right option
		// (currently only simulation is available but in the future other options may be supported)
		parseSimulationArguments(args);
		performSimulation();
		
	}
	
	public static long getTimeElapsed() {
		return timeElapsed;
	}
	
	/**
	 * Resets the fields of the class, so it can be called repeatedly from other code.
	 */
	public static void reset() {
		experimentFile = null;
		outputFolder = null;
		fileEnded = false;
		verbose = true;
		multithreaded = false;
		nThreads = 1;
		timeElapsed = 0;
	}
}