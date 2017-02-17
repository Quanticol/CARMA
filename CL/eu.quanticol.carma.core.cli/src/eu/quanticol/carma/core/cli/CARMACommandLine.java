package eu.quanticol.carma.core.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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
import eu.quanticol.carma.simulator.CarmaComponent;
import eu.quanticol.carma.simulator.CarmaModel;
import eu.quanticol.carma.simulator.CarmaSystem;

public class CARMACommandLine {
	static String experimentFile;
	static String outputFolder = null;
	static boolean fileEnded = false;
	static boolean verbose = true;
	static boolean multithreaded = false;
	static int nThreads = 1;
	static boolean seedSet = false;
	static long baseSeed;
	static boolean deadlineSet = false;
	static double deadline;
	static boolean replicationsSet = false;
	static int nReplications;
	private static long timeElapsed = 0; // time in nanoseconds
	
	private static enum Mode {Simulation, Multivesta, Help, Summary, None};
	
	private static Mode getMode(String[] args) {
		if (args.length == 0) {
			return Mode.Help;
		}
		switch(args[0]) {
		case "multivesta":
			return Mode.Multivesta;
		case "simulate":
		case "sim":
			return Mode.Simulation;
		case "summary":
			return Mode.Summary;
		case "help":
		case "-help":
		case "--help":
		case "h":
		case "-h":
		case "--h":
			return Mode.Help;
		default:
			return Mode.None;
			
		}
	}
	
	private static void parseSimulationArguments(String[] args, boolean skipFirst) {
		boolean unrecognised = false;
		if (args.length == 0 || "-help".equals(args[0]) || "-h".equals(args[0])) {
			printHelp();
			System.exit(0);
		}
		int i = skipFirst ? 1 : 0;
		experimentFile = args[i++];
		for (; i < args.length; i++) {
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
				if (i+1 <= args.length) {
					try {
						nThreads = Integer.parseInt(args[++i]);
						if (nThreads <= 0) {
							System.out.println("Number of threads must be at least 1. "
									+ "Running single-threaded instead.");
							multithreaded = false;
						} else {
						multithreaded = true;
						}
					}
					catch (Exception e) {
						System.out.println("Could not understand number of cores (" + args[i] +
								"). Running single-threaded instead.");
//						multithreaded = false;
					}
				} else {
					System.out.println("Multithreaded option was used but no number of threads given.");
				}
				break;
			case "-seed":
				if (i+1 <= args.length) {
					try {
						baseSeed = Long.parseLong(args[++i]);
						seedSet = true;
					}
					catch (NumberFormatException e) {
						System.out.println("Could not understand initial seed (" + args[i] +
								"). Ignoring.");
					}
				} else {
					System.out.println("Seed option was used but no initial seed given.");
				}
				break;
			case "-t":
			case "-time":
			case "-deadline":
				if (i+1 <= args.length) {
					try {
						deadline = Double.parseDouble(args[++i]);
						deadlineSet = true;
					}
					catch (NumberFormatException e) {
						System.out.println("Could not understand simulation final time (" + args[i] +
								"). Ignoring.");
					}
				} else {
					System.out.println("Final time option was used but no final time given.");
				}
				break;
			case "-reps":
			case "-replications":
				if (i+1 <= args.length) {
					try {
						nReplications = Integer.parseInt(args[++i]);
						replicationsSet = true;
					}
					catch (NumberFormatException e) {
						System.out.println("Could not understand number of replications (" + args[i] +
								"). Ignoring.");
					}
				} else {
					System.out.println("Replications option was used but no number given.");
				}
				break;
			case "-quiet":
			case "-q":
				verbose = false;
				break;
			default:
				System.out.println("Unrecognised option: " + args[i] + " (ignoring).");
				unrecognised = true;
			}
		}
		if (unrecognised)
			printHelp();
		if (seedSet || deadlineSet || replicationsSet) {
			warn("overridden simulation parameters (seed, stop time, number of replications)"
					+ " will apply to all experiments in the file!");
		}
	}
	
	private static MultivestaExperiment parseMultivestaArguments(String[] args) {
		boolean unrecognised = false;
		String modelFile = args[1];
		String queryFile = args[2];
		for (int i = 3; i < args.length; i++) {
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
		return new MultivestaExperiment(modelFile,queryFile,outputFolder);
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
			System.out.println("Could not read model from " + modelName + ".");
			// continue reading until you find blank line or EOF (disregard experiment)
			String line;
			do {
				line = reader.readLine();
			} while (line != null && !line.isEmpty());
			return null;
		}
		String system = reader.readLine();
		int reps = Integer.parseInt(reader.readLine());
		double stopTime = Double.parseDouble(reader.readLine());
		int samplings = Integer.parseInt(reader.readLine());
		List<MeasureData> measures = readMeasures(reader,model);
		
		CommandLineSimulation sim = new CommandLineSimulation(name, model, system, reps,
				stopTime, samplings, measures, modelName);
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
		//int res = compiler.run(null, null, stream, name);
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
			// apply any additional parameters specified in the arguments...
			overrideSimulation(sim);
			long startTime = System.nanoTime();
			// ...perform simulation...
			if (!multithreaded) {
				if (seedSet) {
					sim.setSeed(baseSeed);
				}
				sim.execute(verbose);
			}
			else {
				sim = performMultithreadedSimulation(sim);
			}
			long stopTime = System.nanoTime();
			timeElapsed  = stopTime - startTime;
			report(String.format("Time elapsed: %s for %d replications",
					formatTime(timeElapsed),sim.getReplications()));
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
			CommandLineSimulation newTask = subtask.copy();
			newTask.setTaskName("Thread " + i);
			if (seedSet) {
				newTask.setSeed((i+1) * baseSeed);
			}
			subtasks.add(newTask);
		}
//		int replicationsRemaining = replicationsEach + sim.getReplications() % nThreads;
		subtask.setReplications(replicationsEach + sim.getReplications() % nThreads);
		subtask.setTaskName("Thread " + (nThreads-1));
		if (seedSet) {
			subtask.setSeed(nThreads * baseSeed);
		}
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
			
			//TODO This might be better to do in a static method of SimulationTimeSeries?
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
	
	private static void overrideSimulation(CommandLineSimulation sim) {
		if (deadlineSet) {
			sim.setSimulationTime(deadline);
		}
		if (replicationsSet) {
			sim.setReplications(nReplications);
		}
	}
	
	private static void writeOutput(CommandLineSimulation sim) {
		LocalDateTime finishDate = LocalDateTime.now();
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

		// Write simulation results:
		for (SimulationTimeSeries ts : result.getCollectedData()) {
			Path measureFile = outputBase.resolve(ts.getName() + ".csv");
			if (Files.exists(measureFile)) {
				warn("will overwrite file " + measureFile + ".");
			}
			try (PrintWriter writer = new PrintWriter(measureFile.toFile())) {
				ts.writeToCSV(writer);
			} catch (FileNotFoundException e) {
				System.err.println("Could not create file: " + measureFile + ".");
			}
		}
		
		// Write time taken:
		Path timeFile = outputBase.resolve("timeInSeconds");
		if (Files.exists(timeFile)) {
			warn("will overwrite file " + timeFile + ".");
		}
		try (PrintStream out = new PrintStream(timeFile.toFile())) {
			double time_s = timeElapsed / 1e9;
			out.print(String.format("%.5f",time_s));
		} catch (FileNotFoundException e) {
			System.err.println("Could not create file: " + timeFile + ".");
		}
		
		// Copy original model:
		Path originalPath = Paths.get(sim.getModelLocation());
		Path copyPath = outputBase.resolve(originalPath.getFileName());
		if (Files.exists(copyPath)) {
			warn("will overwrite file " + copyPath + ".");
		}
		boolean madeCopy = true;
		try {
			Files.copy(originalPath, copyPath, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e1) {
			System.err.println("Error when trying to copy model file.");
			madeCopy = false;
		}
		
		// Write experiment segment:
		Path experimentFile = outputBase.resolve("experimentFile");
		if (Files.exists(experimentFile)) {
			warn("will overwrite file " + experimentFile + ".");
		}
		try (PrintStream out = new PrintStream(experimentFile.toFile())) {
			out.println(sim.getName());
			out.println(sim.getModelLocation());
			out.println(sim.getSystem());
			out.println(sim.getReplications());
			out.println(sim.getSimulationTime());
			out.println(sim.getSamplings());
			for (MeasureData md : sim.getMeasures()) {
				out.print(md.getMeasureName());
				if (md.getParameters().size() > 0) {
					List<String> pars = new ArrayList<String>(md.getParameters().size());
					for (Map.Entry<String, Object> e : md.getParameters().entrySet()) {
						pars.add(e.getKey() + " = " + e.getValue());
					}
					out.print(": " + String.join(", ", pars));
				}
				out.println();
			}
		} catch (FileNotFoundException e) {
			System.err.println("Could not create file: " + timeFile + ".");
		}
		
		// Write summary:
		Path summaryFile = outputBase.resolve("info");
		if (Files.exists(summaryFile)) {
			warn("will overwrite file " + summaryFile + ".");
		}
		try(PrintWriter writer = new PrintWriter(summaryFile.toFile())) {
			String firstLine = "Summary for experiment " + sim.getName() + ":";
			writer.println(firstLine);
			writer.println(new String(new char[firstLine.length()-1]).replace('\0', '-'));
			writer.print("This experiment used the model " + sim.getModelLocation() + ".");
			if (madeCopy) {
					writer.println(" A copy has been saved in this directory.");
			} else {
				// just in case, e.g. the original file is deleted during the simulation
				writer.println("I failed to make a copy of the original model.");
			}
			writer.println("The scenario considered was " + sim.getSystem() + ".");
			writer.print("The experiment tracked the following measures: ");
			writer.println(sim.getMeasures().stream().map(MeasureData::toString)
					.collect(Collectors.joining(", ")) + ".");
			writer.println(String.format("The final time of the simulation was %.3f "
					+ "and %d samplings were taken (sampling interval: %.5f).",
					sim.getSimulationTime(),
					sim.getSamplings(),
					sim.getSimulationTime()/sim.getSamplings()));
			writer.println(sim.getReplications() + " replications were performed in "
					+ formatTime(timeElapsed) + " using the CARMA simulator.");
			if (multithreaded) {
				writer.println("The simulations were divided into " + nThreads + " batches.");
			}
			if (seedSet) {
				writer.println("The seed for the simulations was set to " + baseSeed + ".");
			}
			writer.println("The data from individual replications were combined and statistics"
					+ " (mean, variance) were computed using the Apache Commons Mathematics library.");
			writer.println(String.format("This experiment finished at %s on %s.",
					finishDate.format(DateTimeFormatter.ofLocalizedTime(FormatStyle.MEDIUM)),
					finishDate.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.LONG))));
			if (writer.checkError()) {
				System.err.println("Error when writing to file " + summaryFile);
			}
		} catch (FileNotFoundException e) {
			System.err.println("Could not create file: " + summaryFile + ".");
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
					+ "[-output <output_directory>] [-quiet] [-multithreaded N] "
					+ "[-seed N] [-deadline t] [-replications N]\n\n"
					+ "Optional parameters:\n"
					+ "-output (-out, -o)   : specify a location to store experiment results.\n"
					+ "-quiet (-q)          : only print warning and error messages.\n"
					+ "-multithreaded (-m)  : spread the task into the specified number of threads.\n"
					+ "-seed                : specify the random seed for the simulations.\n"
					+ "-deadline (-time, -t): specify the final time for the simulations.\n"
					+ "-replications (-reps): specify the number of replications.\n";
		
		System.out.println(helpMessage);
	}
	
	public static void main(String[] args) {
		// read arguments
		Mode mode = getMode(args);
		// based on the arguments, choose the right option
		switch(mode) {
		case Help:
			printHelp();
			break;
		case Simulation:
			parseSimulationArguments(args,true);
			performSimulation();
			break;
		case Multivesta:
			/*
			MultivestaExperiment exp = parseMultivestaArguments(args);
			try {
				exp.run();
			} catch (Exception e) {
				System.out.println("Could not run MultiVeStA experiment.\nError:");
				e.printStackTrace();
			}
			*/
			System.out.println("MultiVeStA integration coming soon.");
			break;
		case Summary:
			printSummary(args);
			break;
		case None:
			parseSimulationArguments(args,false);
			performSimulation();
		}
	}
	
	private static void printSummary(String[] args) {
		//parseSummaryArguments(args);
		if (args.length < 3) {
			System.out.println("No model given.\n");
			printHelp();
			return;
		}
		String name = args[2];
		boolean create_latex = false;
		boolean unrecognised = false;
		for(int i = 3; i < args.length; i++) {
			switch(args[i]) {
			case "-latex":
			case "-LaTeX":
				create_latex = true;
				break;
			case "-quiet":
			case "-q":
				verbose = false;
				break;
			default:
				System.out.println("Unrecognised option: " + args[i] + " (ignoring).");
				unrecognised = true;
			}
		}
		if (unrecognised) {
			printHelp();
		}
		CarmaModel model = getCARMAModel(name);
		if (model == null) {
			System.out.println("Could not read model from file " + name);
			return;
		}
		if (verbose) {
			printModelOutline(model);
		}
		if (create_latex) {
			String outputFile = null;
			printLatex(model,outputFile);
		}
	}

	private static void printModelOutline(CarmaModel m) {
		for(String sys_name : m.getSystems()) {
			System.out.println("System " + sys_name + ":");
			CarmaSystem system = m.getFactory(sys_name).getModel();
			System.out.println("Initial state:");
			for (CarmaComponent c : system.getCollective()) {
				System.out.print("Component " + c.getName() + 
						((c.getLocation() != null) ? ("at " + c.getLocation()) : ""));
			}
		}
	}
	
	private static void printLatex(CarmaModel m, String filename) {
		System.out.println("LaTeX generation not implemented yet.");
	}
	
	public static long getTimeElapsed() {
		return timeElapsed;
	}
	
	private static String formatTime(long time) {
		long time_ms = TimeUnit.NANOSECONDS.toMillis(time);
		if (time_ms < 1000) { // if up to 1 second, print time in milliseconds
			return time_ms + " ms";
		} else {
			double time_s = (double) time_ms / 1000;
			if (time_s < 300) { // if up to 5 minutes, print seconds
				return String.format("%.5f s", time_s);
			} else { // if more than 5 minutes, print minutes and seconds rounded down
				int minFull = (int) time_s / 60;
				int secRem = (int) time_s % 60;
				return String.format("%d m %d s",minFull,secRem);
			}
		}
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
		seedSet = false;
		deadlineSet = false;
		replicationsSet = false;
	}
}