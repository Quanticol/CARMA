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

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;

import eu.quanticol.carma.core.ui.data.MeasureData;
import eu.quanticol.carma.core.ui.data.SimulationOutcome;
import eu.quanticol.carma.simulator.CarmaModel;

public class CARMACommandLine {
	static String experimentFile;
	static String outputFolder = null;
	static boolean fileEnded = false;
	static boolean verbose = true;
		
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
		reader = new BufferedReader(new FileReader(filename));
		while (!fileEnded) {
			CommandLineSimulation sim = readSingleExperiment(reader);
			if (sim != null)
				experiments.add(sim);
			else
				System.out.println("Disregarding experiment.");
		}
		reader.close();
		report("Read " + experiments.size() + " experiment specification(s).");
		return experiments;
	}
	
	private static CommandLineSimulation readSingleExperiment(BufferedReader reader) throws IOException {
		String name = reader.readLine();
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
		System.out.println("Not supported yet, coming soon.");
		return null;
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
			//URL rootUrl = root.toURI().toURL();
			URL rootUrl = root.getParentFile().toURI().toURL();
			
			//TODO can this be done more elegantly?
			// (both manipulating the classloader, and assuming that the package name is ms)
			// currently, we need to change the classloader or it won't recognise the
			// compiled class to be a subclass of CarmaModel (although this works fine when
			// not run in a jar, for some reason)
			//URLClassLoader classLoader = URLClassLoader.newInstance(new URL[] { rootUrl });
			URLClassLoader classLoader = URLClassLoader.newInstance(new URL[] { rootUrl },
					Thread.currentThread().getContextClassLoader());
			Thread.currentThread().setContextClassLoader(classLoader);
			//String className = modelName.replace(".java", "");
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
			// perform simulation...
			sim.execute(verbose);
			// ...and save output
			writeOutput(sim);
			report("Finished experiment.");
		}

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
					+ "[-output <output_directory>] [-quiet]\n\n"
					+ "Optional parameters:\n"
					+ "-output (-out, -o): specify a location to store experiment results.\n"
					+ "-quiet (-q)       : only print warning and error messages.\n";
		
		System.out.println(helpMessage);
	}
	
	public static void main(String[] args) {
		// read arguments
		// based on the arguments, choose the right option
		// (currently only simulation is available but in the future other options may be supported)
		parseSimulationArguments(args);
		performSimulation();
		
	}
	
}
