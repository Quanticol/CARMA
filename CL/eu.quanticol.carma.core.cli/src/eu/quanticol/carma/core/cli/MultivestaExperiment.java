package eu.quanticol.carma.core.cli;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import eu.quanticol.carma.multivesta.EntryPointMultiVestaCARMA;

public class MultivestaExperiment {
	private static final String jarName = "multivesta-carma.jar";
	
	private String modelFile;
	private String queryFile;
	private String output;
	
	public MultivestaExperiment(String modelFile, String queryFile) {
		this(modelFile,queryFile,null);
	}
	
	public MultivestaExperiment(String modelFile, String queryFile, String output) {
		this.modelFile = modelFile;
		this.queryFile = queryFile;
		this.output = output;
	}
	
	public void run() throws IOException {
		Map<String,String> config = defaultConfiguration();
		config.put("-m", getModelName());
		config.put("-f", queryFile);
		if (output != null) {
			config.put("-op", output);
		}
		//config.put("-jn", getMultivestaJarLocation());
		config.put("-jn", "aaaaa!");
		config.put("-sd","eu.quanticol.carma.multivesta.CARMASimulatorState");
		EntryPointMultiVestaCARMA.main(paramSequence(config,true));
		//vesta.NewVesta.invokeClient(paramSequence(config,false));
	}
	
	private String getModelName() {
		return Paths.get(modelFile).toFile().getAbsolutePath();
	}
	
	private String getMultivestaJarLocation() throws IOException {
		String className = this.getClass().getName();
		int nSegs = className.split("\\.").length;
		URI loc = getCurrentLocation();
		if (loc.toString().startsWith("file:")) {
			// this assumes Eclipse-like structure in order to find the root folder
			Path p = Paths.get(loc);
			for (int i = 0; i < nSegs + 1; i++)
				p = p.getParent();
			p = p.resolve("lib").resolve(jarName);
			return p.toString();
		}
		else {//if jar:
			// must create the file system first
			String[] segs = loc.toString().split("!");
			String root = segs[0];
			Map<String,String> env = new HashMap<String,String>();
			FileSystem fs = FileSystems.newFileSystem(URI.create(root),env);
			String jarLoc = fs.getPath("lib",jarName).toString();
			fs.close();
			return jarLoc;
		}
	}
	
	private URI getCurrentLocation() {
		URI loc = null;
		try {
//			loc = this.getClass().getProtectionDomain().getCodeSource()
//					.getLocation().toURI();
			//String className = this.getClass().getName().replace(".", "/") + ".class";
			String[] classNameParts = this.getClass().getName().split("\\.");
			loc = this.getClass().getResource(classNameParts[classNameParts.length-1] + ".class")
					.toURI();
		return loc;
		} catch (URISyntaxException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private static Map<String,String> defaultConfiguration() {
		Map<String,String> config = new HashMap<String,String>();
		//config.put("-c",null);
		config.put("-sm", "false");
		config.put("-l", "1");
		config.put("-ir", "20");
		config.put("-bs", "20");
		config.put("-a", "0.1");
		config.put("-d1", "1.0");
		config.put("-osws", "ONESTEP");
		config.put("-sots", "12343");
		return config;
	}
	
	private static String[] paramSequence(Map<String,String> config, boolean includeC) {
		List<String> params = new ArrayList<String>();
		if (includeC)
			params.add("-c");
		for (Entry<String,String> e : config.entrySet()) {
			params.add(e.getKey());
			if (e.getValue() != null) { // exclude flags with no parameters
				params.add(e.getValue());
			}
		}
		return params.toArray(new String[params.size()]);
	}
	
	public void test() {
		try {
			System.out.println(getModelName());
			System.out.println(this.queryFile);
			System.out.println(getCurrentLocation());
			System.out.println(getMultivestaJarLocation());
			for (String s : paramSequence(defaultConfiguration(),true)) {
				System.out.println(s);
			}
			run();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}
}
