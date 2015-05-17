package controllers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import play.*;
import play.mvc.*;
import views.html.*;
import play.data.DynamicForm;
import play.data.Form;
import views.html.index;
import views.html.genewords;

public class Application extends Controller {

    public static Result index() {
        return ok(index.render("test me!"));
    }

    public static Result genewords(String q) {
		Runtime rt = Runtime.getRuntime();
    	try {
    		//String[] commands = {"system.exe","-get t"};
    		String base_dir = System.getProperty("user.dir");
    		Logger.info("working directory="+base_dir);
    		String cmd = "Rscript "+base_dir+"/public/r/genewords.r " + base_dir + " "+ q;
    		Logger.info(cmd);
			Process proc = rt.exec(cmd);
			
			BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
			 
			// read the output from the command
			System.out.println("Here is the standard output of the command:\n");
			String s = null;
			StringBuilder sb = new StringBuilder();
			while ((s = stdInput.readLine()) != null) 
			    sb.append(s);
			
			//return ok(sb.toString()).as("text/html");
			return ok(genewords.render(sb.toString()));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    	
    	 
    	    			
    	return ok("<h1>oops cant process "+q+"!</h1>").as("text/html");
    	
    	//return ok("<h1>Hello "+name+"!</h1>").as("text/html");
    	//return ok("GET " + name);
  	}
  	public static Result genewords_post() {
  		DynamicForm form = Form.form().bindFromRequest();
		
		if (form.data().size() == 0) {
	        return badRequest("Expceting some data");
	    } else {
	        String response = "Welcome " + form.get("q") + " !";
	        return ok(response);
    	}
  	}
}
