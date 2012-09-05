class Document 
	def generate_report(xml_data, report_design, output_type, select_criteria)
     		report_design << '.jasper' if !report_design.match(/\.jasper$/)
     		path=File.expand_path(File.dirname(__FILE__))
                parsed_path=path.split("/")
                parsed_path.delete_at(parsed_path.count - 1)
                interface_classpath=File.join(parsed_path)+"/java_src/jasper/bin" 
    		mode = "w+b"
    		Dir.foreach(File.join(parsed_path)+"/java_src/jasper/lib") do |file|
    			interface_classpath << ":#{File.join(parsed_path)}/java_src/jasper/lib/" + file if (file != '.' and file != '..' and file.match(/.jar/))
    		end
    		result=nil
    		IO.popen "java -Djava.awt.headless=true -cp \"#{interface_classpath}\" XmlJasperInterface -o#{output_type} -f#{File.join(parsed_path)}/reports/#{report_design} -x#{select_criteria}", mode do |pipe|
    			pipe.write xml_data
    			pipe.close_write
    			result = pipe.read
    			pipe.close
    		end
    	return result
    	end
end
