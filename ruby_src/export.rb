$: << File.dirname(__FILE__) unless $:.include? File.dirname(__FILE__)
require 'document.rb'
require 'rubygems'
require 'erlectricity'
      
class Export
	def make_report(xpath, jasper_file, name, type)
		receive do |f|
			f.when([:xml, String]) do |xml|
				send_doc("#{xml}", xpath, jasper_file, name, type)
				f.send!([:response, "ok"])
                                f.receive_loop
			end
		end
	end
 
	def send_doc(xml, xml_start_path, report, filename, output_type)
		case output_type
			when 'rtf'
				extension = 'rtf'
				mime_type = 'application/rtf'
				jasper_type = 'rtf'
			when 'pdf'
				extension = 'pdf'
				mime_type = 'application/pdf'
				jasper_type = 'pdf'
			else # xls
				extension = 'xls'
				mime_type = 'application/vnd.ms-excel'
				jasper_type = 'xls'
		end

		doc = Document.new
		file = doc.generate_report(xml, report, jasper_type, xml_start_path)
		path=File.expand_path(File.dirname(__FILE__))
                parsed_path=path.split("/")
                parsed_path.delete_at(parsed_path.count - 1)
                File.open(File.join(parsed_path) + "/reports/" + filename + "." + output_type, 'w') {|f| f.write(file) }  
	end
 
	def get_file_as_string(filename)
		data = ''
		f = File.open(filename, "r") 
		f.each_line do |line|
			data += line
		end
		return data
	end
end
 
#Get the variables from erlang to configure the report
xpath, jasper_file, name, type = *ARGV
report = Export.new
report.make_report(xpath, jasper_file, name, type)
