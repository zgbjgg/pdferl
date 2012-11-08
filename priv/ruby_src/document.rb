## ==============================================================================
#
# CLASS DOCUMENT
#
# Copyright (c) 2012 Jorge Garrido <jorge.garrido@morelosoft.com>.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of copyright holders nor the names of its
#    contributors may be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
## ===============================================================================

# This class creates a file and write in a pipe to java from
# ruby. The method generate_report takes as input a xml_data in binary mode,
# the report_design is the template (.jasper file), output_type is the extension
# file to be created, select_criteria represents the xpath in xml input.  
class Document 
	def generate_report(xml_data, report_design, output_type, select_criteria)
     		report_design << '.jasper' if !report_design.match(/\.jasper$/)
		
		# parse path to gain access from any path
     		path=File.expand_path(File.dirname(__FILE__))
                parsed_path=path.split("/")
                parsed_path.delete_at(parsed_path.count - 1)

		# classpath to interface with java 
                interface_classpath=File.join(parsed_path)+"/java_src/jasper/bin" 
    		
   		# mode write + binary
                mode = "w+b"

		# get all dependencies libraries and add to interface classpath 
    		Dir.foreach(File.join(parsed_path)+"/java_src/jasper/lib") do |file|
    			interface_classpath << ":#{File.join(parsed_path)}/java_src/jasper/lib/" + file if (file != '.' and file != '..' and file.match(/.jar/))
    		end

		# store result	
    		result=nil

		# pipe from ruby to java, read bytes 
    		IO.popen "java -Djava.awt.headless=true -cp \"#{interface_classpath}\" XmlJasperInterface -o#{output_type} -f#{File.join(parsed_path)}/reports/#{report_design} -x#{select_criteria}", mode do |pipe|
    			pipe.write xml_data
    			pipe.close_write
    			result = pipe.read
    			pipe.close
    		end
    	return result
    	end
end
