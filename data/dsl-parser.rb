#!/usr/bin/env ruby
require 'json'
lines = ARGF.readlines

current_word = nil
before_definition = false
in_definition = false
after_definition = false
definition_lines = []
linenum = 0
part_of_speech = nil

puts "["
lines.each do |line|
  linenum += 1
  if line[/^([^# ].+)$/]
    part_of_speech = nil
    entry = {}
    linenum = 1
    after_definition = false
    before_definition = true
    current_word = $1.strip
    # puts current_word
    definition_lines = []
  else    
    if before_definition && line[/^\s+1\. /]
      definition_lines << line
      in_definition = true
      before_definition = false
    elsif line[/\[b\]\[i\]([^\[]+)\[\/i\]\[\/b\]/]
      part_of_speech = $1
    elsif in_definition
      if line[/^\s*$/]
        individual_definitions = []
        definition_lines.each do |def_line|
          stripped = def_line.strip
          
          if stripped[/^(\d+\. )(.*)$/]
            individual_definitions << $2
          else
            individual_definitions.last << " " + stripped
          end
        end
        
        defs = individual_definitions.each_with_index.inject([]) do |definitions, (definition, number)|
          definitions << {"number" => number + 1, "text" => definition}
          definitions
        end
        
        if defs.any?          
          entry_dict = {}
          entry_dict["term"] = current_word
          entry_dict["pos"] = part_of_speech if part_of_speech
          entry_dict["senses"] = defs
          puts entry_dict.to_json + ","
        end
        
        in_definition = false
        after_definition = true
      else
        definition_lines << line
      end
    end
  end
end
puts "]"