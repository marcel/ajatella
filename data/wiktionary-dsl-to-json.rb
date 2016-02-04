#!/usr/bin/env ruby

require 'pp'
require 'set'
require 'json'

CASES = %w[nominative genitive partitive accusative inessive elative illative adessive ablative allative essive translative instructive abessive comitative]
CASES_MATCHER = %r{\[i\](#{CASES.join('|')})\[/i\] (\S+) (\S+)} 

PART_OF_SPEECH = %r{\[b\]\[i\]([^\[]+)\[/i\]\[/b\]}

MOODS = %w[indicative conditional imperative potential]
MOODS_MATCHER = %r{ \[b\](#{MOODS.join('|')})\[/b\]}

NEGATIVES = %w[eivät ette emme et en ei älkööt älköön älkäämme älkää älä]
NEGATIVES_MATCHER = NEGATIVES.join('|')
CONJUGATION_MATCHER = %r{\[i\](?:(\S+) (\S+)\.|passive)\[/i\] (.+)\s((?:#{NEGATIVES_MATCHER}).+)}

class Entry
  attr_accessor :word, :current_section, :declensions, :conjugations, :parts_of_speech
  attr_accessor :current_part_of_speech, :hyphenation, :pronunciation, :saw_rare_section, :mangled_declention_or_conjugation
  attr_accessor :conjugation_type, :declension_type, :derived_terms, :related_terms, :see_also
  attr_accessor :definitions, :current_mood, :current_tenses, :etymologies, :subsection_seen
  
  def initialize(word)
    @word = word
    @definitions   = []
    @conjugations  = []
    @declensions   = []
    @derived_terms = {}
    @related_terms = []
    @see_also      = []
    @etymologies   = []
    @parts_of_speech = Set.new
    @saw_rare_section = false
    @subsection_seen = false
    @mangled_declention_or_conjugation = false
  end
  
  def to_json(*a)
    {
      word: word, definitions: definitions, 
      conjugations: conjugations, declensions: declensions, 
      conjugation_type: conjugation_type, declension_type: declension_type,
      pronunciation: pronunciation, hyphenation: hyphenation,
      related_terms: related_terms, derived_terms: derived_terms, see_also: see_also,
      etymology: etymologies
    }.to_json(*a)
  end
  
  def in_definitions_section?
    (!mangled_declention_or_conjugation? && conjugation_type.nil? && declension_type.nil?) && !parts_of_speech.empty?
  end
  
  def in_conjugations_section?
    !conjugation_type.nil? && !current_mood.nil?
  end
  
  def in_etymology_section?
    current_section.to_s['Etymology'] && !subsection_seen?
  end
  
  def subsection_seen?
    @subsection_seen
  end
  
  def saw_rare_section?
    @saw_rare_section
  end
  
  def mangled_declention_or_conjugation?
    @mangled_declention_or_conjugation
  end
end

class Conjugation
  attr_reader :mood, :number, :person, :positive, :negative, :tense
  def initialize(mood, tense, person, number, positive, negative)
    @mood = mood
    @tense = tense
    @person = person
    @number = number
    @positive = positive
    @negative = negative
  end
  
  def to_json(*a)
    {mood: mood, tense: tense, person: person, number: number, positive: positive, negative: negative}.to_json(*a)
  end
end

class InflectedForm
  attr_reader :case_name, :singular, :plural
  def initialize(case_name, singular, plural)
    @case_name = case_name
    @singular  = singular
    @plural    = plural
  end
  
  def to_json(*a)
    {case_name: case_name, singular: singular, plural: plural}.to_json(*a)
  end
end

class Definition
  attr_reader :part_of_speech, :number, :text, :example_usage
  def initialize(part_of_speech, number, text)
    @part_of_speech = part_of_speech
    @number = number
    @text   = text
    @example_usage = []
  end
  
  def to_json(*a)
    {part_of_speech: part_of_speech, number: number, text: text, examples: example_usage}.to_json(*a)
  end
end

class ExampleUsage
  attr_accessor :finnish, :english
  
  def initialize(finnish = nil, english = nil)
    finnish.strip! if finnish
    english.strip! if english
    @finnish = finnish
    @english = english
  end
  
  def to_json(*a)
    {finnish: finnish, english: english}.to_json(*a)
  end
  
  def blank?
    finnish.nil? && english.nil?
  end
  
  def incomplete?
    !finnish.nil? && english.nil?
  end
  
  def complete?
    !finnish.nil? && !english.nil?
  end
end

if $0 == __FILE__
  ignored_sections_for_inflections = Set.new(["Nominal forms", "I", "II", "III", "IV"])
  entry = nil
  
  puts "["
  
  ARGF.each do |line|
    if entry
      entry.subsection_seen = line['[i]']
    end
    
    case line
    when /^(\S.*)/
      if entry
        puts JSON.generate(entry) + ","
      end
      entry = Entry.new($1)
    when %r{^  (\d+)\. (.*)$}
      if entry.in_definitions_section?
        # entry.definitions[entry.current_part_of_speech] ||= []
        entry.definitions << Definition.new(entry.current_part_of_speech, Integer($1), $2)
      end
    when %r{\[b\]([A-Z][^\[]*)\[/b\]}
      entry.current_section = $1
      entry.subsection_seen = false
    when PART_OF_SPEECH
      entry.current_part_of_speech = $1
      entry.parts_of_speech << $1
    when %r{\* Hyphenation: (\S+)}
      entry.hyphenation ||= $1
    when %r{\* IPA\^\(key\): (?:\(|/)(\S+)(?:\)|/)(?:\s|,)}
      entry.pronunciation ||= $1
    when %r{\[\*\] \[b\] Conjugation of .+ \(type ([^\)]+)\)\s\[/b\]}
      entry.conjugation_type ||= $1
    when %r{\[\*\] \[b\] Declension of .+ \(type ([^\)]+)\)\s\[/b\]}
      entry.declension_type ||= $1
    when %r{\[\*\] \[b\]}
      entry.mangled_declention_or_conjugation = true
    when %r{(present|past)\W+(pluperfect|perfect)}
      entry.current_tenses = [$1, $2]
    when %r{^   \* (\S.+)$}
      case entry.current_section 
      when "Derived terms"
        case $1
        when %r{(\S+): (.+)$}
        entry.derived_terms[$1.to_sym] = $2.split(", ")
        else
        end
      when "Related terms"
        entry.related_terms << $1
      when "See also"
        entry.see_also << $1
      else
      end
    when %r{\^\d+\) Rare\.}
      entry.saw_rare_section = true
    when CASES_MATCHER
      if !ignored_sections_for_inflections.include?(entry.current_section) && !entry.saw_rare_section?
        entry.declensions << InflectedForm.new($1, $2, $3)
      end
    when MOODS_MATCHER
      entry.current_mood = $1
    when %r{^ #{CONJUGATION_MATCHER} #{CONJUGATION_MATCHER}$}
      if entry.in_conjugations_section?
        person = $1 || 'passive'
        number = $2
        entry.conjugations << Conjugation.new(entry.current_mood, entry.current_tenses[0], person, number, $3, $4)
        person = $5 || 'passive'
        number = $6
        entry.conjugations << Conjugation.new(entry.current_mood, entry.current_tenses[1], person, number, $7, $8)
      end
    when /^\s+\S.+$/ # i.e. a non blank line     
      if entry.in_definitions_section? && !entry.definitions.empty?
        
        current_definition = entry.definitions.last
        existing_examples  = current_definition.example_usage
      
        case line
        when /^     \S/ # Definition continuation
          entry.definitions.last.text << " #{line.strip}"
        when /^       (?:\+|o)? ([^=]+) (?:=|-) (.+)/ # Finnish = English
          existing_examples << ExampleUsage.new($1, $2)
        when /^         [A-Z]/#, /^           o [^=]+$/ # Finnish example line
          # if current_definition
            if existing_examples.empty? || existing_examples.last.complete?
              example = ExampleUsage.new
              example.finnish = line.strip
              current_definition.example_usage << example
            end
          # end
        when /^             \S/, /^               = / # English translation
          if !existing_examples.empty? && existing_examples.last.incomplete?
            existing_examples.last.english = line.strip
          end
        when /^                 \S/ # English translation continuation...
          if !existing_examples.empty? && existing_examples.last.complete?
            existing_examples.last.english << " #{line.strip}"
          end
        else
          #p  line
        end
      elsif entry.in_etymology_section?
        stripped_line = line.strip
        
        if entry.etymologies.empty?
          entry.etymologies << stripped_line
        else
          if number = entry.current_section[/\d/]
            
            number = number.to_i
            current_index = number - 1
            
            if entry.etymologies.size == number
              entry.etymologies.last << " #{stripped_line}"
            else
              entry.etymologies << stripped_line
            end
          else
            entry.etymologies.last << " #{stripped_line}"
          end
        end
      end
    else
      # Blank lin
    end    
  end
  
  puts "]"
end

# Sections by count
# 28620 Etymology
# 10647 Pronunciation
# 10368 Synonyms
# 8209 Anagrams
# 7145 Nominal forms
# 7145 IV
# 7145 III
# 7145 II
# 5785 Derived terms
# 4242 Related terms
# 2282 See also
# 1533 Usage notes
# 1311 Compounds
#  958 Antonyms
#  770 Alternative forms
#  669 Etymology 2
#  668 Etymology 1
#  416 References
#   66 Etymology 3