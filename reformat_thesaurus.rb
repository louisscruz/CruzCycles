thesaurus = Hash.new { |h, k| h[k] = { synonyms: [], antonyms: [] }}

raw_lines = File.readlines('thesaurus.txt')

raw_lines.each do |line|
  split_line = line.strip.split(',')
  word = split_line[0]
  others = split_line[3..-1]
  others.each do |term|
    antonym_slice = term[-9..-1]
    generic_slice = term[-14..-1]
    if antonym_slice == '(antonym)'
      thesaurus[word][:antonyms].push(term.split(' (')[0])
    elsif generic_slice != '(generic term)'
      thesaurus[word][:synonyms].push(term.split(' (')[0])
    end
  end
end

thesaurus.reject! { |k, v| v[:synonyms].empty? }.length

thesaurus_lines = thesaurus.map do |k, v|
  values = [k]
  synonyms = v[:synonyms].join(',')
  antonyms = v[:antonyms].join(',')
  values.push(synonyms)# unless synonyms.empty?
  values.push(antonyms)# unless antonyms.empty?
  values.join(';')
end

new_thesaurus = thesaurus_lines.join("\n")

File.open('thesaurus_new.txt', 'w') { |file| file.write(new_thesaurus) }
