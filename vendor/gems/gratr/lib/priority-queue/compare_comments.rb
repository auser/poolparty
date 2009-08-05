c_file = File.read("ext/priority_queue/CPriorityQueue/priority_queue.c")
rb_file = File.read("lib/priority_queue/ruby_priority_queue.rb")

c_comments = Hash.new { "" }

c_file.scan(%r(/\*(.*?)\*/\s*static\s+\w+\s*pq_(\w+)\(.*?\))m).each do | match |
  c_comments[match[1]] = match[0].gsub(%r(\n\s*\* {0,1})m, "\n").strip
end

rb_comments = Hash.new { "" }

rb_file.scan(%r(((?:\n\s*#[^\n]*)*)\s*def\s+(\w+))m).each do | match |
  rb_comments[match[1]] = match[0].gsub(%r(\n\s*# {0,1})m, "\n").strip
end

add_comments = Hash.new

(rb_comments.keys + c_comments.keys).uniq.each do | key |
  #next if rb_comments[key].gsub(/\s+/m, " ") == c_comments[key].gsub(/\s+/m, " ")
  if c_comments[key].empty?
    add_comments[key] = rb_comments[key]
  elsif rb_comments[key].empty?
    add_comments[key] = c_comments[key]
  elsif rb_comments[key] != c_comments[key]

    puts key
    puts "Ruby"
    puts rb_comments[key]
    puts "C"
    puts c_comments[key]
    puts
    puts "Choose [c,r]"
    1 until /^([cr])/ =~ gets
    add_comments[key] = ($1 == "c" ? c_comments : rb_comments)[key]
    puts "-" * 80
    puts
  else
    add_comments[key] = rb_comments[key]
  end

end

File.open("lib/priority_queue/ruby_priority_queue.new.rb", "wb") do | o |
  o << 
    rb_file.gsub(%r(((?:\n\s*#[^\n]*)*)(\s*def\s+(\w+)))m) do | match |
      name, all =  $3, $2
      "\n" + (add_comments[name].gsub(/^/, "#")) + all
    end
end
