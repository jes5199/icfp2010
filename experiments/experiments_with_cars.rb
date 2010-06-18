require 'pp'

def car_experiment(line)
    chambers = line.split('22')
    puts ""
    chambers.each do |ch|
        p [ch[0..0], ch[1..-1]]
    end
end

File.open('example_cars.txt') do |f| 
    f.each do |line|
        line.chomp!

        car_experiment(line)
    end
end
