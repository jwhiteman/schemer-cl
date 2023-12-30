lat = (1..10)

$r1 =
  lat.reduce(->(x) { x }) do |acc, e|
    ->(x) do
      acc[x.unshift(e)]
    end
  end


acc =
  ->{ puts "done" }.tap do |f|
    f.define_singleton_method(:inspect) do
      %q{-> { puts "done" }}
    end
  end

$r2 =
  lat.reduce(acc) do |acc, e|
    ->{ puts e; acc.call }.tap do |f|
      f.define_singleton_method(:inspect) do
        %Q{->{ puts "#{e}"; #{acc.inspect}.call }}
      end
    end
  end
