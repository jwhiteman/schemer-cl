def first(col); col[0]; end
def rest(col); col[1..-1]; end
def atom?(e); !e.kind_of?(Array); end
def array?(e); e.kind_of?(Array); end

def double_fold_star(a, b, acc, &block)
  if [a, b].all?(&:empty?)
    acc
  elsif [first(a), first(b)].all? { |x| atom?(x) }
    double_fold_star(rest(a),
                     rest(b),
                     yield(acc, (first a), (first b)),
                     &block)
  elsif [first(a), first(b)].all? { |x| array?(x) }
    double_fold_star(rest(a),
                     rest(b),
                     double_fold_star(rest(first(a)),
                                      rest(first(b)),
                                      yield(acc, (first (first a)), (first (first b))),
                                      &block),
                     &block)
  else
    raise "error"
  end
end

a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
b = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

res =
  double_fold_star(a, b, 0) do |acc, e1, e2|
    acc = acc + e1 + e2
  end

puts res
