def first(col); col[0]; end
def rest(col); col[1..-1]; end
def atom?(e); !e.kind_of?(Array); end
def array?(e); e.kind_of?(Array); end

=begin
def fold(col, acc, &block)
  if col.empty?
    acc
  else
    fold(rest(col), yield(acc, (first col)), &block)
  end
end

puts fold([1, 2, 3], 0) { |acc, e| acc = acc + e }

def double_fold(a, b, acc, &block)
  if [a, b].all?(&:empty?)
    acc
  else
    double_fold(rest(a),
                rest(b),
                yield(acc, (first a), (first b)),
                &block)
  end
end

res =
  double_fold([1, 2, 3], [5, 6, 7], 0) do |acc, e1, e2|
    acc = acc + e1 + e2
  end

puts(res)
=end

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

# TODO: try double_fold_star(acc, *args, &block)
# would need #firsts & #rests, probably, no?

# breakthrough:
# the acc for a star fun is:
# normal:
# x(rest(a),
#   yield(acc, first(a)),
#   &block)
#
# star:
# x(rest(a),
#   x(first(a),
#     yield(acc, first(first(a))),
#     &block),
#   &block)
#
# the trick: for the *-condition, make the result of the recursion of first
# be the acc of the recursion for the rest. slick!

# idea: once this is all done - you'll want this to be the 'acc' below (i.e yield(acc ...)
# so maybe inline this there?
=begin
double_fold(rest(first(a)),
            rest(first(b)),
            yield(acc, (first (first a)), (first (first b))),
            &block)

double_fold(rest(a),
            rest(b),
            yield(acc, (first (first a)), (first (first b))),
            &block)

double_fold(rest(a),
            rest(b),
            double_fold(rest(first(a)),
                        rest(first(b)),
                        yield(acc, (first (first a)), (first (first b))),
                        &block)
            &block)
=end
