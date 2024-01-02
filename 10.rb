# approx 300 lines for a low-budget "scheme" parser & interpreter:
require "strscan"
require "pry"

class Parser
  class ParseError < StandardError; end

  # FYI - this grammar accepts a ton of invalid scheme, but for our purposes...
  # program : atom | list
  # 
  # list:  ( list-elems )
  # 
  # list-elems: program list-elems
  #           | epsilon
  # 
  # atom: num
  #     | op              (example: +)
  #     | bool            (example: t or f)
  #     | ident           (example: + -> <procedure> ...)
  #     | quoted-ident    (example: 'primitive or 'non-primitive)
  attr_reader :scanner

  NUM          = /-?\d+/
  END_OF_INPUT = /^$/
  BOOL         = /#(t|f)/i
  IDENT        = /[a-z]+[a-z0-9]*(\?)?/
  OP           = /[+-]/
  QIDENT       = /'#{IDENT}/
  LPAREN       = /\(/
  RPAREN       = /\)/
  QUOTE        = /'/

  def initialize(str)
    @scanner = StringScanner.new(str)
  end

  def self.parse(str)
    new(str).parse
  end

  def parse
    program.tap do
      match!(END_OF_INPUT)
    end
  end

  def program
    if peek?(LPAREN)
      list
    else
      atom
    end
  end

  def list
    match!(LPAREN)

    list_elems([]).tap do
      match!(RPAREN)
    end
  end

  def list_elems(acc)
    if peek?(RPAREN)
      acc
    else
      list_elems(acc << program)
    end
  end

  def atom
    if peek?(NUM)
      num
    elsif peek?(BOOL)
      bool
    elsif peek?(IDENT)
      ident
    elsif peek?(QIDENT)
      qident
    elsif peek?(OP)
      op
    end
  end

  def num
    match!(NUM).to_i
  end

  def bool
    match!(BOOL).match?(/t/i)
  end

  def ident
    match!(IDENT).intern
  end

  def op
    match!(OP).intern
  end

  def qident
    [:quote, to_sym(match!(QIDENT))]
  end

  private

  def to_sym(tok)
    tok.gsub(QUOTE, "").intern # 'a => :a
  end

  def peek?(c)
    skip_whitespace

    scanner.match?(c)
  end

  def match(c)
    skip_whitespace

    scanner.scan(c)
  end

  def match!(c)
    match(c) || raise(ParseError)
  end

  def skip_whitespace
    scanner.skip(/\s+/)
  end
end

module Interpreter
  # an array of hashes; iterate through and find the one that has :key
  # ...and return the value
  def lookup_in_table(key, table)
    table.detect { |entry| entry[key] }.fetch(key)
  end

  # convenience for meaning. parses first. empty table for now
  def value(exp_str)
    exp = Parser.parse(exp_str)

    meaning(exp, [])
  end

  # lookup the action, call it w/ the exp & table
  def meaning(exp, table)
    action = exp_to_action(exp) # note: action is a method converted into a proc

    action[exp, table]
  end

  # dispatch:
  # expression as an atom (e.g 1, 'b, #'+)
  # expression as a list (e.g (car (quote a b)))
  def exp_to_action(exp)
    if atom?(exp)
      atom_to_action(exp)
    else
      list_to_action(exp)
    end
  end

  def list_to_action(exp)
    if atom?(first(exp))
      case first(exp)
      when :quote then method(:do_quote)
      when :lambda then method(:do_lambda)
      when :cond then method(:do_cond)
      else
        method(:do_application) # for built-ins (e.g +, car)
      end
    else
      method(:do_application) # for lambdas
    end
  end

  def do_application(exp, table)
    op, *args = exp

    apply(
      meaning(op, table),                    # meaning of the 'operator' - could be a lambda
      args.map { |arg| meaning(arg, table) } # meaning of the args
    )
  end

  def apply(fun, args)
    case fun
    in [:primitive, primf] then apply_primitive(primf, args)
    in [:non_primitive, *primf] then apply_closure(primf, args)
    end
  end

  def apply_primitive(fun, args)
    case fun
    when :car     then args[0][0]
    when :cdr     then args[0][1..-1]
    when :cons
      first, rest = args

      rest.unshift(first)
    when :null?   then args[0].empty?
    when :eq?     then args[0] == args[1]
    when :atom?   then atom?(args[0])
    when :zero?   then args[0].zero?
    when :number? then number?(args[0])
    when :+       then args.inject(&:+)
    when :-       then args.inject(&:-)
    end
  end

  def apply_closure(fun, args)
    env, params, body = fun
    entry             = Hash[params.zip(args)] # e.g turn formal params e.g (x y) and args e.g [1, 2] into {x: 1, y: 2}
    table             = env.dup.unshift(entry) # ...and then set it at the top of the "table"

    meaning(body, table)
  end

  UNSPECIFIED = Class.new do
    def inspect
      "#<unspecified>"
    end
  end.new

  # iterate through the question/answer pairs
  # if meaning(question) is true, return meaning(answer)
  def do_cond(exp, table)
    _, *pairs = exp

    pairs.each do |question, answer|
      if meaning(question, table)
        return meaning(answer, table) # note: this returns do_cond
      end
    end

    UNSPECIFIED
  end

  # TODO: gong illegal quotes - e.g (quote a b) instead of (quote (a b))
  def do_quote(exp, _table)
    exp.last
  end

  def do_lambda(exp, table)
    _, args, body = exp

    [:non_primitive, table, args, body] # note the table shoe-horned in
  end

  def atom_to_action(exp)
    case
    when number?(exp) then method(:do_const)
    when bool?(exp) then method(:do_const)
    when primf?(exp) then method(:do_const)
    else
      method(:do_identifier)
    end
  end

  def do_const(exp, _table)
    if number?(exp)
      exp
    elsif bool?(exp)
      exp
    else
      [:primitive, exp]
    end
  end

  def do_identifier(exp, table)
    lookup_in_table(exp, table)
  end

  def atom?(exp); !exp.kind_of?(Array); end

  BOOL = [true, false]
  def bool?(exp); BOOL.include?(exp); end

  PRIMF = %i(cons car cdr null? eq? atom? zero? number? + -).freeze
  def primf?(exp); PRIMF.include?(exp); end

  def number?(exp)
    [Integer].any? { |klass| exp.kind_of?(klass) }
  end

  def first(exp); exp[0]; end
  def text_of(exp); exp[1]; end
end
include Interpreter

require "minitest/autorun"

Class.new(Minitest::Test) do
  def test_quote
    assert_equal [1, 2, 3], value("(quote (1 2 3))")
  end

  def test_simple_primitives
    assert_equal 1, value("1")
    assert_equal -1, value("-1")
    assert_equal true, value("#t")
    assert_equal false, value("#f")
  end

  def test_do_lambda
    assert_equal [:non_primitive, [], [:x, :y], [:+, :x, :y]],
                 value("(lambda (x y) (+ x y))")
  end

  def test_do_cond
    res = value(<<~SCHEME)
    (cond ((eq? (car (quote (x y))) 'a) 7)
          ((eq? (car (quote (b y))) 'b) 9)
          (#t 11))
    SCHEME

    assert_equal 9, res

    res = value(<<~SCHEME)
    (cond ((eq? (car (quote (x y))) 'a) 7)
          ((eq? (car (quote (c y))) 'b) 11)
          (#t 11))
    SCHEME

    assert_equal 11, res
  end

  def test_primitive_functions
    PRIMF.each do |primf|
      assert_equal [:primitive, primf], value(primf.to_s)
    end
  end

  def test_application_primitive_functions
    assert_equal :a, value("(car (quote (a b)))")
    assert_equal %i(b), value("(cdr (quote (a b)))")
    assert_equal %i(a b c), value("(cons 'a (quote (b c)))")
    assert_equal true, value("(null? (quote ()))")
    assert_equal false, value("(null? (quote (a b c)))")
    assert_equal true, value("(eq? (quote (a b)) (quote (a b)))")
    assert_equal false, value("(eq? 1 2)")
    assert_equal true, value("(atom? 1)")
    assert_equal true, value("(atom? 'b)")
    assert_equal false, value("(atom? (quote ()))")
    assert_equal false, value("(atom? (quote (a b c)))")
    assert_equal true, value("(zero? 0)")
    assert_equal false, value("(zero? 1)")
    assert_equal true, value("(number? 1)")
    assert_equal false, value("(number? 'b)")
    assert_equal 6, value("(+ 1 2 3)")
    assert_equal 0, value("(- 3 2 1)")
  end

  def test_apply_closure
    res = value(<<~SCHEME)
    ((lambda (x) (+ 1 x)) 41)
    SCHEME

    assert_equal 42, res

    res = value(<<~SCHEME)
    ((lambda (x)
        (cons x (quote ())))
      (quote (foo bar baz)))
    SCHEME

    assert_equal [[:foo, :bar, :baz]], res

    res = value(<<~SCHEME)
    ((lambda (x)
        (cond
          (x (quote true))
          (else
            (quote false))))
      #t)
    SCHEME

    assert_equal :true, res
  end

  def test_identifier
    assert_equal 42, meaning("x", [{"x" => 42}])
  end

  def test_table_lookup
    table = [
      { entree: "spaghetti", dessert: "spumoni" },
      { appetizer: "food", entree: "tastes", beverage: "good" },
    ]

    value = lookup_in_table(:entree, table)

    assert_equal "spaghetti", value
  end

  def test_unspecified
    assert_equal UNSPECIFIED, value("(cond ((eq? 1 2) 42))")
  end
end
