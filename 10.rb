# 10
require "strscan"
require "pry"

class Parser
  class ParseError < StandardError; end

  # IDEA:
  # program : atom | list
  # 
  # list:  ( list-elems )
  # 
  # list-elems: program list-elems
  #           | epsilon
  # 
  # atom: num
  #     | bool            (example: t or f)
  #     | ident           (example: + -> <procedure> ...)
  #     | quoted-ident    (example: 'primitive or 'non-primitive)
  attr_reader :scanner

  NUM          = /\d+/
  END_OF_INPUT = /^$/
  BOOL         = /#(t|f)/i
  IDENT        = /[a-z\-\+\*]+(\?)?/
  QIDENT       = /'#{IDENT}/
  LPAREN       = /\(/
  RPAREN       = /\)/

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

  def qident
    [:quote, match!(QIDENT).intern]
  end

  private

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

require "minitest/autorun"

class ParserTest < Minitest::Test
  def test_num
    assert_equal 1, Parser.parse("1")
  end

  def test_bool
    assert_equal true, Parser.parse("#t")
    assert_equal false, Parser.parse("#f")

    assert_raises(Parser::ParseError) do
      Parser.parse("#x")
    end
  end

  def test_ident
    assert_equal :"some-ident", Parser.parse("some-ident")
    assert_equal :x, Parser.parse("x")
  end

  def test_quoted_ident
    assert_equal [:quote, :"'some-quoted-ident"], Parser.parse("'some-quoted-ident")
  end

  def test_simple_list
    # allowing these to parse, for now:
    assert_equal [], Parser.parse("()")
    assert_equal [1], Parser.parse("(1)")
    assert_equal [1, true, :"some-ident", false], Parser.parse("(1 #t some-ident #f)")
    assert_equal [1, [:quote, :"'x"]], Parser.parse("(1 'x)")
  end

  def test_nested_lists
    assert_equal [:lambda, [:x], [:+, :x, 1]], Parser.parse("(lambda (x) (+ x 1))")
    assert_equal [[:lambda, [:x], [:+, :x, 1]], 41], Parser.parse("((lambda (x) (+ x 1)) 41)")
    assert_equal [:+, [:+, 1, 2], 3], Parser.parse("(+ (+ 1 2) 3)")
  end
end
