##
## $Header$
##

HC = ghc
HFLAGS = -Wall

sources = \
	Language/LazyS/PrimNum.hs \
	Language/LazyS/ParseResult.hs \
	Language/LazyS/SExpSyntax.hs \
	Language/LazyS/SExpParser.hs \
	Language/LazyS/Syntax.hs \
	Language/LazyS/Parser.hs \
	Language/LazyS/Evaluator.hs \


tests = \
	SExpTest.hs \
	EvalTest.hs \
	expr.hs \
	single.hs

all_sources = $(sources) $(tests)

targets = $(all_sources:.hs=.o)

interfaces = $(all_sources:.hs=.hi)

%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

all: $(targets)
	hlint --utf8 --color $(sources)

rebuild: clean all

push: rebuild
	hg commit
	hg kwshrink
	hg kwexpand
	hg push

clean:
	$(RM) $(targets) $(interfaces)
