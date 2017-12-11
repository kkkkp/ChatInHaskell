TARGETS = stresstest

all: $(TARGETS)

%.o: %.cc
	g++ $^ -c -o $@

testserver: testserver.o
	g++ $^ -o $@

stresstest: stresstest.o
	g++ $^ -o $@

proxy: proxy.o
	g++ $^ -o $@

clean::
	rm -fv $(TARGETS) *~ *.o
