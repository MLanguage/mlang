ifeq ($(MAKELEVEL), 0)
    include ../../Makefile.include
endif

MLANG_JAVA_OPTS=\
	--mpp_file=$(MPP_FILE) \
	--mpp_function=$(MPP_FUNCTION)

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_JAVA_OPTS)

# Check Java compiler version : 
#   - if < 8 : backend won't compile due to TestHarness, period.
#   - if = 8 : calculator code prefers JRE 7 base class rt.jar to be compiled targetting Java 7, so we try to find it.
#   - if > 8 : calculator code is cross-compiled to Java 7, test code is cross-compiled to Java 8.
JAVAC8=$(shell javac -version 2>&1|grep -q "javac 1.8"; printf $$?)
ifeq ($(JAVAC8),0)
# Default path is debian-based OpenJDK 7 package path, overridable by environment variables.
JAVA7_PATH?=/usr/lib/jvm/java-7-openjdk-amd64
# if path exists, use it as bootclasspath
ifeq (test -d $(JAVA7_PATH),0)
BOOTCLASSPATH=-bootclasspath $(JAVA7_PATH)/jre/lib/rt.jar
# Otherwise we default to empty bootclasspath, meaning javac will use its own version of base class (JRE 8).
endif
# JDK 8 specific cross-compilation option: -source and optional -bootclasspath. -target defaults to the value of -source
JAVACOPT_CALC=-source 7 $(BOOTCLASSPATH)
JAVACOPT_TEST=
else
# JDK9+ cross-compilation options. From version 9, reference base classes for older JRE are provided.
JAVACOPT_CALC=--release 7
JAVACOPT_TEST=--release 8
endif

all: backend_tests $(shell find . -name "run_*.py")

clean: 
	rm -f src/com/mlang/Ir_*.java target/com/mlang/*.class backend_tests/target/com/mlang/*.class

##################################################
# Generating and running Java files from Mlang
##################################################

.PRECIOUS: src/com/mlang/Ir_%.java
src/com/mlang/Ir_%.java: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend java --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

target:
	mkdir -p $@

backend_tests/target:
	mkdir -p $@

target/com/mlang/Ir_%.class:  src/com/mlang/Ir_%.java | target
ifeq ($(JAVAC8),0)
	@echo "Cross-compiling Java 7 sources with JDK 8, using JAVA7_PATH=$(JAVA7_PATH) if present. Feel free to override it."
endif
	javac -J-Xss10m -J-Xmx4096m $(JAVACOPT_CALC) -d target -cp src src/com/mlang/*.java

backend_tests/target/TestHarness.class: target/com/mlang/Ir_tests_2020.class | backend_tests/target
	javac $(JAVACOPT_TEST) -cp "backend_tests/src/:target" -d backend_tests/target backend_tests/src/com/mlang/TestHarness.java

run_tests: backend_tests/target/TestHarness.class
	java -cp "target:backend_tests/target" com.mlang.TestHarness $(TESTS_DIR)
