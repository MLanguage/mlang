package com.mlang;

import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.OptionalDouble;
import java.util.stream.IntStream;

public class TestHarness {

    private static final String SEPERATOR = "/";

    public static void main(String[] args){

        if (args.length != 1) {
            System.err.println("Expected one command-line argument, the tests directory");
            return;
        }

        String testDirString = args[0];

        Path testsDir = Paths.get(testDirString);

        System.out.println(testsDir);

        if(!Files.exists(testsDir)  || !Files.isDirectory(testsDir)){
            System.err.println("Tests directory does not exist or is not a directory");
            return;
        }

        try {
            Files.list(testsDir)
                .map(TestHarness::parseTest)
                .forEach(test -> {
                    Map<String, MValue> realOutputs = Ir_tests_2020
                                                    .calculateTax(test.getInputVariables());

                    test.getExceptedVariables().forEach((name, value) -> {
                            if(!realOutputs.get(name).equals(value)) {
                                System.err.println("Code " + name + ", expected: " +  value + ", got: " + realOutputs.get(name) );
                            }
                        } );
                    
                });
        } catch (IOException ex) {
            System.err.println(ex.getMessage());
            return;
        }
    }

    private static TestData parseTest(Path test){
        TestData td = new TestData();

        System.out.println("Test case : " + test);
        try {
            List<String> lines = Files.readAllLines(test);

            TestPosition tp = new TestPosition();


            IntStream.range(0, lines.size()).forEach(pos -> {
                String line = lines.get(pos);
                switch(line) {
                    case "#ENTREES-PRIMITIF": 
                    tp.setEntreesPrimitif(pos);
                    break;
                    case "#CONTROLES-PRIMITIF":
                    tp.setControlesPrimitif(pos);
                    break;
                    case "#RESULTATS-PRIMITIF":
                    tp.setResultatsPrimtifs(pos);
                    break;
                    case "#ENTREES-CORRECTIF":
                    tp.setEntreesCorrectif(pos);
                    break;
                    case "#CONTROLES-CORRECTIF":
                    tp.setControlesCorrectif(pos);
                    break;
                    case "#RESULTATS-CORRECTIF":
                    tp.setResultatsCorrectifs(pos);
                    break;
                }
            });

            lines.subList(tp.getEntreesPrimitif() + 1, tp.getControlesPrimitif())
                .stream()
                .forEach(variableLine -> {
                    addInputVariableToTestData(variableLine, td);
                });
            
              lines.subList(tp.getResultatsPrimtifs() + 1, tp.getEntreesCorrectif())
                .stream()
                .forEach(variableLine -> {
                    addExpectedVariableToTestData(variableLine, td);
                });

                
        } catch (IOException | NumberFormatException e) {
            e.printStackTrace();
        } 

        return td;
    }

    private static void addInputVariableToTestData(String variableLine, TestData td){
        Variable var = createVariable(variableLine);
        td.addInputVariable(var.getCode(), new MValue(var.getValue(), false));
    }

    private static void addExpectedVariableToTestData(String variableLine, TestData td){
        Variable var = createVariable(variableLine);
        td.addExpectedVariable(var.getCode(), new MValue(var.getValue(), false));
    }

    private static Variable createVariable(String variableLine){
        String[] variableLineArray = variableLine.split(SEPERATOR);
        String code = variableLineArray[0];
        double value = Double.parseDouble(variableLineArray[1]);
        return new Variable(code, value);
    }
}

class TestData {
    private final Map<String, MValue> inputVariables = new HashMap<>();
    private final Map<String, MValue> expectedVariables = new HashMap<>();

    public void addInputVariable(String code, MValue value){
        inputVariables.put(code, value);
    }

    public void addExpectedVariable(String code, MValue value){
        expectedVariables.put(code, value);
    }

    public Map<String, MValue> getInputVariables(){
        return inputVariables;
    }

    public Map<String, MValue> getExceptedVariables(){
        return expectedVariables;
    }
}

class Variable {
    private final String code;
    private final double value;

    public Variable(String code, double value){
        this.code = code;
        this.value = value;
    }

    public String getCode() {
        return code;
    }

    public double getValue() {
        return value;
    }
    
}

class TestPosition {
    private int entreesPrimitif;
    private int controlesPrimitif;
    private int resultatsPrimtifs;

    private int entreesCorrectif;
    private int controlesCorrectif;
    private int resultatsCorrectifs;

    public int getEntreesPrimitif() {
        return entreesPrimitif;
    }
    public void setEntreesPrimitif(int entreesPrimitif) {
        this.entreesPrimitif = entreesPrimitif;
    }
    public int getControlesPrimitif() {
        return controlesPrimitif;
    }
    public void setControlesPrimitif(int controlesPrimitif) {
        this.controlesPrimitif = controlesPrimitif;
    }
    public int getResultatsPrimtifs() {
        return resultatsPrimtifs;
    }
    public void setResultatsPrimtifs(int resultatsPrimtifs) {
        this.resultatsPrimtifs = resultatsPrimtifs;
    }
    public int getEntreesCorrectif() {
        return entreesCorrectif;
    }
    public void setEntreesCorrectif(int entreesCorrectif) {
        this.entreesCorrectif = entreesCorrectif;
    }
    public int getControlesCorrectif() {
        return controlesCorrectif;
    }
    public void setControlesCorrectif(int controlesCorrectif) {
        this.controlesCorrectif = controlesCorrectif;
    }
    public int getResultatsCorrectifs() {
        return resultatsCorrectifs;
    }
    public void setResultatsCorrectifs(int resultatsCorrectifs) {
        this.resultatsCorrectifs = resultatsCorrectifs;
    }


}
