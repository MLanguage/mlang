import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
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
            Files.list(testsDir).forEach(TestHarness::parseTest);
        } catch (IOException ex) {
            System.err.println(ex.getMessage());
            return;
        }
    }

    private static void parseTest(Path test){
        System.out.println("Test case : " + test);
        try {
            List<String> lines = Files.readAllLines(test);

            TestPosition tp = new TestPosition();

            TestData td = new TestData();

            List<String> variables = new ArrayList<>();

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
                    String[] variableLineArray = variableLine.split(SEPERATOR);
                    String code = variableLineArray[0];
                    OptionalDouble value = OptionalDouble.of(Double.parseDouble(variableLineArray[1]));
                    td.addVariable(code, value);
                });

        } catch (IOException e) {
            e.printStackTrace();
        } catch (NumberFormatException e ){
            e.printStackTrace();
        }
    }
}

class TestData {
    private Map<String, OptionalDouble> calculationVariables = new HashMap<>();

    public void addVariable(String code, OptionalDouble value){
        calculationVariables.put(code, value);
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