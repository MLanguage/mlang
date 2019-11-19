import java.util.Map;
import java.util.HashMap;

class CasBasique2018 {
    public static void main(String[] args) throws MError {
        Map<String, MValue> inputs = new HashMap<String, MValue>();
        inputs.put("0CF", new MValue(1.0));
        inputs.put("1AJ", new MValue(10023.0));
        inputs.put("1BJ", new MValue(42598.0));
        IR ir = new IR(inputs);
        ir.compute();
        double ir_net = ir.getIRNET().get_value();
        System.out.println(ir_net);
    }
}
