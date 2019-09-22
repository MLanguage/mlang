
class CasBasique2018 {
    public static void main(String[] args) {
        IR ir = new IR(
            1.0,
            10023.0,
            42598.0
        );
        ir.compute();
        double ir_net = ir.getIRNET().get_value();
        System.out.println(ir_net);
    }
}
