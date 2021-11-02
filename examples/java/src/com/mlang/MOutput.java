package com.mlang;

import java.util.List;
import java.util.Map;

public class MOutput {

  private final Map<String, MValue> outputValues;
  private final List<MError> calculationErrors;

  public MOutput(Map<String, MValue> outputValues, List<MError> calculationErrors) {
    this.outputValues = outputValues;
    this.calculationErrors = calculationErrors;
  }

  public Map<String, MValue> getOutputValues() {
    return outputValues;
  }

  public List<MError> getCalculationErrors() {
    return calculationErrors;
  }

}
