/* Copyright (C) 2021 Inria, contributor: James Barnes <bureau.si-part-ircalcul@dgfip.finances.gouv.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. */

package com.mlang;

import java.util.List;
import java.util.Map;

public class MOutput {

  private final Map<String, MValue> outputValues;
  private final List<MError> calculationErrors;

  MOutput(Map<String, MValue> outputValues, List<MError> calculationErrors) {
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
