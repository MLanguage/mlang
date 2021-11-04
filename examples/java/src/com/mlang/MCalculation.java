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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MCalculation {

  private final MValue[] calculationVariables;
  private final MValue[] localVariables;
  private final Map<String, List<MValue>> tableVariables = new HashMap<>();
  private final int maxAnomalies;
  private int currentAnomalies = 0;

  public MCalculation(MValue[] calculationVariables, MValue[] localVariables, int maxAnomalies) {
    this.calculationVariables = calculationVariables;
    this.maxAnomalies = maxAnomalies;
    this.localVariables = localVariables;
  }

  public MValue[] getCalculationVariables() {
    return calculationVariables;
  }

  public MValue[] getLocalVariables() {
    return localVariables;
  }

  public Map<String, List<MValue>> getTableVariables() {
    return tableVariables;
  }

  public int getMaxAnomalies() {
    return maxAnomalies;
  }

  public int getCurrentAnomalies() {
    return currentAnomalies;
  }

  public void setCurrentAnomalies(int currentAnomalies) {
    this.currentAnomalies = currentAnomalies;
  }

}
