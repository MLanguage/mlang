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

/**
 * MValue is the representation of a variable used during a tax calculation,
 * either as input, output or an intermediate value.
 */
public class MValue {

  static final MValue mUndefined = new MValue(0., true);
  static final MValue zero = new MValue(0., false);
  static final MValue one = new MValue(1., false);

  private final double value;
  private final boolean undefined;

  MValue(double value, boolean isDefined) {
    this.value = value;
    this.undefined = isDefined;
  }

  /**
   * Create an MValue with a double value, by default MValues are created as
   * defined
   * 
   * @param value the value to be used with this variable
   */
  public MValue(double value) {
    this.value = value;
    this.undefined = false;
  }

  /**
   * Getter for value Field
   * 
   * @return the double value of the MValue
   */
  public double getValue() {
    return this.value;
  }

  /**
   * Getter for undefined field
   * 
   * @return boolean, whether the value has been defined during calculation
   */
  public boolean isUndefined() {
    return this.undefined;
  }

  private static MValue boolToMValue(boolean b) {
    return b ? one : zero;
  }

  static MValue mGreaterThan(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() > y.getValue());
  }

  static MValue mGreaterThanEqual(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() >= y.getValue());
  }

  static MValue mLessThan(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() < y.getValue());
  }

  static MValue mLessThanEqual(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() <= y.getValue());
  }

  static MValue mEqual(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() == y.getValue());
  }

  static MValue mNotEqual(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() != y.getValue());
  }

  static MValue mAnd(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() != 0 && y.getValue() != 0);
  }

  static MValue mOr(MValue x, MValue y) {
    if (x.isUndefined() && y.isUndefined()) {
      return mUndefined;
    }
    return boolToMValue(x.getValue() != 0 || y.getValue() != 0);
  }

  static MValue mAdd(MValue x, MValue y) {

    if (x.isUndefined() && y.isUndefined()) {
      return mUndefined;
    }

    return new MValue(x.getValue() + y.getValue());
  }

  static MValue mSubtract(MValue x, MValue y) {
    if (x.isUndefined() && y.isUndefined()) {
      return mUndefined;
    }

    return new MValue(x.getValue() - y.getValue());
  }

  static MValue mMultiply(MValue x, MValue y) {
    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return new MValue(x.getValue() * y.getValue());
  }

  static MValue mDivide(MValue x, MValue y) {

    if (x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }

    double denominateur = y.getValue();

    if (denominateur == 0) {
      return zero;
    }

    return new MValue(x.getValue() / denominateur);
  }

  static MValue m_round(MValue x) {
    if (x.isUndefined()) {
      return mUndefined;
    }
    double dValue = x.getValue();
    double valueToRound = dValue + (dValue < 0 ? -0.50005 : 0.50005);
    return new MValue((double) (int) (valueToRound));
  }

  static MValue m_floor(MValue x) {
    if (x.isUndefined()) {
      return mUndefined;
    }
    double valueToFloor = x.getValue() + 0.000001;
    return new MValue(Math.floor(valueToFloor));
  }

  static MValue m_cond(MValue cond, MValue trueVal, MValue falseVal) {
    if (cond.isUndefined()) {
      return mUndefined;
    } else if (cond.getValue() != 0) {
      return trueVal;
    } else {
      return falseVal;
    }
  }

  static MValue m_max(MValue x, MValue y) {
    return new MValue(Math.max(x.getValue(), y.getValue()));
  }

  static MValue m_min(MValue x, MValue y) {
    return new MValue(Math.min(x.getValue(), y.getValue()));
  }

  static MValue mNeg(MValue x) {
    if (x.isUndefined()) {
      return mUndefined;
    }
    return new MValue(-x.getValue());
  }

  static MValue mPresent(MValue value) {
    return value.isUndefined() ? zero : one;
  }

  static MValue mNot(MValue value) {
    if (value.isUndefined()) {
      return mUndefined;
    }
    return value.getValue() == 0 ? one : zero;
  }

  static MValue m_multimax(int bound, MValue[] array, int position) {
    MValue max = mAdd(array[position], zero);
    for (int i = 0; i <= bound; i++) {
      MValue challenger = mAdd(array[position + i], zero);
      if (challenger.getValue() > max.getValue()) {
        max = challenger;
      }
    }
    return max;
  }

  static boolean m_is_defined_true(MValue x) {
    if (x.isUndefined()) {
      return false;
    } else {
      return x.getValue() != 0;
    }
  }

  static boolean m_is_defined_false(MValue x) {
    if (x.isUndefined()) {
      return false;
    } else {
      return x.getValue() == 0;
    }
  }

  static MValue m_array_index(MValue[] array, int tableStart, int index, int size) {
    if (index < 0) {
      return zero;
    } else if (index >= size) {
      return mUndefined;
    } else {
      return array[tableStart + index];
    }
  }

  /**
   * Compare two MValues based on their value alone taking into account a
   * threshold of precision (e.g. 0.001). This threshold is useful as comparing
   * floats with the == operator can lead to false negatives, especially with
   * large numbers
   * 
   * @param toCompare     MValue to be compared with current MValue
   * @param withThreshold double representing threshold
   * @return boolean true if MValues are equal
   */
  public boolean equalsWithThreshold(MValue toCompare, double withThreshold) {
    return Math.abs(this.getValue() - toCompare.getValue()) < withThreshold;
  }

  /**
   * Compare two MValues based on their value and definedness, beware that
   * comparaison of float values is done with == operator
   * 
   * @param o Java Object to be compared with current MValue
   * @return boolean true if objects are equal
   */
  @Override
  public boolean equals(Object o) {
    return o != null && o instanceof MValue && ((MValue) o).getValue() == this.getValue()
        && ((MValue) o).isUndefined() == this.isUndefined();
  }

  @Override
  public String toString() {
    return "Value: " + this.getValue() + " undefined: " + this.isUndefined();
  }
}
