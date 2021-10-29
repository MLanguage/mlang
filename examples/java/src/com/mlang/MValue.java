package com.mlang;

import java.util.List;
import java.util.function.BiFunction;

public class MValue {


  public static final MValue mUndefined = new MValue(0., true);
  public static final MValue zero = new MValue(0., false);
  public static final MValue one = new MValue(1., false);

  private final double value;
  private final boolean undefined;

   public MValue(double value, boolean isDefined){
     this.value = value;
     this.undefined = isDefined;
  }

public MValue(double value){
     this.value = value;
     this.undefined = false;
  }

  public double getValue() {
    return this.value;
  } 

  public boolean isUndefined() {
    return this.undefined;
  }

  private static MValue boolToMValue(boolean b){
   return b ? one : zero;
  }

  public static MValue mGreaterThan(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() > y.getValue()); 
  }

  public static MValue mGreaterThanEqual(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
   return boolToMValue(x.getValue() >= y.getValue()); 
  }

  public static MValue mLessThan(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() < y.getValue());
  }

  public static MValue mLessThanEqual(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() <= y.getValue());
  }

  public static MValue mEqual(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() == y.getValue());
  }

  public static MValue mNotEqual(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() != y.getValue());
  }

  public static MValue mAnd(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() != 0 && y.getValue() != 0);
  }

  public static MValue mOr(MValue x, MValue y) {
    if(x.isUndefined() && y.isUndefined())  {
      return mUndefined;
    } 
    return boolToMValue(x.getValue() != 0 || y.getValue() != 0);
  }

  public static MValue mAdd(MValue x, MValue y) {

    if(x.isUndefined() && y.isUndefined())  {
      return mUndefined;
    } 
    
    return new MValue(x.getValue() + y.getValue());
  }

  public static MValue mSubtract(MValue x, MValue y) {
    if(x.isUndefined() && y.isUndefined())  {
      return mUndefined;
    } 

    return new MValue(x.getValue() - y.getValue());
  }

  public static MValue mMultiply(MValue x, MValue y) {
    if(x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }
    return new MValue(x.getValue() * y.getValue());
  }

  public static MValue mDivide(MValue x, MValue y) {
   
    if(x.isUndefined() || y.isUndefined()) {
      return mUndefined;
    }

    double denominateur = y.getValue();

    if (denominateur == 0) {
      return zero;
    }

    return new MValue(x.getValue() / denominateur);
  }


  public static MValue m_round(MValue x) {
    if (x.isUndefined()) {
      return mUndefined;
    }
    double dValue = x.getValue();
    double valueToRound = dValue + (dValue < 0 ? -0.50005 : 0.50005);
    return new MValue(Math.floor(valueToRound));
  }

  public static MValue m_floor(MValue x) {
    if (x.isUndefined()) {
      return mUndefined;
    }
    double valueToFloor = x.getValue()+ 0.000001;
    return new MValue(Math.floor(valueToFloor));
  }

  public static MValue m_cond(MValue cond, MValue trueVal, MValue falseVal) {
    if (cond.isUndefined()) {
      return mUndefined;
    } else if (cond.getValue() != 0) {
      return trueVal;
    } else {
      return falseVal;
    }
  }

  public static MValue m_max(MValue x, MValue y) {
    return new MValue(Math.max(x.getValue(), y.getValue()));
  }

  public static MValue m_min(MValue x, MValue y) { 
    return new MValue(Math.min(x.getValue(), y.getValue()));
  }

  public static MValue mNeg(MValue x) {
    if(x.isUndefined()) {
      return mUndefined;
    }
    return new MValue(-x.getValue());
  }

  public static MValue mPresent(MValue value) {
   return value.isUndefined() ? zero : one;
  }

  public static MValue mNot(MValue value) {
    if(value.isUndefined()) {
      return mUndefined;
    }
    return value.getValue() == 0 ? one : zero;
  }

  public static MValue m_multimax(MValue bound, List<MValue> array) {
    if (bound.isUndefined()) {
      throw new RuntimeException("Multimax bound undefined!");
    } else {
      int max_index = (int)Math.floor(bound.getValue());
      MValue max = mAdd(m_array_index(array, zero), zero);
      for (int i = 0; i <= max_index; i++) {
        MValue challenger = mAdd(m_array_index(array, new MValue(i)), zero);
        if (challenger.getValue() > max.getValue()) {
          max = challenger;
        }
      }
      return max;
    }
  }

  public static boolean m_is_defined_true(MValue x) {
    if (x.isUndefined()) {
      return false;
    } else {
      return x.getValue() != 0;
    }
  }

  public static boolean m_is_defined_false(MValue x) {
    if (x.isUndefined()) {
      return false;
    } else {
      return x.getValue() == 0;
    }
  }

  public static MValue m_array_index(List<MValue> array, MValue index)
{
    if (index.isUndefined())
    {
        return mUndefined;
    }
    else
    {
        if (index.getValue() < 0)
        {
            return zero;
        }
        else if (index.getValue() >= array.size() - 1)
        {
            return mUndefined;
        }
        else
        {
            return array.get((int)(index.getValue()));
        }
    }
}

  @Override
  public boolean equals(Object o) {
    return o != null && o instanceof MValue && ((MValue)o).getValue() == this.getValue() && ((MValue)o).isUndefined() == this.isUndefined();
  }

  @Override
  public String toString() {
    return "Value: " + this.getValue() + " undefined: " + this.isUndefined(); 
  }
}
