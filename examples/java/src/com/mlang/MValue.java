package com.mlang;

import java.util.OptionalDouble;
import java.lang.StackWalker.Option;
import java.util.List;
import java.util.function.BiFunction;

public class MValue {

  public static OptionalDouble mGreaterThan(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() > value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mGreaterThanEqual(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() >= value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mLessThan(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() < value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mLessThanEqual(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() <= value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mEqual(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() == value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mNotEqual(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      if (value1.getAsDouble() != value2.getAsDouble()) {
        return OptionalDouble.of(1.);
      } else {
        return OptionalDouble.of(0.);
      }
    }
  }

  public static OptionalDouble mAnd(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    } else if ((!value1.isEmpty() && value1.getAsDouble() != 0) && (!value2.isEmpty() && value2.getAsDouble() != 0)) {
      return OptionalDouble.of(1.);
    } else {
      return OptionalDouble.of(0.);
    }
  }

  public static OptionalDouble mOr(OptionalDouble value1, OptionalDouble value2) {
    if (value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    } else if ((!value1.isEmpty() && value1.getAsDouble() != 0) || (!value2.isEmpty() && value2.getAsDouble() != 0)) {
      return OptionalDouble.of(1.);
    } else {
      return OptionalDouble.of(0.);
    }
  }

  public static OptionalDouble mAdd(OptionalDouble value1, OptionalDouble value2) {

    if(value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    }

    double localValue1 = 0.;
    double localValue2 = 0.;

    if(!value1.isEmpty()) {
      localValue1 = value1.getAsDouble();
    }

    if(!value2.isEmpty()) {
      localValue2 = value2.getAsDouble();
    }
    
    return OptionalDouble.of(localValue1 + localValue2);
  }

  public static OptionalDouble mSubstract(OptionalDouble value1, OptionalDouble value2) {
    if(value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    }

    double localValue1 = 0.;
    double localValue2 = 0.;

    if(value1.isPresent()) {
      localValue1 = value1.getAsDouble();
    }

    if(value2.isPresent()) {
      localValue2 = value2.getAsDouble();
    }
    
    return OptionalDouble.of(localValue1 - localValue2);
  }

  public static OptionalDouble mMultiply(OptionalDouble value1, OptionalDouble value2) {
    if(value1.isEmpty() || value2.isEmpty()) {
      return OptionalDouble.empty();
    }
    return OptionalDouble.of(value1.getAsDouble() * value2.getAsDouble());
  }

  public static OptionalDouble mDivide(OptionalDouble value1, OptionalDouble value2) {
    if (firstOrSecond(value1, value2)) {
      return OptionalDouble.empty();
    }

    double denominateur = value2.getAsDouble();

    if (denominateur == 0) {
      return OptionalDouble.of(0);
    }

    return OptionalDouble.of(value1.getAsDouble() / denominateur);
  }

  public static OptionalDouble unopCondition(BiFunction<OptionalDouble, OptionalDouble, Boolean> condition,
      OptionalDouble value1, OptionalDouble value2) {
    if (firstOrSecond(value1, value2)) {
      return OptionalDouble.empty();
    }

    if (condition.apply(value1, value2)) {
      return OptionalDouble.of(1);
    } else {
      return OptionalDouble.of(0);
    }
  }

  public static OptionalDouble binopCondition(BiFunction<OptionalDouble, OptionalDouble, OptionalDouble> condition,
      OptionalDouble value1, OptionalDouble value2) {
    if (firstOrSecond(value1, value2)) {
      return OptionalDouble.empty();
    }

    return condition.apply(value1, value2);
  }

  public static boolean firstOrSecond(OptionalDouble value1, OptionalDouble value2) {
    return value1.isEmpty() || value2.isEmpty();
  }

  public static OptionalDouble m_round(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToRound = value.getAsDouble() + (value.getAsDouble() < 0 ? -0.50005 : 0.50005);
    return OptionalDouble.of(Math.floor(valueToRound));
  }

  public static OptionalDouble m_floor(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToFloor = value.getAsDouble() + 0.000001;
    return OptionalDouble.of(Math.floor(valueToFloor));
  }

  public static OptionalDouble m_cond(OptionalDouble cond, OptionalDouble trueVal, OptionalDouble falseVal) {
    if (!cond.isPresent()) {
      return cond;
    } else if (cond.getAsDouble() != 0) {
      return trueVal;
    } else {
      return falseVal;
    }
  }

  public static OptionalDouble m_max(OptionalDouble value1, OptionalDouble value2) {
    double localValue1 = 0.;
    double localValue2 = 0.;

    if (!value1.isEmpty()) {
      localValue1 = value1.getAsDouble();
    }

    if (!value2.isEmpty()) {
      localValue2 = value2.getAsDouble();
    }

    return OptionalDouble.of(Math.max(localValue1, localValue2));
  }

  public static OptionalDouble m_min(OptionalDouble value1, OptionalDouble value2) {
    double localValue1 = 0.;
    double localValue2 = 0.;

    if (!value1.isEmpty()) {
      localValue1 = value1.getAsDouble();
    }

    if (!value2.isEmpty()) {
      localValue2 = value2.getAsDouble();
    }
    
    return OptionalDouble.of(Math.min(localValue1, localValue2));
  }

  public static OptionalDouble mNeg(OptionalDouble value) {
    if (value.isEmpty()) {
      return value;
    }
    return OptionalDouble.of(-value.getAsDouble());
  }

  public static OptionalDouble mPresent(OptionalDouble value) {
    if (value.isEmpty()) {
      return OptionalDouble.of(0.);
    } else {
      return OptionalDouble.of(1.);
    }
  }

  public static OptionalDouble mNot(OptionalDouble value) {
    if (!value.isPresent()) {
      return OptionalDouble.of(0.);
    } else if (value.getAsDouble() == 0) {
      return OptionalDouble.of(1.);
    } else {
      return OptionalDouble.of(0.);
    }
  }

  public static OptionalDouble m_multimax(OptionalDouble bound, List<OptionalDouble> array) {
    if (!bound.isPresent()) {
      throw new RuntimeException("Multimax bound undefined!");
    } else {
      double max_index = Math.floor(bound.getAsDouble());
      OptionalDouble max = mAdd(array.get(0), OptionalDouble.of(0.));
      for (int i = 0; i <= max_index; i++) {
        OptionalDouble challenger = mAdd(array.get(i), OptionalDouble.of(0.));
        if (challenger.getAsDouble() > max.getAsDouble()) {
          max = challenger;
        }
      }
      return max;
    }
  }
}
