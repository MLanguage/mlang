public class MOperation {

  public static OptionalDouble mGreaterThan(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() > secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mGreaterThanEqual(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() >= secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mLessThan(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() < secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mLessThanEqual(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() <= secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mEqual(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() == secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mNotEqual(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return firstValue.getAsDouble() != secondValue.getAsDouble();
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mAnd(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return (
          firstValue.getAsDouble() != 0d && secondValue.getAsDouble() != 0d
        );
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mOr(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return unopCondition(
      (firstValue, secondValue) -> {
        return (
          firstValue.getAsDouble() != 0d || secondValue.getAsDouble() != 0d
        );
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mAdd(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return binopCondition(
      (firstValue, secondValue) -> {
        return OptionalDouble.of(
          firstValue.getAsDouble() + secondValue.getAsDouble()
        );
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mSubstract(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return binopCondition(
      (firstValue, secondValue) -> {
        return OptionalDouble.of(
          firstValue.getAsDouble() - secondValue.getAsDouble()
        );
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mMultiply(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return binopCondition(
      (firstValue, secondValue) -> {
        return OptionalDouble.of(
          firstValue.getAsDouble() * secondValue.getAsDouble()
        );
      },
      value1,
      value2
    );
  }

  public static OptionalDouble mDivide(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    double denominateur = value2.getAsDouble();

    if (denominateur == 0) {
      return OptionalDouble.of(0);
    }

    return OptionalDouble.of(value1.getAsDouble() / denominateur);
  }

  public static OptionalDouble unopCondition(
    BiFunction<OptionalDouble, OptionalDouble, Boolean> condition,
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    if (condition.apply(value1, value2)) {
      return OptionalDouble.of(1);
    } else {
      return OptionalDouble.of(0);
    }
  }

  public static OptionalDouble binopCondition(
    BiFunction<OptionalDouble, OptionalDouble, OptionalDouble> condition,
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    if (valuesNotPresent(value1, value2)) {
      return OptionalDouble.empty();
    }

    return condition.apply(value1, value2);
  }

  public static boolean valuesNotPresent(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    return value1.isEmpty() || value2.isEmpty();
  }

  public static OptionalDouble m_round(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToRound = value.getAsDouble() + value.getAsDouble() < 0
      ? -0.50005
      : 0.50005;
    return OptionalDouble.of(Math.round(valueToRound));
  }

  public static OptionalDouble m_floor(OptionalDouble value) {
    if (!value.isPresent()) {
      return value;
    }
    double valueToFloor = value.getAsDouble() + 0.000001;
    return OptionalDouble.of(Math.floor(valueToFloor));
  }

  public static OptionalDouble m_cond(
    OptionalDouble value,
    OptionalDouble value2,
    OptionalDouble value3
  ) {
    if (!value.isPresent()) {
      return value;
    } else if (value.getAsDouble() != 0) {
      return value2;
    } else {
      return value3;
    }
  }

  public static OptionalDouble m_max(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    if (value1.isEmpty() && value2.isPresent()) {
      return value2;
    } else if (value1.isPresent() && value2.isEmpty()) {
      return value1;
    } else if (value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      return OptionalDouble.of(
        Math.max(value1.getAsDouble(), value2.getAsDouble())
      );
    }
  }

  public static OptionalDouble m_min(
    OptionalDouble value1,
    OptionalDouble value2
  ) {
    if (value1.isEmpty() && value2.isPresent()) {
      return value2;
    } else if (value1.isPresent() && value2.isEmpty()) {
      return value1;
    } else if (value1.isEmpty() && value2.isEmpty()) {
      return OptionalDouble.empty();
    } else {
      return OptionalDouble.of(
        Math.min(value1.getAsDouble(), value2.getAsDouble())
      );
    }
  }

  public static OptionalDouble mNeg(OptionalDouble value) {
    if (value.isEmpty()) {
      return value;
    }
    return OptionalDouble.of(-value.getAsDouble());
  }

  public static OptionalDouble mPresent(OptionalDouble value) {
    if (!value.isPresent()) {
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

  public static OptionalDouble m_multimax(
    OptionalDouble bound,
    List<OptionalDouble> array
  ) {
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
