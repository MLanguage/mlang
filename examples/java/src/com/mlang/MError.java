package com.mlang;

public class MError {

  private final String name;
  private final String kind;
  private final String majorCode;
  private final String minorCode;
  private final String description;
  private final String alias;

  public MError(String name, String kind, String majorCode, String minorCode, String description, String alias) {
    this.name = name;
    this.kind = kind;
    this.majorCode = majorCode;
    this.minorCode = minorCode;
    this.description = description;
    this.alias = alias;
  }

  public String getName() {
    return name;
  }

  public String getKind() {
    return kind;
  }

  public String getMajorCode() {
    return majorCode;
  }

  public String getMinorCode() {
    return minorCode;
  }

  public String getDescription() {
    return description;
  }

  public String getAlias() {
    return alias;
  }

}
