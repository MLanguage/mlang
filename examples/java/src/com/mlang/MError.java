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

public class MError {

  private final String name;
  private final String kind;
  private final String majorCode;
  private final String minorCode;
  private final String description;
  private final String alias;

  MError(String name, String kind, String majorCode, String minorCode, String description, String alias) {
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
