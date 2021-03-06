// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

module.exports = {
  testEnvironment: "node",
  testMatch: [
    "**/__tests__/**/*.+(ts|tsx|js)",
    "**/?(*.)+(spec|test).+(ts|tsx|js)"
  ],
  transform: {
    "^.+\\.(ts|tsx)$": "ts-jest",
    "^.+\\.(js|jsx)$": "babel-jest"
  },
  moduleNameMapper: {
    '^@daml/types$': '../daml-types',
    // $1 used for @daml/react/ledgerStore
    '^@daml/react(.*)$': '../daml-react$1'
  }
}

