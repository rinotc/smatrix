package com.github.rinotc.matrix

import org.scalatest.{EitherValues, Inside, OptionValues}
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait BaseTest extends AnyFunSpec with OptionValues with EitherValues with Inside with Diagrams with Matchers
