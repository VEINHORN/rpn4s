package com.github.veinhorn.rpn.repl

import com.github.veinhorn.rpn.ReversePolishNotation
import xsbti.Exit

/**
  * Created by VEINHORN on 09.04.2018.
  */
class ReplApp extends xsbti.AppMain {
  def run(configuration: xsbti.AppConfiguration): xsbti.MainResult = {
    // get the version of Scala used to launch the application
    val scalaVersion = configuration.provider.scalaProvider.version

    // Print a message and the arguments to the application
    println("Hello world! Running Scala " + scalaVersion)

    new Exit(0)
  }

  class Exit(val code: Int) extends xsbti.Exit
}
