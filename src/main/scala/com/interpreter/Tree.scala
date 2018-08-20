package com.interpreter

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Node(name: String, children: List[Tree]) extends Tree
