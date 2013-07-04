package macaroon

import language.experimental.macros

import com.codecommit.antixml

package object xml {

  def anti(ns:scala.xml.NodeSeq):antixml.Group[antixml.Node] = macro Anti.anti_nodeseq

  def anti(elem:scala.xml.Elem):antixml.Elem = macro Anti.anti_elem
}
