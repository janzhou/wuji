package wuji

import collection.mutable.Stack
import org.scalatest._

class wujiTest extends FlatSpec with Matchers {
  "defobj and getobj" should "work" in {
    wuji.SDao.toString should be ("(dao)")
    wuji.parse("(0)").toString should be ("(0)")
    wuji.call("(0)").toString should be ("0")
    wuji.call("()").toString should be ("nil")
    wuji.call("(0 1)").toString should be ("1")
    wuji.call("(0 1 2)").toString should be ("2")
    wuji.call("(0 (1 2) 3)").toString should be ("3")
    wuji.call("(0 (1 2) (3 4) 5)").toString should be ("5")
    wuji.call("(defobj 1 a)").toString should be ("1")
    wuji.call("(defobj 1 a) (a)").toString should be ("1")
    wuji.call("(defobj 1 a) (a defobj 2 b) (a b)").toString should be ("2")
    wuji.call("(defobj (defobj 2 b) a) (b)").toString should be ("2")
    wuji.call("(letobj 1 a) (a)").toString should be ("1")
    wuji.call("(defobj 0 a) (letobj 1 a) (a)").toString should be ("1")
    wuji.call("(defobj 0 a) (letobj 1 a) (setobj 2 a) (a)").toString should be ("2")
    wuji.call("(defobj 0 a) (letobj 1 a) (undefobj a) (a)").toString should be ("1")
    wuji.call("(defobj 1 a) (undefobj a) (defobj 2 a) (a)").toString should be ("2")
  }

  it should "throw Exception for errors" in {
    a [wujiException] should be thrownBy {
      wuji.call("(0 (1 2( (3 4) 5)")
      wuji.call("(defobj 1 a) (undefobj a) (a)")
      wuji.call("(defobj 1 a) (a (defobj 2 b)) (b)")
      wuji.call("(defobj 1 a) (a defobj 2 b) (a undefobj b) (a b)")
      wuji.call("(letobj 0 a) (letobj 1 a)")
    }
  }

  "defun" should "work" in {
    wuji.call("(defun have-fun (name) (name)) (have-fun 1)").toString should be ("1")
    wuji.call("(defun have-fun (name) (name)) (have-fun 1)").toString should be ("1")
    wuji.call("(defun have-fun (name) ((name))) (have-fun 1)").toString should be ("1")
    wuji.call("(defun have-fun () ((defobj 1 a) (letobj 2 b))) (have-fun ()) (a)").toString should be ("1")
    wuji.call("(class)").toString should be ("(dao)")
    wuji.call("(defobj 1 a )(a class)").toString should be ("(dao) -> (int)")
  }

  "if" should "work" in {
    wuji.call("(if #true 1 2)").toString should be ("1")
    wuji.call("(if #false 1 2)").toString should be ("2")
    wuji.call("(if #true () 2)").toString should be ("nil")
    wuji.call("(if #false 1 ())").toString should be ("nil")
    wuji.call("(defun if-true (is-true) ((if is-true 1 2))) (if-true #true)").toString should be ("1")
    wuji.call("(defun if-true (is-true) ((if is-true 1 2))) (defun if-true-second (is-true) ((if-true is-true))) (if-true-second #true)").toString should be ("1")
  }

  "defclass" should "work" in {
    wuji.call("(class)").toString should be ("(dao)")
    wuji.call("(dao)").toString should be ("(dao)")
    wuji.call("(class ())").toString should be ("(dao)")
    wuji.call("(dao ())").toString should be ("(dao)")
    wuji.call("(int 1)").toString should be ("1")
    wuji.call("(int 1.1)").toString should be ("1")
    wuji.call("(string 1)").toString should be ("1")
    wuji.call("(string 1.1)").toString should be ("1.1")
    wuji.call("(float 1)").toString should be ("1.0")
    wuji.call("(float 1.1)").toString should be ("1.1")
    wuji.call("(string #true)").toString should be ("true")
    wuji.call("(defun obj () ((int 1.1))) (obj ())").toString should be ("1")
    wuji.call("(defclass new-class () (dao ()) ((defun echo (obj) ((obj))))) (defobj (new-class ()) new-obj) (new-obj 1)").toString should be ("1")
    wuji.call("(defclass new-class () (dao ()) ((defun echo (obj) ((obj))))) (defobj (new-class ()) new-obj) (new-obj class)").toString should be ("(dao) -> (new-class)")
  }

  "while" should "work for obj object" in {
    wuji.call("(defobj #true do) (while ((do)) ((setobj #false do) (do))) (do)").toString should be ("#false")
  }

  "while" should "work for env object" in {
    wuji.call("(letobj #true do) (while ((do)) ((setobj #false do) (do))) (do)").toString should be ("#false")
  }

  "while" should "not escap local env object" in {
    wuji.call("(letobj #true do) (while ((letobj #true do)(do)) ((setobj #false do) (do))) (do)").toString should be ("#true")
  }

  "ops" should "work" in {
    wuji.call("(1 add 2)").toString should be ("3")
    wuji.call("(1 add 2.1)").toString should be ("3.1")
    wuji.call("(1 sub 2.1)").toString should be ("-1.1")
    wuji.call("(#true and #true)").toString should be ("#true")
    wuji.call("(#true and #false)").toString should be ("#false")
    wuji.call("(#true or #false)").toString should be ("#true")
  }

  "this" should "work" in {
    wuji.call("(defobj (dao ()) d) (d)").toString should be ("(dao)")
    wuji.call("(defobj (dao ()) d) (d this)").toString should be ("(dao)")
    wuji.call("(defobj (dao ()) d) (d defobj 1 v) (d (v this))").toString should be ("(dao)")
  }
}